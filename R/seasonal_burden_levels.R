#' Compute burden levels from seasonal time series observations.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function calculates the burden levels of time series of observations that are stratified by season.
#' It uses the previous seasons to calculate the levels of the newest season.
#'
#' @param tsd A `aedseo_tsd` object containing time series data with 'time' and 'observed'.
#' @param season_weeks A numeric vector of length 2, `c(start, end)`, with the start and end weeks of the seasons to
#' stratify the observations by. Must span the new year; e.g.: `season_weeks = c(21, 20)`.
#' NOTE: The data must include data for a complete previous season to make predictions for the newest season.
#' @param method A character string specifying the model to be used in the level calculations.
#' Choose between "intensity_levels" or "peak_levels". Both model predict the levels of the newest series of
#' observations.
#'  - `intensity_levels`: models the risk compared to what has been observed in previous seasons.
#'  - `peak_levels`: models the risk compared to what has been observed in the `n_peak` observations each season.
#' @param conf_levels A numeric vector specifying the confidence levels for parameter estimates. The values have
#' to be unique and in ascending order, (i.e. the lowest level is first and highest level is last).
#' The `conf_levels` are specific for each method:
#'   - for `intensity_levels` only specify the highest confidence level e.g.: `0.95`, which is the highest intensity
#'     that has been observed in previous seasons.
#'   - for `peak_levels` specify three confidence levels e.g.: `c(0.5, 0.9, 0.95)`, which are the three confidence
#'     levels low, medium and high that reflect the peak severity relative to those observed in previous seasons.
#' @param decay_factor A numeric value between 0 and 1, that specifies the weight applied to previous seasons in
#' calculations. It is used as `decay_factor`^(number of seasons back), whereby the weight for the most recent season
#' will be `decay_factor`^0 = 1. This parameter allows for a decreasing weight assigned to prior seasons, such that
#' the influence of older seasons diminishes exponentially.
#' @param disease_threshold An integer specifying the threshold for considering a disease outbreak. It defines the per
#' time-step disease threshold that has to be surpassed for the observation to be included in the calculations.
#' @param n_peak A numeric value specifying the number of peak observations to be selected from each season in the
#' level calculations. The `n_peak` observations have to surpass the `disease_threshold` to be included.
#' @param ... arguments that can be passed to the `fit_quantiles()` function.
#'
#' @return A list containing:
#'   - 'season': The season that burden levels are calculated for.
#'   - 'high_conf_level': (only for intensity_level method) The conf_level chosen for the high level.
#'   - 'conf_levels': (only for peak_level method) The conf_levels chosen to fit the "low", "medium", "high" levels.
#'   - 'values': A named vector with values for "very low", "low", "medium", "high" levels.
#'   - 'par': The fit parameters for the chosen family.
#'       - par_1:
#'          - For 'weibull': Shape parameter (k).
#'          - For 'lnorm': Mean of the log-transformed observations.
#'          - For 'exp': Rate parameter (λ).
#'       - 'par_2':
#'          - For 'weibull': Scale parameter (λ).
#'          - For 'lnorm': Standard deviation of the log-transformed observations.
#'          - For 'exp': Not applicable (set to NA).
#'   - 'obj_value': The value of the objective function - (negative log-likelihood), which represent the minimized
#'                  objective function value from the optimisation. Smaller value equals better optimisation.
#'   - 'converged': Logical. TRUE if the optimisation converged.
#'   - 'family': The distribution family used for the optimization.
#'      - 'weibull': Uses the Weibull distribution for fitting.
#'      - 'lnorm': Uses the Log-normal distribution for fitting.
#'      - 'exp': Uses the Exponential distribution for fitting.
#'      - 'disease_threshold': The input disease threshold, which is also the very low level.
#'
#' @export
#'
#' @examples
#' # Generate random flu season
#' generate_flu_season <- function(start = 1, end = 1000) {
#'   random_increasing_obs <- round(sort(runif(24, min = start, max = end)))
#'   random_decreasing_obs <- round(rev(random_increasing_obs))
#'
#'   # Generate peak numbers
#'   add_to_max <- c(50, 100, 200, 100)
#'   peak <- add_to_max + max(random_increasing_obs)
#'
#'   # Combine into a single observed sequence
#'   observed <- c(random_increasing_obs, peak, random_decreasing_obs)
#'
#'  return(observed)
#' }
#'
#' season_1 <- generate_flu_season()
#' season_2 <- generate_flu_season()
#'
#' start_date <- as.Date("2022-05-29")
#' end_date <- as.Date("2024-05-20")
#'
#' weekly_dates <- seq.Date(from = start_date,
#'                          to = end_date,
#'                          by = "week")
#'
#' tsd_data <- tsd(
#'   observed = c(season_1, season_2),
#'   time = as.Date(weekly_dates),
#'   time_interval = "week"
#' )
#'
#' # Print seasonal burden results
#' seasonal_burden_levels(tsd_data)
seasonal_burden_levels <- function(
  tsd,
  season_weeks = c(21, 20),
  method = c("intensity_levels", "peak_levels"),
  conf_levels = 0.95,
  decay_factor = 0.8,
  disease_threshold = 20,
  n_peak = 6,
  ...
) {
  # Check input arguments
  method <- rlang::arg_match(method)
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(tsd, add = coll)
  checkmate::assert_class(tsd, "aedseo_tsd", add = coll)
  checkmate::assert_names(colnames(tsd), identical.to = c("time", "observed"), add = coll)
  checkmate::assert_integerish(season_weeks, len = 2, lower = 1, upper = 53,
                               null.ok = FALSE, add = coll)
  checkmate::assert_numeric(decay_factor, lower = 0, upper = 1, len = 1, add = coll)
  checkmate::assert_numeric(n_peak, lower = 1, len = 1, add = coll)
  checkmate::assert_integerish(disease_threshold, len = 1, add = coll)
  # Assert conf_levels based on the method chosen
  if (method == "intensity_levels") {
    checkmate::assert_numeric(conf_levels, lower = 0, upper = 1, len = 1,
                              unique = TRUE, sorted = TRUE, add = coll)
  } else if (method == "peak_levels") {
    checkmate::assert_numeric(conf_levels, lower = 0, upper = 1, len = 3,
                              unique = TRUE, sorted = TRUE, add = coll)
  }
  # Add the seasons to data
  seasonal_tsd <- tsd |>
    dplyr::mutate(season = epi_calendar(.data$time, start = season_weeks[1], end = season_weeks[2])) |>
    dplyr::arrange(dplyr::desc(.data$season))

  # Check that there is at least two seasons of data
  if (length(unique(seasonal_tsd$season)) <= 1) coll$push("There must be at least two unique seasons in the data.")
  checkmate::reportAssertions(coll)

  # Add weights and remove newest season to get predictions for this season
  weighted_seasonal_tsd <- seasonal_tsd |>
    dplyr::filter(.data$season != max(.data$season)) |>
    dplyr::mutate(year = purrr::map_chr(.data$season, ~ stringr::str_extract(.x, "[0-9]+")) |>
                    as.numeric()) |>
    dplyr::mutate(weight = decay_factor^(max(.data$year) - .data$year))

  # Select n_peak highest observations and filter observations >= disease_threshold
  season_observations_and_weights <- weighted_seasonal_tsd |>
    dplyr::select(-c("year", "time")) |>
    dplyr::filter(.data$observed >= disease_threshold) |>
    dplyr::slice_max(.data$observed, n = n_peak, with_ties = FALSE, by = "season")

  # Run quantiles_fit function
  quantiles_fit <- season_observations_and_weights |>
    dplyr::select("observed", "weight") |>
    fit_quantiles(weighted_observations = _, conf_levels = conf_levels, ...)

  # If method intensity_levels was chosen; use the high level from the `fit_quantiles` function as the high
  # level and the disease_threshold as the very low level. The low and medium levels are defined as the relative
  # increase between the very low level and high level.
  results <- switch(method,
    peak_levels = {
      model_output <- append(quantiles_fit, list(season = max(seasonal_tsd$season)), after = 0)
      model_output$values <- stats::setNames(c(disease_threshold, model_output$values),
                                              c("very low", "low", "medium", "high"))
      model_output <- append(model_output, list(disease_threshold = disease_threshold))
    },
    intensity_levels = {
      level_step_log <- pracma::logseq(disease_threshold, quantiles_fit$values, n = 4)
      model_output <- list(
        season = max(seasonal_tsd$season),
        high_conf_level = quantiles_fit$conf_levels,
        values = stats::setNames(level_step_log, c("very low", "low", "medium", "high")),
        par = quantiles_fit$par,
        obj_value = quantiles_fit$conf_levels,
        converged = quantiles_fit$converged,
        family = quantiles_fit$family,
        disease_threshold = disease_threshold
      )
    }
  )
  return(results)
}
