#' Compute burden levels with seasonal time series observations.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function calculates the burden levels of time series of observations that are stratified by season.
#' It uses the previous seasons to calculate the levels of the newest season.
#'
#' @param tsd A `aedseo_tsd` object containing time series data with 'time' and 'observed'.
#' @param method A character string specifying the model to be used in the level calculations.
#' Choose between intensity_levels or peak_levels. Both model predict the levels of the next series of observations.
#'  - `intensity_levels`: models the risk compared to what has been observed in previous years.
#'  - `peak_levels`: models the risk compared to what has been observed in the `n_peak` observations each season.
#' @param conf_levels
#' @param decay_factor A numeric value between 0 and 1, that specifies the weight applied to previous seasons in
#' calculations. It is used as `decay_factor`^(number of seasons back), whereby the weight for the most recent season
#' will be `decay_factor`^0 = 1. This parameter allows for a decreasing weight assigned to prior seasons, such that
#' the influence of older seasons diminishes exponentially.
#' @param disease_threshold An integer specifying the threshold for considering a disease outbreak. It defines the per
#' time-step disease threshold that has to be surpassed for the observation to be included in the calculations.
#' @param n_peak A numeric value specifying the number of peak observations to be selected from each season in the
#' level calculations. The `n_peak` observations have to surpass the `disease_threshold` to be included.
#' @param season_weeks A numeric vector of length 2, `c(start, end)`, with the start and end weeks of the seasons to
#' stratify the observations by. Must span the new year; e.g.: `season_weeks = c(21, 20)`.
#' @param ... arguments that can be passed to the `compute_weighted_intensity_levels()` function.
#'
#' @return A tibble containing:
#'   - 'very_low_level': The very low burden level
#'   - 'low_level': The low burden level
#'   - 'medium_level': The medium burden level
#'   - 'high_level': The high burden level
#'   - 'optim_fit_par_1': The first fit parameter for the chosen family.
#'       - For 'weibull': Shape parameter (k).
#'       - For 'lnorm': Mean of the log-transformed observations.
#'       - For 'exp': Rate parameter (λ).
#'   - 'optim_fit_par_2': The second fit parameter for the chosen family.
#'      - For 'weibull': Scale parameter (λ).
#'      - For 'lnorm': Standard deviation of the log-transformed observations.
#'      - For 'exp': Not applicable (set to NA).
#'   - 'obj_value': The value of the objective function
#'     (negative log-likelihood), which represent the minimized objective
#'      function value from the optimisation. Smaller value equals better
#'      optimisation.
#'   - 'converged': Logical. TRUE if the optimisation converged.
#'   - 'family': The distribution family used for the optimization.
#'      - 'weibull': Uses the Weibull distribution for fitting.
#'      - 'lnorm': Uses the Log-normal distribution for fitting.
#'      - 'exp': Uses the Exponential distribution for fitting.

intensity_levels <- function(
  tsd,
  method = c("intensity_levels", "peak_levels"),
  conf_levels = 0.95,
  decay_factor = 0.8,
  disease_threshold = 20,
  n_peak = 6,
  season_weeks = c(21, 20),
  ...
) {
  # Check input arguments
  method <- rlang::arg_match(method)
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(tsd, add = coll)
  checkmate::assert_class(tsd, "aedseo_tsd", add = coll)
  checkmate::assert_names(colnames(tsd), identical.to = c("time", "observed"), add = coll)
  checkmate::assert_numeric(decay_factor, lower = 0, upper = 1, len = 1, add = coll)
  checkmate::assert_numeric(n_peak, lower = 1, len = 1, add = coll)
  checkmate::assert_integerish(disease_threshold, len = 1, add = coll)
  checkmate::assert_integerish(season_weeks, len = 2, lower = 1, upper = 53,
                               null.ok = FALSE, add = coll)
  # Assert conf_levels based on the method chosen
  if (method == "intensity_levels") {
    checkmate::assert_numeric(conf_levels, lower = 0, upper = 1, len = 1,
                              unique = TRUE, sorted = TRUE, add = coll)
  } else if (method == "peak_levels") {
    checkmate::assert_numeric(conf_levels, lower = 0, upper = 1, len = 3,
                              unique = TRUE, sorted = TRUE, add = coll)
  }
  checkmate::reportAssertions(coll)

  # Add the seasons to data
  seasonal_tsd <- tsd |>
    dplyr::mutate(season = epi_calendar(.data$time, start = season_weeks[1], end = season_weeks[2])) |>
    dplyr::arrange(dplyr::desc(.data$season))

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

  # Run with chosen method
  burden_levels <- switch(method,
    peak_levels = fit_quantiles(weighted_observations = season_observations_and_weights |>
                                  dplyr::select(.data$observed, .data$weight), ...)
  )

  # Select the newest season for intensity level output
  #season_peak_levels <- computed_peak_levels |>
  #  dplyr::mutate(season = max(seasonal_tsd$season)) |>
  #  dplyr::select(.data$season, dplyr::everything())

  return(burden_levels)
}
