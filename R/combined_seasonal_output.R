#' Compute seasonal onset and burden levels from seasonal time series observations.
#'
#' @description
#'
#' This function performs automated and early detection of seasonal epidemic onsets and calculates the burden
#' levels from time series dataset stratified by season. The seasonal onset estimates growth rates for consecutive
#' time intervals and calculates the sum of cases. The burden levels use the previous seasons to calculate the levels
#' of the current season.
#' @inheritParams seasonal_burden_levels
#' @inheritParams seasonal_onset
#' @param tsd `r rd_tsd()`
#' @param disease_threshold `r rd_disease_threshold(usage = "combined")`
#' @param family `r rd_family(usage = "combined")`
#' @param family_quant A character string specifying the family for modeling burden levels.
#' @param season_weeks `r rd_season_weeks()`
#' @param ... arguments that can be passed to the `fit_quantiles()` function in the burden level calculations.
#'
#' @return An object containing two lists: onset_output and burden_output:
#'
#' onset_output:
#'   - 'reference_time': The time point for which the growth rate is estimated.
#'   - 'observation': The observation in the reference time point.
#'   - 'season': The stratification of observables in corresponding seasons.
#'   - 'growth_rate': The estimated growth rate.
#'   - 'lower_growth_rate': The lower bound of the growth rate's confidence interval.
#'   - 'upper_growth_rate': The upper bound of the growth rate's confidence interval.
#'   - 'growth_warning': Logical. Is the growth rate significantly higher than zero?
#'   - 'sum_of_cases': The sum of cases within the time window.
#'   - 'sum_of_cases_warning': Logical. Does the Sum of Cases exceed the disease threshold?
#'   - 'seasonal_onset_alarm': Logical. Is there a seasonal onset alarm?
#'   - 'skipped_window': Logical. Was the window skipped due to missing?
#'   - 'converged': Logical. Was the IWLS judged to have converged?
#'
#' burden_output:
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
#'   - 'family_quant': The distribution family used for the optimization.
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
#'   # Combine into a single observations sequence
#'   observations <- c(random_increasing_obs, peak, random_decreasing_obs)
#'
#'  return(observations)
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
#'   observation = c(season_1, season_2),
#'   time = as.Date(weekly_dates),
#'   time_interval = "week"
#' )
#'
#' # Run the main function
#' combined_data <- combined_seasonal_output(tsd_data)
#' # Print seasonal onset results
#' print(combined_data$onset_output)
#' # Print burden level results
#' print(combined_data$burden_output)
combined_seasonal_output <- function(
  tsd,
  k = 5,
  level = 0.95,
  disease_threshold = 20,
  family = c(
    "poisson",
    "quasipoisson"
  ),
  family_quant = c(
    "weibull",
    "lnorm",
    "exp"
  ),
  na_fraction_allowed = 0.4,
  season_weeks = c(21, 20),
  method = c("intensity_levels", "peak_levels"),
  conf_levels = 0.95,
  decay_factor = 0.8,
  n_peak = 6,
  only_current_season = TRUE,
  ...
) {
  # Run the models
  burden_output <- seasonal_burden_levels(tsd = tsd, season_weeks = season_weeks, method = method,
                                          conf_levels = conf_levels, decay_factor = decay_factor,
                                          disease_threshold = disease_threshold, n_peak = n_peak,
                                          family = family_quant, only_current_season, ...)

  onset_output <- seasonal_onset(tsd = tsd, k = k, level = level, disease_threshold = disease_threshold,
                                 family = family, na_fraction_allowed = na_fraction_allowed,
                                 season_weeks = season_weeks, only_current_season)

  # Extract current season from onset_output and create seasonal_onset
  onset_output <- onset_output |>
    dplyr::group_by(.data$season) |>
    dplyr::mutate(onset_flag = cumsum(.data$seasonal_onset_alarm),
                  seasonal_onset = .data$onset_flag == 1 & !duplicated(.data$onset_flag)) |>
    dplyr::select(-(.data$onset_flag))

  if (only_current_season == TRUE) {
    onset_output <- onset_output |>
      dplyr::filter(.data$season == max(.data$season))
  }

  return(list(onset_output = onset_output, burden_output = burden_output))
}
