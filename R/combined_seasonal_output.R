#' Compute seasonal onset and burden levels from seasonal time series observations.
#'
#' @description
#'
#' This function performs automated and early detection of seasonal epidemic onsets and calculates the burden
#' levels of time series dataset stratified by season. The seasonal onset estimates growth rates for consecutive
#' time intervals and calculates the sum of cases (sum_of_cases). The burden levels use the previous seasons to
#' calculate the levels of the newest season.
#'
#' @param tsd An object containing time series data with 'time' and 'observation.'
#' @param k An integer specifying the window size for modeling growth rates.
#' @param level The confidence level for parameter estimates, a numeric value between 0 and 1.
#' @param disease_threshold An integer specifying the threshold for considering a disease outbreak. It defines
#' the per time-step disease threshold that has to be surpassed to possibly trigger a seasonal onset alarm.
#' If the total number of cases in a window of size k exceeds `disease_threshold * k`, a seasonal onset alarm
#' can be triggered. The burden levels are only considered if the surpass the disease_threshold.
#' @param family A character string specifying the family for modeling seasonal onset.
#' Choose between "poisson," or "quasipoisson".
#' @param na_fraction_allowed Numeric value between 0 and 1 specifying the fraction of observables in the window
#' of size k that are allowed to be NA.
#' @param season_weeks A numeric vector of length 2, `c(start, end)`, with the start and end weeks of the seasons to
#' stratify the observations by. Must span the new year; e.g.: `season_weeks = c(21, 20)`.
#' @param method A character string specifying the model to be used in the burden level calculations.
#' Choose between "intensity_levels" or "peak_levels". Both model predict the levels of the newest series of
#' observations.
#'  - `intensity_levels`: models the risk compared to what has been observed in previous seasons.
#'  - `peak_levels`: models the risk compared to what has been observed in the `n_peak` observations each season.
#' @param conf_levels A numeric vector specifying the confidence levels for parameter estimates. The values have
#' to be unique and in ascending order, (i.e. the lowest level is first and highest level is last).
#' The `conf_levels` are specific for each method in the burden level calculations:
#'   - for `intensity_levels` only specify the highest confidence level e.g.: `0.95`, which is the highest intensity
#'     that has been observed in previous seasons.
#'   - for `peak_levels` specify three confidence levels e.g.: `c(0.5, 0.9, 0.95)`, which are the three confidence
#'     levels low, medium and high that reflect the peak severity relative to those observed in previous seasons.
#' @param decay_factor A numeric value between 0 and 1, that specifies the weight applied to previous seasons in
#' calculations. It is used as `decay_factor`^(number of seasons back), whereby the weight for the most recent season
#' will be `decay_factor`^0 = 1. This parameter allows for a decreasing weight assigned to prior seasons, such that
#' the influence of older seasons diminishes exponentially.
#' @param n_peak A numeric value specifying the number of peak observations to be selected from each season in the
#' level calculations. The `n_peak` observations have to surpass the `disease_threshold` to be included.
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
  na_fraction_allowed = 0.4,
  season_weeks = c(21, 20),
  method = c("intensity_levels", "peak_levels"),
  conf_levels = 0.95,
  decay_factor = 0.8,
  n_peak = 6,
  ...
) {
  # Run the models
  burden_output <- seasonal_burden_levels(tsd = tsd, season_weeks = season_weeks, method = method,
                                          conf_levels = conf_levels, decay_factor = decay_factor,
                                          disease_threshold = disease_threshold, n_peak = n_peak, ...)

  onset_output <- seasonal_onset(tsd = tsd, k = k, level = level, disease_threshold = disease_threshold,
                                 family = family, na_fraction_allowed = na_fraction_allowed,
                                 season_weeks = season_weeks)

  # Extract newest season from onset_output and create seasonal_onset
  onset_output <- onset_output |>
    dplyr::filter(.data$season == max(.data$season)) |>
    dplyr::mutate(onset_flag = cumsum(.data$seasonal_onset_alarm),
                  seasonal_onset = .data$onset_flag == 1 & !duplicated(.data$onset_flag)) |>
    dplyr::select(-(.data$onset_flag))

  return(list(onset_output = onset_output, burden_output = burden_output))
}
