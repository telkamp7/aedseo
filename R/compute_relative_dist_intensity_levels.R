#' Compute intensity levels with weighted time series observations (relative dist model).
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function calculates the intensity levels of weighted time series observations. The output contains very low,
#' low, medium, high intensity levels, that predict the intensity levels of the next series of observations.
#' It uses the highest level from the `compute_weighted_intensity_levels` function as the high intensity level and
#' the disease_threshold as the very low intensity level.
#'
#' @param weighted_observations A tibble containing two columns of length n; `observed`, which contains the data
#' points, and `weight`, which is the importance assigned to the observation. Higher weights indicate that an
#' observation has more influence on the model outcome, while lower weights reduce its impact.
#' @param disease_threshold An integer specifying the threshold for considering a disease outbreak. It defines the per
#' time-step disease threshold that has to be surpassed for the observation to be included in the calculations.
#' @param ... arguments that can be passed to the `compute_weighted_intensity_levels` function.
#'
#' @return A tibble containing:
#'   - 'very_low_level' The very low intensity level.
#'   - 'low_level': The low intensity level.
#'   - 'medium_level': The medium intensity level.
#'   - 'high_level': The high intensity level.
#'   - 'relative_dist': The relative distance between levels.
#'   - 'high_conf_level': The conf level for the high intensity level.
#'   - 'optim_fit_par_1': The first fit parameter for the chosen family.
#'       - For 'weibull': Shape parameter (k).
#'       - For 'lnorm': Mean of the log-transformed observations.
#'       - For 'exp': Rate parameter (λ).
#'   - 'optim_fit_par_2': The second fit parameter for the chosen family.
#'      - For 'weibull': Scale parameter (λ).
#'      - For 'lnorm': Standard deviation of the log-transformed observations.
#'      - For 'exp': Not applicable (set to NA).
#'   - 'obj_value': The value of the objective function.
#'     (negative log-likelihood), which represent the minimized objective
#'      function value from the optimisation. Smaller value equals better
#'      optimisation.
#'   - 'converged': Logical. TRUE if the optimisation converged.
#'   - 'family': The distribution family used for the optimization.
#'      - 'weibull': Uses the Weibull distribution for fitting.
#'      - 'lnorm': Uses the Log-normal distribution for fitting.
#'      - 'exp': Uses the Exponential distribution for fitting.
#'
#'
compute_relative_dist_intensity_levels <- function(
  weighted_observations,
  disease_threshold = 20,
  ...
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(weighted_observations, add = coll)
  checkmate::assert_names(colnames(weighted_observations),
                          identical.to = c("observed", "weight"), add = coll)
  checkmate::assert_integerish(disease_threshold, len = 1, add = coll)
  checkmate::reportAssertions(coll)

  # Run `compute_weighted_intensity_levels` function on data
  weighted_intensity_levels <- compute_weighted_intensity_levels(weighted_observations = weighted_observations, ...)

  # Log transform levels
  high_level_log <- log(weighted_intensity_levels$high_level)
  very_low_level_log <- log(disease_threshold)

  # Relative distance between very low and high level
  relative_dist <- (high_level_log - very_low_level_log) / 3

  # Calculate low and medium levels
  medium_level <- exp(high_level_log - relative_dist)
  low_level <- exp(very_low_level_log + relative_dist)

  # Combine output results
  relative_dist_results <- weighted_intensity_levels |>
    dplyr::select(-c("low_level", "medium_level")) |>
    dplyr::mutate(medium_level = medium_level,
                  low_level = low_level,
                  very_low_level = disease_threshold,
                  relative_dist = relative_dist)

  print(relative_dist_results)
}