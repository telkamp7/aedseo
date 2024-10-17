#' Fit a peak algorithm to time series observations.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function calculates the intensity levels of time series
#' observations to each available season based on previouse seasons.
#' It provides low, medium, high intensity levels.
#'
#' @param n_observations A numeric vector of size k containing the
#' time series observations.
#' @param conf_levels The confidence levels for parameter estimates, a numeric
#' vector of length 3. Default is c(0.95, 0.90, 0.50).
#' @param family A character string specifying the family for
#' modeling. Choose between "weibull", "lnorm" or "exp".

#'
#' @return A list containing:
#'   - 'season':
#'   - 'high_level':
#'   - 'medium_level':
#'   - 'low_level':
#'   - 'obj_value':
#'   - 'family':

fit_peak <- function(
  n_observations,
  conf_levels = c(0.95, 0.90, 0.5),
  family = c("weibull",
             "lnorm",
             "qexp")
) {
  # Match the arguements
  family <- rlang::arg_match(family)

}