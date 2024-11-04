#' Fit a growth rate model to time series observations.
#'
#' @description
#'
#' This function fits a growth rate model to time series observations and
#' provides parameter estimates along with confidence intervals.
#'
#' @param observations A numeric vector containing the time series
#' observations.
#' @param level The confidence level for parameter estimates, a numeric
#' value between 0 and 1.
#' @param family A character string specifying the family for modeling.
#' Choose between "poisson," or "quasipoisson".
#'
#' @return A list containing:
#'   - 'fit': The fitted growth rate model.
#'   - 'estimate': A numeric vector with parameter estimates, including
#'   the growth rate and its confidence interval.
#'   - 'level': The confidence level used for estimating parameter
#'   confidence intervals.
#' @export
#'
#' @examples
#' # Fit a growth rate model to a time series of counts
#' # (e.g., population growth)
#' data <- c(100, 120, 150, 180, 220, 270)
#' growth_rate_model <- fit_growth_rate(
#'   observations = data,
#'   level = 0.95,
#'   family = "poisson"
#' )
#'
#' # Print the estimated growth rate and confidence interval
#' print(growth_rate_model$estimate)
#'
fit_growth_rate <- function(
    observations,
    level = 0.95,
    family = c(
      "poisson",
      "quasipoisson" # TODO #10 Include negative.binomial regressions. @telkamp7
    )) {
  safe_confint <- purrr::safely(stats::confint)

  # Match the arguements
  family <- rlang::arg_match(family)

  # Calculate the length of the series
  n <- base::length(observations)

  # Construct a data.frame wit growth rate data
  growth_data <- stats::aggregate(
    observations,
    by = list(growth_rate = rep(1:n, each = 1)),
    FUN = sum
  )

  # Fit the model using the specified family
  growth_fit <- base::switch(family,
    poisson = stats::glm(
      formula = x ~ growth_rate,
      data = growth_data,
      family = stats::poisson(link = "log")
    ),
    quasipoisson = stats::glm(
      formula = x ~ growth_rate,
      data = growth_data,
      family = stats::quasipoisson(link = "log")
    )
  )

  # Calculate the 'safe' confidence intervals
  growth_confint <- suppressMessages(
    safe_confint(
      object = growth_fit,
      parm = "growth_rate",
      level = level
    )
  )

  # Collect the estimates
  ans <- c(
    stats::coef(object = growth_fit)["growth_rate"],
    growth_confint$result
  )

  return(list(
    fit = growth_fit,
    estimate = ans,
    level = level
  ))
}
