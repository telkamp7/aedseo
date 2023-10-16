


fit_growth_rate <- function(
    observations,
    level = 0.95,
    family = c(
      stats::poisson(link = "log"),
      stats::quasipoisson(link = "log")))
  {

  # Calculate the length of the series
  n <- base::length(observations)

  # Construct a data.frame wit growth rate data
  growth_data <- stats::aggregate(
    observations,
    by = list(growth_rate = rep(1:n, each=1)),
    FUN = sum)

  # Fit the model using the specified family
  growth_fit <- stats::glm(
    formula = x ~ growth_rate,
    data = growth_data,
    family = family)

  # Profile the model
  growth_profile <- stats::profile(fitted = growth_fit)

  # Collect the estimates
  ans <- c(
    stats::coef(object = growth_fit)["growth_rate"],
    stats::confint(object = growth_profile,
                   parm = "growth_rate",
                   level = level))

  return(list(
    estimate = ans,
    level = level,
    family = family))

}
