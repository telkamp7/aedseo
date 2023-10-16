test_that("The growth rate models converge", {

  # Number of random data points to generate
  n <- 1e3

  # Set a seed for the simulations
  set.seed(42)

  # The simulated data
  data_poisson <- stats::rpois(n = n, lambda = 5)
  data_nbinom <- stats::rnbinom(n = n, mu = 5, size = 1)

  # Fit the models
  fit_poisson <- fit_growth_rate(observations = data_poisson, level = 0.95, family = "poisson")
  fit_quasipoisson <- fit_growth_rate(observations = data_nbinom, level = 0.95, family = "quasipoisson")
  fit_nbinom <- fit_growth_rate(observations = data_nbinom, level = 0.95, family = "negative.binomial")

  # Check if they all converge
  expect_true(object = fit_poisson$fit$converged)
  expect_true(object = fit_quasipoisson$fit$converged)
  expect_true(object = fit_nbinom$fit$converged)
})
