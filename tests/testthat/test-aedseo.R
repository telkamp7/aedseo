test_that("The growth rate models converge", {
  from_date <- as.Date("2021-01-01")
  to_date <- as.Date("2021-01-31")

  # Choose some time dates
  time <- seq.Date(from = from_date, to = to_date, by = "day")

  # Count the number of observations
  n <- length(time)

  # Data
  tsd_data_poisson <- tsd(
    observed = rpois(n = n, lambda = 1:n),
    time = time,
    time_interval = "day"
  )
  tsd_data_nbinom <- tsd(
    observed = rnbinom(n = n, mu = 1:n, size = 5),
    time = time,
    time_interval = "day"
  )

  # Calculate AEDSEO with a 3-day window
  aedseo_poisson <- aedseo(
    tsd = tsd_data_poisson,
    k = 3,
    level = 0.95,
    family = "poisson"
  )
  aedseo_quasipoisson <- aedseo(
    tsd = tsd_data_nbinom,
    k = 3,
    level = 0.95,
    family = "quasipoisson"
  )

  # Check if they all converge
  expect_true(object = all(aedseo_poisson$converged))
  expect_true(object = all(aedseo_quasipoisson$converged))
})
