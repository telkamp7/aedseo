test_that("Summary prints without errors", {
  # Set start and end dates
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
  # Calculate AEDSEO with a 3-day window
  aedseo_poisson <- aedseo(
    tsd = tsd_data_poisson,
    k = 3,
    level = 0.95,
    family = "poisson"
  )

  # Capture the output of the summary function
  tmp <- capture_output(summary(aedseo_poisson))

  # Verify that the summary printed without errors
  expect_true(grepl(pattern = "Summary of aedseo Object", x = tmp))
})
