test_that("Summary prints without errors", {
  # Set start and end dates
  from_date <- as.Date("2021-01-01")
  to_date <- as.Date("2021-01-31")

  # Choose some time dates
  time <- seq.Date(from = from_date, to = to_date, by = "day")

  # Count the number of observations
  n <- length(time)

  # Data
  tsd_data_poisson <- to_time_series(
    observation = rpois(n = n, lambda = 1:n),
    time = time,
    time_interval = "day"
  )
  # Calculate seasonal_onset with a 3-day window
  tsd_poisson <- seasonal_onset(
    tsd = tsd_data_poisson,
    k = 3,
    level = 0.95,
    family = "poisson"
  )

  # Capture the output of the summary function
  tmp <- capture_output(summary(tsd_poisson))

  # Verify that the summary printed without errors
  expect_true(grepl(pattern = "Summary of seasonal_onset Object", x = tmp))
})
