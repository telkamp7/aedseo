test_that("Summary prints without errors", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  # Generate seasonal data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2021-01-01")
  )

  # Calculate seasonal_onset with a 3-day window
  tsd_onset <- seasonal_onset(
    tsd = tsd_data,
    k = 3,
    level = 0.95,
    family = "quasipoisson"
  )

  # Capture the output of the summary function
  tmp <- capture_output(summary(tsd_onset))

  # Verify that the summary printed without errors
  expect_true(grepl(pattern = "Summary of tsd_onset object", x = tmp))
})
