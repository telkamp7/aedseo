test_that("output only includes one season", {

  start_date <- as.Date("2021-01-04")
  end_date <- as.Date("2023-12-31")

  weekly_dates <- seq.Date(from = start_date,
                           to = end_date,
                           by = "week")

  obs <- stats::rpois(length(weekly_dates), 1000)

  tsd_data <- to_time_series(
    observation = obs,
    time = as.Date(weekly_dates),
    time_interval = "week"
  )

  combined_df <- combined_seasonal_output(tsd = tsd_data)

  expect_equal(length(unique(combined_df$onset_output$season)), 1)
  expect_equal(length(combined_df$burden_output$season), 1)
})

