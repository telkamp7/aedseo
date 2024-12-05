test_that("Test that selection of current and all seasons work as expected", {
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

  current_season <- epi_calendar(end_date)

  current_season_output <- combined_seasonal_output(tsd_data, only_current_season = TRUE)
  all_seasons_output <- combined_seasonal_output(tsd_data, only_current_season = FALSE)

  expect_equal(unique(current_season_output$onset_output$season), current_season)
  expect_equal(unique(current_season_output$burden_output$season), current_season)

  expect_gt(length(unique(all_seasons_output$onset_output$season)), 1)
  expect_gt(length(all_seasons_output$burden_output), 1)
})

test_that("Test that onset_output has one more season than burden_output", {
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

  all_seasons_output <- combined_seasonal_output(tsd_data, only_current_season = FALSE)

  expect_length(unique(all_seasons_output$onset_output$season), 4)
  expect_length(all_seasons_output$burden_output, 3)
})
