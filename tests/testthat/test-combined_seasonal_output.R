test_that("Test that selection of current and all seasons work as expected", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04")
  )

  current_season <- epi_calendar(dplyr::last(tsd_data$time))

  current_season_output <- combined_seasonal_output(tsd_data, only_current_season = TRUE)
  all_seasons_output <- combined_seasonal_output(tsd_data, only_current_season = FALSE)

  expect_equal(unique(current_season_output$onset_output$season), current_season)
  expect_equal(unique(current_season_output$burden_output$season), current_season)

  expect_gt(length(unique(all_seasons_output$onset_output$season)), 1)
  expect_gt(length(all_seasons_output$burden_output), 1)
})

test_that("Test that onset_output has one more season than burden_output", {
  skip_if_not_installed("withr")
  withr::local_seed(123)
  tsd_data <- generate_seasonal_data(
    years = 3,
    start_date = as.Date("2021-01-04")
  )

  all_seasons_output <- combined_seasonal_output(tsd_data, only_current_season = FALSE)

  expect_length(unique(all_seasons_output$onset_output$season), 4)
  expect_length(all_seasons_output$burden_output, 3)
})
