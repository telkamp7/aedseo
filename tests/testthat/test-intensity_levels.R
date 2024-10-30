test_that("Test that input argument checks work", {

  start_date <- as.Date("2021-01-04")
  end_date <- as.Date("2023-12-31")

  weekly_dates <- seq.Date(from = start_date,
                           to = end_date,
                           by = "week")

  obs <- rpois(length(weekly_dates), 1000)

  tsd_data <- tsd(
    observed = obs,
    time = as.Date(weekly_dates),
    time_interval = "week"
  )

  expect_no_error(intensity_levels(tsd_data))

  expect_error(intensity_levels(tsd_data, decay_factor = 3))

  expect_error(intensity_levels(tsd_data, n_peak = c(1, 2)))

  expect_error(intensity_levels(tsd_data, disease_threshold = "hey"))

  expect_error(intensity_levels(tsd_data, season_weeks = c(2, 10)))

  tsd_fail <- tsd_data |> dplyr::rename(week = time)

  expect_error(intensity_levels(tsd_fail))

  #Finally check that dots arguments work
  expect_no_error(intensity_levels(tsd_data, conf_levels = c(0.2, 0.5, 0.9)))

  expect_no_error(intensity_levels(tsd_data, family = "exp", optim_method = "Brent",
                                   lower_optim = 1, upper_optim = 1000))

})

test_that("Test we get correct season output for newest season", {

  start_date <- as.Date("2021-01-04")
  end_date <- as.Date("2023-12-31")

  weekly_dates <- seq.Date(from = start_date,
                           to = end_date,
                           by = "week")

  obs <- rpois(length(weekly_dates), 1000)

  tsd_data <- tsd(
    observed = obs,
    time = as.Date(weekly_dates),
    time_interval = "week"
  )

  seasonal_tsd <- tsd_data |>
    dplyr::mutate(season = epi_calendar(.data$time, start = 21, end = 20))

  newest_season <- unique(max(seasonal_tsd$season))

  intensity_levels <- intensity_levels(tsd_data)

  expect_equal(newest_season, intensity_levels$season)

})
