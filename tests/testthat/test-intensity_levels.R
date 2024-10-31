test_that("Test that input argument checks work", {

  start_date <- as.Date("2021-01-04")
  end_date <- as.Date("2023-12-31")

  weekly_dates <- seq.Date(from = start_date,
                           to = end_date,
                           by = "week")

  obs <- stats::rpois(length(weekly_dates), 1000)

  tsd_data <- tsd(
    observed = obs,
    time = as.Date(weekly_dates),
    time_interval = "week"
  )

  expect_no_error(intensity_levels(tsd_data))

  expect_error(
    checkmate_err_msg(intensity_levels(tsd_data, decay_factor = 3),
      "Variable 'decay_factor': Element 1 is not <= 1."
    )
  )

  expect_error(
    checkmate_err_msg(intensity_levels(tsd_data, n_peak = c(1, 2)),
      "Variable 'n_peak': Must have length 1, but has length 2."
    )
  )

  expect_error(
    checkmate_err_msg(intensity_levels(tsd_data, disease_threshold = "hey"),
      "'disease_threshold': Must be of type 'integerish', not 'character'.",
      fixed = TRUE
    )
  )

  expect_error(
    checkmate_err_msg(intensity_levels(tsd_data, season_weeks = c(2, 10)),
      "`start` must be greater than `end`!"
    )
  )

  tsd_fail <- tsd_data |> dplyr::rename(week = time)

  expect_error(
    checkmate_err_msg(intensity_levels(tsd_fail),
      "Variable 'colnames(tsd)': Names must be a identical to set {'time','observed'}, but is {'week','observed'}.",
      fixed = TRUE
    )
  )

  #Finally check that dots arguments work
  expect_no_error(intensity_levels(tsd_data, conf_levels = c(0.2, 0.5, 0.9)))

  expect_no_error(intensity_levels(tsd_data, family = "exp", optim_method = "Brent",
                                   lower_optim = 1, upper_optim = 1000))

})

test_that("Test that we get correct season output for newest season", {

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

  newest_season <- max(seasonal_tsd$season)

  intensity_levels <- intensity_levels(tsd_data)

  expect_equal(newest_season, intensity_levels$season)

})
