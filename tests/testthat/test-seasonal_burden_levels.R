test_that("Test that input argument checks work", {

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

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, decay_factor = 3),
      "Variable 'decay_factor': Element 1 is not <= 1."
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, n_peak = c(1, 2)),
      "Variable 'n_peak': Must have length 1, but has length 2."
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, disease_threshold = "hey"),
      "'disease_threshold': Must be of type 'integerish', not 'character'."
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, season_weeks = c(2, 10)),
      "`start` must be greater than `end`!"
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, method = "peak_levels"),
      "Variable 'conf_levels': Must have length 3, but has length 1."
    )
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_data, conf_levels = c(0.2, 0.5)),
      "Variable 'conf_levels': Must have length 1, but has length 2."
    )
  )

  tsd_fail <- tsd_data |> dplyr::rename(week = time)

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_fail),
      paste("Variable 'colnames(tsd)': Names must be a identical to set {'time','observation'},
             but is {'week','observation'}."),
      fixed = TRUE
    )
  )

  #Finally check that dots arguments work
  model_output <- seasonal_burden_levels(tsd_data, family = "exp", optim_method = "Brent",
                                         lower_optim = 1, upper_optim = 1000)
  expect_equal(model_output$family, "exp")
})

test_that("Test that we get correct season output for newest season", {

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

  seasonal_tsd <- tsd_data |>
    dplyr::mutate(season = epi_calendar(.data$time, start = 21, end = 20))

  newest_season <- max(seasonal_tsd$season)

  intensity_levels <- seasonal_burden_levels(tsd_data, method = "intensity_levels")

  expect_equal(newest_season, intensity_levels$season)
})

test_that("Test that we have same numbers of outputs for both methods", {

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

  intensity_levels <- seasonal_burden_levels(tsd_data, method = "intensity_levels")

  peak_levels <- seasonal_burden_levels(tsd_data, method = "peak_levels", conf_levels = c(0.4, 0.9, 0.99))

  expect_equal(length(intensity_levels), length(peak_levels))
})

test_that("Test that function fail with less than two seasons", {

  start_date <- as.Date("2021-06-04")
  end_date <- as.Date("2021-12-31")

  weekly_dates <- seq.Date(from = start_date,
                           to = end_date,
                           by = "week")

  obs <- stats::rpois(length(weekly_dates), 1000)

  tsd_one_season <- to_time_series(
    observation = obs,
    time = as.Date(weekly_dates),
    time_interval = "week"
  )

  expect_error(
    checkmate_err_msg(seasonal_burden_levels(tsd_one_season),
      "There must be at least two unique seasons in the data."
    )
  )
})

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

  current_level <- seasonal_burden_levels(tsd_data, only_current_season = TRUE)
  all_levels <- seasonal_burden_levels(tsd_data, only_current_season = FALSE)

  expect_equal(current_season, unique(current_level$season))
  expect_gt(length(all_levels), 1)
})


test_that("Test that function does not fail if there are no observations surpassing the disease specific threshold
          in the current season.", {
            start_date <- as.Date("2022-05-23")
            end_date <- as.Date("2023-05-15")

            start_date_2 <- as.Date("2023-05-22")
            end_date_2 <- as.Date("2024-05-13")

            weekly_dates <- seq.Date(from = start_date,
                                     to = end_date,
                                     by = "week")

            weekly_dates_2 <- seq.Date(from = start_date_2,
                                       to = end_date_2,
                                       by = "week")

            obs <- stats::rpois(length(weekly_dates), 1000)
            obs_2 <- stats::rpois(length(weekly_dates), 10)

            tsd_data <- to_time_series(
              observation = c(obs, obs_2),
              time = c(as.Date(weekly_dates), as.Date(weekly_dates_2)),
              time_interval = "week"
            )

            burden_list <- seasonal_burden_levels(tsd_data, disease_threshold = 100)

            expect_equal(burden_list$season, "2023/2024")
          })
