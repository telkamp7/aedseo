test_that("The growth rate models converge", {
  from_date <- as.Date("2021-01-01")
  to_date <- as.Date("2021-01-31")

  # Choose some time dates
  time <- seq.Date(from = from_date, to = to_date, by = "day")

  # Count the number of observations
  n <- length(time)

  # Data
  set.seed(123)
  tsd_data_poisson <- to_time_series(
    observation = rpois(n = n, lambda = 1:n),
    time = time,
    time_interval = "day"
  )
  tsd_data_nbinom <- to_time_series(
    observation = rnbinom(n = n, mu = 1:n, size = 5),
    time = time,
    time_interval = "day"
  )

  # Calculate seasonal_onset with a 3-day window
  tsd_poisson <- seasonal_onset(
    tsd = tsd_data_poisson,
    k = 3,
    level = 0.95,
    family = "poisson",
    disease_threshold = 20,
    na_fraction_allowed = 0.2
  )
  tsd_quasipoisson <- seasonal_onset(
    tsd = tsd_data_nbinom,
    k = 3,
    level = 0.95,
    family = "quasipoisson",
    disease_threshold = 20,
    na_fraction_allowed = 0.2
  )

  # Check if they all converge
  expect_true(object = all(tsd_poisson$converged))
  expect_true(object = all(tsd_quasipoisson$converged))
})

test_that("Test if it works with weeks with NA values", {
  from_date <- as.Date("2021-01-01")
  to_date <- as.Date("2021-01-31")

  # Choose some time dates
  time <- seq.Date(from = from_date, to = to_date, by = "day")

  # Count the number of observations
  n <- length(time)

  # Add NA values to observation
  na_count <- 15

  # Randomly select indices to replace with NA
  set.seed(123)
  na_indices <- sample(1:n, na_count, replace = FALSE)

  # Create observable
  observation <- rpois(n = n, lambda = 1:n)

  # Add NA values
  observation[na_indices] <- NA

  # Data
  tsd_data_poisson_na <- to_time_series(
    observation = observation,
    time = time,
    time_interval = "day"
  )

  # Calculate seasonal_onset with a 3-day window
  tsd_poisson_na <- seasonal_onset(
    tsd = tsd_data_poisson_na,
    k = 3,
    level = 0.95,
    family = "poisson",
    disease_threshold = 20,
    na_fraction_allowed = 0.4
  )

  # Test if correct amount of windows with NA are skipped
  k <- 3
  na_fraction_allowed <- 0.4
  n <- base::nrow(tsd_data_poisson_na)
  skipped_window_count <- 0

  for (i in k:n) {
    obs_iter <- tsd_data_poisson_na[(i - k + 1):i, ]
    if (sum(is.na(obs_iter)) >= k * na_fraction_allowed) {
      skipped_window_count <- skipped_window_count + 1
    }
  }

  # Not all will be converged due to NA injections
  expect_false(all(tsd_poisson_na$converged))
  # Count if the skipped windows are = ones in output
  expect_equal(skipped_window_count, sum(tsd_poisson_na$skipped_window))
})

test_that("Test that input argument checks work", {

  tsd_data <- to_time_series(
    observation = c(100, 120, 150, 180, 220, 270),
    time = as.Date(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    time_interval = "day"
  )

  expect_no_error(seasonal_onset(tsd_data))

  # Expect error when not matching family
  expect_error(seasonal_onset(tsd_data, family = "ttt"))

  # Expect errors from wrong input arguments
  expect_error(seasonal_onset(tsd_data, k = 1.4, disease_threshold = 1.5))
  expect_error(seasonal_onset(tsd_data, disease_threshold = 1.5))
  expect_error(seasonal_onset(tsd_data, level = 2))
  expect_error(seasonal_onset(tsd_data, na_fraction_allowed = 2))

  # Expect error with random data frame
  r_df <- data.frame(
    observation = c(100, 120, 150, 180, 220, 270),
    time = as.Date(c(
      "2023-01-01",
      "2023-01-02",
      "2023-01-03",
      "2023-01-04",
      "2023-01-05",
      "2023-01-06"
    )),
    time_interval = "day"
  )

  expect_error(seasonal_onset(r_df))

  # Expect error with wrong column names
  colnames(tsd_data) <- c("hey", "test")
  expect_error(seasonal_onset(tsd_data))

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

  current_onset <- seasonal_onset(tsd_data, season_start = 21, only_current_season = TRUE)
  all_onsets <- seasonal_onset(tsd_data, season_start = 21, only_current_season = FALSE)

  expect_equal(current_season, unique(current_onset$season))
  expect_gt(length(unique(all_onsets$season)), 1)
})
