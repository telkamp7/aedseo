test_that("The growth rate models converge", {
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
  tsd_data_nbinom <- tsd(
    observed = rnbinom(n = n, mu = 1:n, size = 5),
    time = time,
    time_interval = "day"
  )

  # Calculate AEDSEO with a 3-day window
  aedseo_poisson <- aedseo(
    tsd = tsd_data_poisson,
    k = 3,
    level = 0.95,
    family = "poisson",
    disease_threshold = 20,
    na_percentage_allowed = 0.2
  )
  aedseo_quasipoisson <- aedseo(
    tsd = tsd_data_nbinom,
    k = 3,
    level = 0.95,
    family = "quasipoisson",
    disease_threshold = 20,
    na_percentage_allowed = 0.2
  )

  # Check if they all converge
  expect_true(object = all(aedseo_poisson$converged))
  expect_true(object = all(aedseo_quasipoisson$converged))
})

test_that("Test if it works with weeks with NA values", {
  from_date <- as.Date("2021-01-01")
  to_date <- as.Date("2021-01-31")

  # Choose some time dates
  time <- seq.Date(from = from_date, to = to_date, by = "day")

  # Count the number of observations
  n <- length(time)

  # Add NA values to observed
  na_count <- 15

  # Randomly select indices to replace with NA
  na_indices <- sample(1:n, na_count, replace = FALSE)

  # Create observable
  observed <- rpois(n = n, lambda = 1:n)

  # Add NA values
  observed[na_indices] <- NA

  # Data
  tsd_data_poisson_na <- tsd(
    observed = observed,
    time = time,
    time_interval = "day"
  )

  # Calculate AEDSEO with a 3-day window
  aedseo_poisson_na <- aedseo(
    tsd = tsd_data_poisson_na,
    k = 3,
    level = 0.95,
    family = "poisson",
    disease_threshold = 20,
    na_percentage_allowed = 0.4
  )

  # Test if correct amount of windows with NA are skipped
  k <- 3
  na_percentage_allowed <- 0.4
  n <- base::nrow(tsd_data_poisson_na)
  skipped_window_count <- 0

  for (i in k:n) {
    obs_iter <- tsd_data_poisson_na[(i - k + 1):i, ]
    if (sum(is.na(obs_iter)) >= k * na_percentage_allowed) {
      skipped_window_count <- skipped_window_count + 1
    }
  }

  # Not all will be converged due to NA injections
  expect_false(all(aedseo_poisson_na$converged))
  # Count if the skipped windows are = ones in output
  expect_equal(skipped_window_count, sum(aedseo_poisson_na$skipped_window))
})
