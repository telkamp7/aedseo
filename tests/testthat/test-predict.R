test_that("Returns the desired length", {
  skip_if_not_installed("withr")
  withr::local_seed(222)
  # Generate some sample data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2023-01-01"),
    time_interval = "day"
  )

  # Employ the seasonal_onset function
  tsd_results <- seasonal_onset(
    tsd = tsd_data,
    k = 7,
    level = 0.95,
    family = "poisson"
  )

  # Make a 7 step prediction
  n_step <- 7

  # Predict growth rates for the next 7 time steps
  prediction <- predict(object = tsd_results, n_step = n_step)

  # Return the number of prediction + the initial observation
  expect_length(prediction$estimate, n_step + 1)
})

test_that("Can correctly make an 'tsd_predict' class object", {
  skip_if_not_installed("withr")
  withr::local_seed(222)
  # Generate some sample data
  tsd_data <- generate_seasonal_data(
    years = 1,
    start_date = as.Date("2023-01-01"),
    time_interval = "day"
  )

  # Employ the seasonal_onset function
  tsd_results <- seasonal_onset(
    tsd = tsd_data,
    k = 7,
    level = 0.95,
    family = "poisson"
  )

  # Make a 7 step prediction
  n_step <- 7

  # Predict growth rates for the next 7 time steps
  prediction <- predict(object = tsd_results, n_step = n_step)

  # Return the number of prediction + the initial observation
  expect_s3_class(object = prediction, class = "tsd_predict")
})
