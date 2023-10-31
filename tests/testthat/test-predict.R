test_that("Returns the desired length", {
  # Generate some sample data
  tsd_data <- tsd(
    observed = c(100, 120, 150, 180, 220, 270),
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

  # Employ the aedseo function
  aedseo_results <- aedseo(
    tsd = tsd_data,
    k = 3,
    level = 0.95,
    family = "poisson"
  )

  # Make a 5 step prediction
  n_step <- 5

  # Predict growth rates for the next 5 time steps
  prediction <- predict(object = aedseo_results, n_step = n_step)

  # Return the number of prediction + the initial observation
  expect_length(prediction$estimate, n_step + 1)
})

test_that("Can correctly make an 'aedseo_predics' class object", {
  # Generate some sample data
  tsd_data <- tsd(
    observed = c(100, 120, 150, 180, 220, 270),
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

  # Employ the aedseo function
  aedseo_results <- aedseo(
    tsd = tsd_data,
    k = 3,
    level = 0.95,
    family = "poisson"
  )

  # Make a 5 step prediction
  n_step <- 5

  # Predict growth rates for the next 5 time steps
  prediction <- predict(object = aedseo_results, n_step = n_step)

  # Return the number of prediction + the initial observation
  expect_s3_class(object = prediction, class = "aedseo_predict")
})
