#' Predict Growth Rates for Future Time Steps
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function is used to predict future growth rates based on a model object
#' created using the AEDSEO package. It takes the model object and the number
#' of future time steps (`n_step`) for which you want to make predictions and
#' returns a prediction tibble.
#'
#' @param object A model object created using the `aedseo` package, typically
#' the result of the `aedseo()` function.
#' @param n_step An integer specifying the number of future time steps for
#' which you want to predict growth rates. Default is 3.
#' @param ... Additional arguments (not used).
#'
#' @return  A tibble S3 object called `aedseo` containing the predicted growth
#' rates, including time, estimated growth rate, lower confidence interval,
#' and upper confidence interval for the specified number of future time steps.
#'
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' # Analyze the data using the aedseo package
#' tsd_data <- tsd(
#'   observed = c(100, 120, 150, 180, 220, 270),
#'   time = as.Date(c(
#'     "2023-01-01",
#'     "2023-01-02",
#'     "2023-01-03",
#'     "2023-01-04",
#'     "2023-01-05",
#'     "2023-01-06"
#'   )),
#'   time_interval = "day"
#' )
#'
#' aedseo_results <- aedseo(
#'   tsd = tsd_data,
#'   k = 3,
#'   level = 0.95,
#'   family = "poisson"
#' )
#'
#' # Predict growth rates for the next 5 time steps
#' prediction <- predict(object = aedseo_results, n_step = 5)
#'
#' # Print the prediction
#' print(prediction)
#'
# TODO: #13 Turn the results into a `aedseo_predict` class. @telkmp7
predict.aedseo <- function(object, n_step = 3, ...) {
  # Calculate the prediction
  res <- dplyr::last(object) %>%
    dplyr::reframe(
      t = 0:n_step,
      time = .data$reference_time + t,
      estimate = exp(log(.data$observed) + .data$growth_rate * t),
      lower = exp(log(.data$observed) + .data$lower_growth_rate * t),
      upper = exp(log(.data$observed) + .data$upper_growth_rate * t)
    )

  # Extract the attributes from the object
  attributes_object <- attributes(object)

  # Extract the object k, level, and family
  k <- attributes_object$k
  level <- attributes_object$level
  family <- attributes_object$family

  # Turn the results into an `aedseo` class
  ans <- tibble::new_tibble(
    x = res,
    class = "aedseo_predict",
    k = k,
    level = level,
    family = family
  )

  # Return
  return(ans)
}
