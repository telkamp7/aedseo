#' Predict Growth Rates for Future Time Steps
#'
#' This function is used to predict future growth rates based on a model object created using the AEDSEO package. It takes the model object and the number of future time steps (`n_step`) for which you want to make predictions and returns a prediction tibble.
#'
#' @param object A model object created using the aedseo package, typically
#' the result of the `aedseo` function.
#' @param n_step An integer specifying the number of future time steps for
#' which you want to predict growth rates.
#'
#' @return  A tibble containing the predicted growth rates, including time,
#' estimated growth rate, lower confidence interval, and upper confidence
#' interval for the specified number of future time steps.
#' @export
#'
#' @examples
#' # Create a model using the AEDSEO package
#' tsd_data <- tsd(
#'   observed = c(100, 120, 150, 180, 220, 270),
#'   time = as.Date(c(
#'     "2023-01-01",
#'     "2023-01-02",
#'     "2023-01-03",
#'     "2023-01-04",
#'     "2023-01-05",
#'     "2023-01-06")
#'     ),
#'     time_interval = "day"
#'   )
#'
#' aedseo_results <- aedseo(tsd = tsd_data, k = 3, level = 0.95, family = "poisson")
#'
#' # Predict growth rates for the next 5 time steps
#' prediction <- predict_growth_rate(object = aedseo_results, n_step = 5)
#'
#' # Print the prediction
#' print(prediction)
#'
predict_growth_rate <- function(object, n_step){

  # Calculate the prediction
  prediction <- dplyr::last(object) %>%
    dplyr::reframe(t = 0:n_step,
                   time = reference_time + t,
                   estimate = exp(log(observed) + growth_rate * t),
                   lower = exp(log(observed) + lower_growth_rate * t),
                   upper = exp(log(observed) + upper_growth_rate * t))

  # Return
  return(prediction)

}
