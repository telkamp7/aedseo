#' Summary method for aedseo objects
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarize the results of an aedseo analysis, including the latest growth rate estimate,
#' the confidence interval, and information about growth warnings.
#'
#' @param object An object of class 'aedseo' containing the results of an aedseo analysis.
#' @param ... Additional arguments (not used).
#'
#' @return This function is used for its side effect, which is printing a summary message
#'   to the console.
#'
#' @examples
#' # Create a tsibble object from sample data
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
#' # Calculate AEDSEO with a 3-day window and a Poisson family model
#' aedseo_results <- aedseo(
#'   tsd = tsd_data,
#'   k = 3,
#'   level = 0.95,
#'   family = "poisson"
#' )
#' # Print the summary of the aedseo_results to the console
#' summary(aedseo_results)
summary.aedseo <- function(object, ...) {

  # Extract the last observation
  last_observation <- dplyr::last(object)

  # Extract the reference time
  reference_time <- last_observation$reference_time

  # Latest growth warning
  latest_growth_warning <- object %>%
    dplyr::filter(.data$growth_warning == TRUE) %>%
    dplyr::summarise(latest_growth_warning = dplyr::last(reference_time)) %>%
    dplyr::pull(latest_growth_warning)

  # Calculate the total number of growth warnings
  sum_of_growth_warnings <- object %>%
    dplyr::summarise(sum_of_growth_warnings = sum(.data$growth_warning)) %>%
    dplyr::pull(sum_of_growth_warnings)

  rlang::local_options(digits = 3)

  # Write the output to the console
  cat("Reference time point:", as.character(reference_time), "\n \n")
  cat("Growth rate estimate: \n")
  cat("Estimate   Lower   Upper \n")
  cat("  ",
      last_observation$growth_rate, " ",
      last_observation$lower_growth_rate, " ",
      last_observation$upper_growth_rate, "\n \n")
  cat("Total number of growth warnings in the series:",
      sum_of_growth_warnings, "\n")
  cat("Latest growth warning:", as.character(latest_growth_warning), "\n")

}
