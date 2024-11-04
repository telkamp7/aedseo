#' Summary method for aedseo objects
#'
#' @description
#'
#' Summarize the results of an seasonal onset analysis, including the latest growth rate estimate,
#' the confidence interval, and information about growth warnings.
#'
#' @param object An object of class 'seasonal_onset' containing the results of a seasonal_onset analysis.
#' @param ... Additional arguments (not used).
#'
#' @return This function is used for its side effect, which is printing a summary message to the console.
#'
#' @export
#'
#' @examples
#' # Create a tibble object from sample data
#' tsd_data <- tsd(
#'   observation = c(100, 120, 150, 180, 220, 270),
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
#' # Calculate aedseo with a 3-day window and a Poisson family model
#' seasona_onset_results <- aedseo(
#'   tsd = tsd_data,
#'   k = 3,
#'   level = 0.95,
#'   family = "poisson"
#' )
#' # Print the summary of the seasonal_onset_results to the console
#' summary(seasonal_onset_results)
summary.seasonal_onset <- function(object, ...) {
  # Extract the last observation
  last_observation <- dplyr::last(object)

  # Extract the reference time
  reference_time <- last_observation$reference_time

  # Extract the time_interval
  time_interval <- attr(object, "time_interval")

  # Extract the seasons
  seasons <- toString(unique(object$season))

  # Latest sum of cases
  latest_sum_of_cases <- object %>%
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
    dplyr::pull(.data$sum_of_cases)

  # Latest sum of cases warning
  latest_sum_of_cases_warning <- object %>%
    dplyr::filter(.data$sum_of_cases_warning == TRUE) %>%
    dplyr::summarise(
      latest_sum_of_cases_warning = dplyr::last(reference_time)
    ) %>%
    dplyr::pull(latest_sum_of_cases_warning)

  # Latest growth warning
  latest_growth_warning <- object %>%
    dplyr::filter(.data$growth_warning == TRUE) %>%
    dplyr::summarise(latest_growth_warning = dplyr::last(reference_time)) %>%
    dplyr::pull(latest_growth_warning)

  # Latest growth warning
  latest_seasonal_onset_alarm <- object %>%
    dplyr::filter(.data$seasonal_onset_alarm == TRUE) %>%
    dplyr::summarise(
      latest_seasonal_onset_alarm = dplyr::last(reference_time)
    ) %>%
    dplyr::pull(latest_seasonal_onset_alarm)

  # Calculate the total number of growth warnings
  sum_of_growth_warnings <- object %>%
    dplyr::summarise(sum_of_growth_warnings = sum(.data$growth_warning)) %>%
    dplyr::pull(sum_of_growth_warnings)

  # Extract the attributes from the object
  attributes_object <- attributes(object)

  # Extract the object k, level, and family
  k <- attributes_object$k
  level <- attributes_object$level
  disease_threshold <- attributes_object$disease_threshold
  family <- attributes_object$family

  # Extract the lower and upper confidence intervals
  lower_confidence_interval <- (1 - level) / 2
  upper_confidence_interval <- level + lower_confidence_interval

  # Generate the summary message
  summary_message <- sprintf(
    "Summary of seasonal_onset Object

    Called using distributional family:
      %s

    Window size for growth rate estimation and
    calculation of sum of cases:
      %d

    The time interval for the observations:
      %s

    Disease specific threshold:
      %d

    Reference time point:
      %s

    Sum of cases at reference time point:
      %d
    Latest sum of cases warning:
      %s

    Growth rate estimate at reference time point:
      Estimate   Lower (%.1f%%)   Upper (%.1f%%)
         %.3f     %.3f          %.3f

    Total number of growth warnings in the series:
      %d
    Latest growth warning:
      %s

    Latest seasonal onset alarm:
      %s

    The seasons defined in the series:
      %s",
    family,
    k,
    time_interval,
    disease_threshold,
    as.character(reference_time),
    latest_sum_of_cases,
    as.character(latest_sum_of_cases_warning),
    lower_confidence_interval * 100,
    upper_confidence_interval * 100,
    last_observation$growth_rate,
    last_observation$lower_growth_rate,
    last_observation$upper_growth_rate,
    sum_of_growth_warnings,
    as.character(latest_growth_warning),
    as.character(latest_seasonal_onset_alarm),
    seasons
  )

  # Print the summary message
  cat(summary_message)
}
