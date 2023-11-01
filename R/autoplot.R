#' Create a complete ggplot appropriate for a `aedseo_tsd` object
#'
#' This function generates a complete ggplot object suitable for visualizing
#' time series data in an `aedseo_tsd` object. It creates a line plot connecting
#' the observations and adds points at each data point.
#'
#' @param object An `aedseo_tsd` object containing time series data with columns
#' 'time' (containing time points in date format) and 'observed' (integer values
#' representing observed counts).
#' @param ... Additional arguments to be passed to `ggplot`.
#'
#' @return A ggplot object for visualizing the time series data.
#'
#' @export
#'
#' @examples
#' # Create an example aedseo_tsd object
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
#' # Create a ggplot visualization for the aedseo_tsd object
#' autoplot(tsd_data)
autoplot.aedseo_tsd <- function(object, ...) {
  object %>%
    ggplot(mapping = aes(x = .data$time, y = .data$observed)) +
    geom_point() +
    geom_line()
}


#' Title
#'
#' @param object
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
autoplot.aedseo <- function(object, ...) {
  object %>%
    ggplot(mapping = aes(x = .data$time, y = .data$observed)) +
    geom_point() +
    geom_line()
}


