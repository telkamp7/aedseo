#' @rdname autoplot
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
#' @param object An `aedseo` object containing growth rate estimates
#' @param linewidth Numeric, the width of the line for the growth rate
#' @param alpha Numeric, the alpha (transparency) for the confidence interval ribbon
#' @param ... Additional arguments to be passed to the ggplot2::ggplot function
#'
#' @return A ggplot object for visualizing growth rates with confidence intervals
#'
#' @export
#'
#' @examples
#' # Create an aedseo object (replace with your actual object creation)
#' aedseo_object <- aedseo(
#'   tsd = tsd_data,
#'   k = 3,
#'   level = 0.95,
#'   family = "quasipoisson")
#'
#' # Create a plot of growth rates with confidence intervals
#' autoplot(aedseo_object, linewidth = 1, alpha = 0.2)
autoplot.aedseo <- function(object, linewidth = 0.7, alpha = 0.3, ...) {
  object %>%
    ggplot(
      mapping = aes(
        x = .data$reference_time,
        y = .data$growth_rate,
        ymin = .data$lower_growth_rate,
        ymax = .data$upper_growth_rate)) +
    geom_line(linewidth = linewidth) +
    geom_ribbon(alpha = alpha)
}


