#' Create a complete 'ggplot' appropriate to a particular data type
#'
#' @description
#'
#' This function generates a complete 'ggplot' object suitable for visualizing time series data in an `tsd` object.
#' It creates a line plot connecting the observations and adds points at each data point.
#'
#' @param x An `tsd` or `tsd_onset` object
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A 'ggplot' object for visualizing the time series data.
#'
#' @aliases plot
#'
#' @seealso [autoplot()]
#'
#' @examples
#' # Create an example `tsd` object
#' time_series <- to_time_series(
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
#' # Create a ggplot visualization for the `tsd` object
#' plot(time_series)
#'
#' # Create an `tsd_onset` object
#' object <- seasonal_onset(
#'   tsd = time_series,
#'   k = 3,
#'   level = 0.95,
#'   family = "quasipoisson"
#' )
#'
#' # Create a ggplot visualization of growth rates with confidence intervals
#' plot(object, linewidth = 1, alpha = 0.2)
#' @importFrom graphics plot
#' @rdname plot
#' @method plot tsd
#' @export
plot.tsd <- function(x, ...) {
  print(autoplot(x, ...))
}
#' @rdname plot
#' @method plot tsd_onset
#' @export
plot.tsd_onset <- function(x, ...) {
  plot_list <- autoplot(object = x, ...)
  suppressWarnings(print(plot_list$observed))
  suppressWarnings(print(plot_list$growth_rate))
}
