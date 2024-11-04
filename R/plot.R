#' Create a complete 'ggplot' appropriate to a particular data type
#'
#' @description
#'
#'  This function generates a complete 'ggplot' object suitable for
#'  visualizing time series data in an `aedseo_tsd` object. It creates a line
#'  plot connecting the observations and adds points at each data point.
#'
#' @param x An `aedseo_tsd` or `aedseo` object
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A 'ggplot' object for visualizing the time series data.
#'
#' @aliases plot
#'
#' @seealso [autoplot()]
#'
#' @examples
#' # Create an example aedseo_tsd object
#' aedseo_tsd_object <- tsd(
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
#' # Create a ggplot visualization for the aedseo_tsd object
#' plot(aedseo_tsd_object)
#'
#' # Create an aedseo object
#' aedseo_object <- aedseo(
#'   tsd = aedseo_tsd_object,
#'   k = 3,
#'   level = 0.95,
#'   family = "quasipoisson"
#' )
#'
#' # Create a ggplot visualization of growth rates with confidence intervals
#' plot(aedseo_object, linewidth = 1, alpha = 0.2)
#' @importFrom graphics plot
#' @rdname plot
#' @method plot aedseo_tsd
#' @export
plot.aedseo_tsd <- function(x, ...) {
  print(autoplot(x, ...))
}
#' @rdname plot
#' @method plot aedseo
#' @export
plot.aedseo <- function(x, ...) {
  print(autoplot(object = x, ...))
}
