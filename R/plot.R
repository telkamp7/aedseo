#' Create a complete 'ggplot' appropriate to a particular data type
#'
#' @description
#'
#' This function generates a complete 'ggplot' object suitable for visualizing time series data in
#' `tsd`, `tsd_onset` or `tsd_onset_and_burden` objects.
#'
#' @param x An `tsd`, `tsd_onset` or `tsd_onset_and_burden` object
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A 'ggplot' object for visualizing output from desired method.
#'
#' @aliases plot
#'
#' @seealso [autoplot.tsd()], [autoplot.tsd_onset()], [autoplot.tsd_onset_and_burden()]
#'
#' @examples
#' # set.seed(321)
#' # Create and plot `tsd` object
#' tsd_obj <- generate_seasonal_data()
#' plot(tsd_obj)
#'
#' # Create and plot `tsd_onset` object
#' tsd_onset_obj <- seasonal_onset(
#'   tsd = tsd_obj,
#'   k = 3,
#'   level = 0.95,
#'   disease_threshold = 50,
#'   family = "quasipoisson"
#' )
#' plot(tsd_onset_obj)
#'
#' @importFrom graphics plot
#' @rdname plot
#' @method plot tsd
#' @export
plot.tsd <- function(x, ...) {
  suppressWarnings(print(autoplot(x, ...)))
}
#' @rdname plot
#' @method plot tsd_onset
#' @export
plot.tsd_onset <- function(x, ...) {
  plot_list <- autoplot(object = x, ...)
  suppressWarnings(print(plot_list$observed))
  suppressWarnings(print(plot_list$growth_rate))
}
