#' Create a complete 'ggplot' appropriate to a particular data type
#'
#' @description
#'
#'  This function generates a complete 'ggplot' object suitable for visualizing time series data in an `tsd` object.
#' It creates a line plot connecting the observations and adds points at each data point.
#'
#' @param object An `tsd` or `seasonal_onset_tsd` object
#' @param linewidth Numeric, the width of the line for the growth rate
#' @param size Numeric, size of observational points.
#' @param width Numeric, the width of the error bar employed to show the
#' confidence interval of the growth rate estimate.
#' @param alpha Numeric, the alpha (transparency) for the observations with a
#' significantly positive growth rate.
#' @param ... Additional arguments (not used).
#'
#' @return A 'ggplot' object for visualizing the time series data.
#'
#' @aliases autoplot
#'
#' @examples
#' # Create an example tsd object
#' tsd_object <- tsd(
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
#' # Create a ggplot visualization for the tsd object
#' autoplot(tsd_object)
#'
#' # Create an tsd object
#' tsd_object <- seasonal_onset(
#'   tsd = tsd_object,
#'   k = 3,
#'   level = 0.95,
#'   family = "quasipoisson"
#' )
#'
#' # Create a ggplot visualization of growth rates with confidence intervals
#' autoplot(tsd_object, linewidth = 1, alpha = 0.2)
#' @importFrom ggplot2 autoplot
#' @rdname autoplot
#' @export
autoplot <- function(object, ...) {
  UseMethod("autoplot")
}
#' @rdname autoplot
#' @method autoplot tsd
#' @export
autoplot.tsd <- function(object, ...) {
  object %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data$time,
        y = .data$observation
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line()
}
#' @importFrom grDevices devAskNewPage
#' @rdname autoplot
#' @method autoplot seasonal_onset
#' @export
autoplot.seasonal_onset <- function(
    object,
    linewidth = 0.7,
    size = 2,
    alpha = 0.3,
    width = 0.2,
    ...) {
  # Construct the observed cases plot
  # NOTE: We use print to show plots sequentially
  suppressWarnings(
    print(
      object %>%
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            x = .data$reference_time,
            y = .data$observation
          )
        ) +
        ggplot2::geom_point(
          mapping = ggplot2::aes(alpha = .data$seasonal_onset_alarm),
          size = size
        ) +
        ggplot2::geom_line(linewidth = linewidth)
    )
  )
  # Set 'ask' for plotting device to TRUE
  oask <- devAskNewPage(ask = TRUE)
  # ... and clean-up on exit
  on.exit(devAskNewPage(oask))
  # ... and the growth rate plots
  print(
    object %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = .data$reference_time,
          y = .data$growth_rate,
          ymin = .data$lower_growth_rate,
          ymax = .data$upper_growth_rate
        )
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(alpha = .data$growth_warning),
        size = size
      ) +
      ggplot2::geom_errorbar(
        mapping = ggplot2::aes(alpha = .data$growth_warning),
        width = width
      ) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed")
  )
}
