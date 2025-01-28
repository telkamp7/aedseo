# Make a custom week/year label for the x axis - inspired by scales::label_date_short
label_week_short <- function() {
  function(x) {
    # Detect changes in vector (between season end and start)
    changed <- function(x) {
      c(TRUE, is.na(x[-length(x)]) | x[-1] != x[-length(x)])
    }
    # Split dates into year and week
    dt <- dplyr::tibble(year = as.character(lubridate::isoyear(x)), week = as.character(lubridate::isoweek(x)))
    # Detect changes to either year or week
    changes <- cbind(year = changed(dt$year), week = changed(dt$week))
    # Keep only the labels where changes occur
    dt2 <- cbind(ifelse(changes[, 1], dt$year, NA),
                 ifelse(changes[, 2], dt$week, NA))
    # Combine to character two-line label with week in first line and year in second line
    apply(dt2, 1, function(x) paste(rev(x[!is.na(x)]), collapse = "\n"))
  }
}

time_interval_x_axis <- function(start_date, end_date, time_interval_step) {
  if (grepl("week", time_interval_step)) {
    x_axis_scale <- ggplot2::scale_x_date(breaks = seq(start_date, end_date, by = time_interval_step),
                                          limits = c(start_date, end_date), oob = scales::oob_keep,
                                          labels = label_week_short(),
                                          expand = ggplot2::expansion(mult = c(0, 0), add = c(4, 4)))
    x_label <- "Week"
  } else if (grepl("month", time_interval_step)) {
    x_axis_scale <- ggplot2::scale_x_date(breaks = seq(start_date, end_date, by = time_interval_step),
                                          limits = c(start_date, end_date), oob = scales::oob_keep,
                                          labels = scales::label_date_short(format = c("%Y", "%b"), sep = "\n"),
                                          expand = ggplot2::expansion(mult = c(0, 0), add = c(4, 4)))
    x_label <- "Month"
  } else if (grepl("day", time_interval_step)) {
    x_axis_scale <- ggplot2::scale_x_date(breaks = seq(start_date, end_date, by = time_interval_step),
                                          limits = c(start_date, end_date), oob = scales::oob_keep,
                                          labels = scales::label_date_short(format = c("%Y", "%b", "%d"), sep = "\n"),
                                          expand = ggplot2::expansion(mult = c(0, 0), add = c(4, 4)))
    x_label <- "Day"
  }

  list(
    x_axis_scale,
    ggplot2::labs(x = x_label)
  )
}
