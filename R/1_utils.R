# Make a custom week/year label for the x axis - inspired by scales::label_date_short
label_week_short <- function() {
  function(x) {

    changed <- function(x) {
      c(TRUE, is.na(x[-length(x)]) | x[-1] != x[-length(x)])
    }

    dt <- dplyr::tibble(year = as.character(lubridate::isoyear(x)), week = as.character(lubridate::isoweek(x)))

    changes <- cbind(year = changed(dt$year), week = changed(dt$week))

    dt2 <- cbind(ifelse(changes[, 1], dt$year, NA),
                 ifelse(changes[, 2], dt$week, NA))

    apply(dt2, 1, function(x) paste(rev(x[!is.na(x)]), collapse = "\n"))
  }
}

time_interval_x_axis <- function(start_date, end_date, time_interval) {
  if (grepl("week", time_interval)) {
    x_axis_scale <- ggplot2::scale_x_date(breaks = seq(start_date, end_date, by = time_interval),
                                          limits = c(start_date, end_date), oob = scales::oob_keep,
                                          labels = label_week_short(),
                                          expand = ggplot2::expansion(mult = c(0, 0), add = c(4, 4)))
    x_label <- "Week"
  } else if (grepl("month", time_interval)) {
    x_axis_scale <- ggplot2::scale_x_date(breaks = seq(start_date, end_date, by = time_interval),
                                          limits = c(start_date, end_date), oob = scales::oob_keep,
                                          labels = scales::label_date_short(format = c("%Y", "%b"), sep = "\n"),
                                          expand = ggplot2::expansion(mult = c(0, 0), add = c(4, 4)))
    x_label <- "Month"
  } else if (grepl("day", time_interval)) {
    x_axis_scale <- ggplot2::scale_x_date(breaks = seq(start_date, end_date, by = time_interval),
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
