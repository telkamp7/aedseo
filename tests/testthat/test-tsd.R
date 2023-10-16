test_that("'tsd' handle implicit missingnes by inserting NA", {
  tsd_day <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-05")),
    time_interval = "day"
  ) %>%
    dplyr::reframe(sumNA = sum(is.na(observed)))
  tsd_week <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15", "2023-01-29")),
    time_interval = "week"
  ) %>%
    dplyr::reframe(sumNA = sum(is.na(observed)))
  tsd_month <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-05-01")),
    time_interval = "month"
  ) %>%
    dplyr::reframe(sumNA = sum(is.na(observed)))
  expect_true(
    tsd_day$sumNA > 0
  )
  expect_true(
    tsd_week$sumNA > 0
  )
  expect_true(
    tsd_month$sumNA > 0
  )
})
test_that("'tsd' does not have gaps", {
  tsd_day <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-05")),
    time_interval = "day"
  )
  tsd_week <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15", "2023-01-29")),
    time_interval = "week"
  )
  tsd_month <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-05-01")),
    time_interval = "month"
  )
  expect_false(
    tsibble::has_gaps(tsd_day)$.gaps
  )
  expect_false(
    tsibble::has_gaps(tsd_week)$.gaps
  )
  expect_false(
    tsibble::has_gaps(tsd_month)$.gaps
  )
})
test_that("'tsd' correctly identifies intervals", {
  tsd_1day <- tsd(
    observed = 10,
    time = as.Date("2023-01-01"),
    time_interval = "day"
  ) %>%
    attr(
      which = "interval"
    )
  tsd_day <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-05")),
    time_interval = "day"
  ) %>%
    attr(
      which = "interval"
    )
  tsd_1week <- tsd(
    observed = 10,
    time = as.Date("2023-01-01"),
    time_interval = "week"
  ) %>%
    attr(
      which = "interval"
    )
  tsd_week <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-01-08", "2023-01-15", "2023-01-29")),
    time_interval = "week"
  ) %>%
    attr(
      which = "interval"
    )
  tsd_1month <- tsd(
    observed = 10,
    time = as.Date("2023-01-01"),
    time_interval = "month"
  ) %>%
    attr(
      which = "interval"
    )
  tsd_month <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-05-01")),
    time_interval = "month"
  ) %>%
    attr(
      which = "interval"
    )
  expect_true(
    tsd_1day == tsibble::new_interval(day = 1)
  )
  expect_true(
    tsd_1week == tsibble::new_interval(week = 1)
  )
  expect_true(
    tsd_1month == tsibble::new_interval(month = 1)
  )
  expect_true(
    tsd_day == tsibble::new_interval(day = 1)
  )
  expect_true(
    tsd_week == tsibble::new_interval(week = 1)
  )
  expect_true(
    tsd_month == tsibble::new_interval(month = 1)
  )
})
