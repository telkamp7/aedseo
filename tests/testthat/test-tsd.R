test_that("Can correctly make an 'aedseo_tsd' class object", {
  tsd_day <- tsd(
    observed = c(10, 15, 20, 18),
    time = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-05")),
    time_interval = "day"
  )

  expect_s3_class(object = tsd_day, class = "aedseo_tsd")
})
