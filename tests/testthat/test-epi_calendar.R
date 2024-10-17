# Test if a date within the season returns the correct season
test_that("Within the season, correct season is returned", {
  expect_equal(
    epi_calendar(as.Date("2023-03-15"), start = 21, end = 20), "2022/2023"
  )
  expect_equal(
    epi_calendar(as.Date("2023-05-01"), start = 21, end = 20), "2022/2023"
  )
  expect_equal(
    epi_calendar(as.Date("2023-01-15"), start = 21, end = 20), "2022/2023"
  )
  expect_equal(
    epi_calendar(as.Date("2023-12-01"), start = 21, end = 20), "2023/2024"
  )
})

# Test if a date outside the season returns "out_of_season"
test_that("Outside the season, 'out_of_season' is returned", {
  expect_equal(
    epi_calendar(as.Date("2023-06-01"), start = 40, end = 20), "out_of_season"
  )
  expect_equal(
    epi_calendar(as.Date("2023-09-15"), start = 40, end = 20), "out_of_season"
  )
  expect_equal(
    epi_calendar(as.Date("2023-06-30"), start = 40, end = 20), "out_of_season"
  )
})

# Test if algorithm returns out_of_season when start:end only spand one year
test_that("Within one year, out_of_season is returned", {
  expect_equal(
    epi_calendar(as.Date("2023-03-15"), start = 2, end = 5), "out_of_season"
  )
  expect_equal(
    epi_calendar(as.Date("2023-05-01"), start = 1, end = 53), "out_of_season"
  )
  expect_equal(
    epi_calendar(as.Date("2023-01-15"), start = 30, end = 40), "out_of_season"
  )
  expect_equal(
    epi_calendar(as.Date("2023-12-01"), start = 3, end = 20), "out_of_season"
  )
})

# Test that boundary weeks are included if end < start
test_that("Within the season, boundary weeks are included if end < start", {
  expect_equal(
    epi_calendar(as.Date("2023-03-15"), start = 40, end = 11), "2022/2023"
  )
  expect_equal(
    epi_calendar(as.Date("2024-09-30"), start = 40, end = 11), "2024/2025"
  )
})

# Test that all dates from week 53 belongs to correct season 2015/2016
test_that("All dates from week 53 belongs to correct season 2015/2016", {
  week_53_season_15_16 <- c("2015-12-28", "2015-12-29", "2015-12-30",
                            "2015-12-31", "2016-01-01", "2016-01-02",
                            "2016-01-03")
  results <- purrr::map(week_53_season_15_16,
                        ~ epi_calendar(as.Date(.x), start = 21, end = 20))
  results_vector <- unlist(results)

  expect_equal(results_vector, rep("2015/2016", length(week_53_season_15_16)))
})
