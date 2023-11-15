# Test if a date within the season returns the correct season
test_that("Within the season, correct season is returned", {
  expect_equal(
    epi_calendar(as.Date("2023-03-15"), start = 40, end = 20), "2022/2023"
  )
  expect_equal(
    epi_calendar(as.Date("2023-05-01"), start = 40, end = 20), "2022/2023"
  )
  expect_equal(
    epi_calendar(as.Date("2023-01-15"), start = 40, end = 20), "2022/2023"
  )
  expect_equal(
    epi_calendar(as.Date("2023-12-01"), start = 40, end = 20), "2023/2024"
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
