#' Determine Epidemiological Season
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function identifies the epidemiological season to which a given date
#' belongs.
#' The epidemiological season is defined by a start and end week, where weeks
#' are numbered
#' according to the ISO week date system.
#'
#' @param date A date object representing the date to check.
#' @param start An integer specifying the start week of the epidemiological
#' season.
#' @param end An integer specifying the end week of the epidemiological season.
#'
#' @return A character vector indicating the season:
#'   - "out_of_season" if the date is outside the specified season,
#'   - If within the season, the function returns a character string indicating
#'   the epidemiological season.
#'
#' @export
#'
#' @examples
#' # Check if a date is within the epidemiological season
#' epi_calendar(as.Date("2023-09-15"), start = 40, end = 20)
#' # Expected output: "2022/2023"
#'
#' epi_calendar(as.Date("2023-05-01"), start = 40, end = 20)
#' # Expected output: "out_of_season"
#'
#' epi_calendar(as.Date("2023-01-15"), start = 40, end = 20)
#' # Expected output: "2022/2023"
#'
#' epi_calendar(as.Date("2023-12-01"), start = 40, end = 20)
#' # Expected output: "2023/2024"
epi_calendar <- Vectorize(function(date, start = 40, end = 20) {
  # Compute the current week
  current_week <- as.integer(format(x = date, "%U"))

  if (current_week <= start & end <= current_week) {
    return("out_of_season")
  }

  # Compute the current year
  current_year <- format(date, "%Y")
  # ... and turn into integer
  current_year_integer <- as.integer(current_year)

  if (current_week <= end) {
    ans <- paste0(current_year_integer - 1, "/", current_year_integer)
  } else {
    ans <- paste0(current_year_integer, "/", current_year_integer + 1)
  }

  return(ans)
})
