#' Determine Epidemiological Season
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function identifies the epidemiological season, (spanning two years)
#' to which a given date belongs.
#' The epidemiological season is defined by a start and end week, where weeks
#' are numbered according to the ISO week date system.
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
#' epi_calendar(as.Date("2023-09-15"), start = 21, end = 20)
#' # Expected output: "2023/2024"
#'
#' epi_calendar(as.Date("2023-05-30"), start = 40, end = 20)
#' # Expected output: "out_of_season"
#'
#' epi_calendar(as.Date("2023-01-15"), start = 1, end = 40)
#' # Expected output: "out_of_season"
#'
#' epi_calendar(as.Date("2023-10-06"), start = 40, end = 11)
#' # Expected output: "2023/2024"
epi_calendar <- Vectorize(function(date, start = 21, end = 20) {
  # Compute the current week
  current_week <- as.integer(format(x = date, "%V"))

  # Season is contained to a single year
  if (start <= end && (dplyr::between(current_week, start, end) |
                         !dplyr::between(current_week, start, end))) {
    return("out_of_season")
    # Season spans new-year
  } else if (end < start && dplyr::between(current_week, end + 1, start - 1)) {
    return("out_of_season")
  }

  # Compute the current year
  current_year <- as.integer(strftime(date, format = "%G"))

  if (current_week <= end) {
    ans <- paste0(current_year - 1, "/", current_year)
  } else {
    ans <- paste0(current_year, "/", current_year + 1)
  }

  return(ans)
})
