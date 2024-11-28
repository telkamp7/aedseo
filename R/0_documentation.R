rd_disease_threshold <- function(type = "param", usage = NULL) {
  checkmate::assert_choice(type, "param")
  paste("An integer specifying the threshold for considering a disease outbreak.",
        if (usage == "onset") {
          paste("It defines the per time-step disease threshold that has to be surpassed to possibly trigger a seasonal
          onset alarm. If the total number of cases in a window of size k exceeds `disease_threshold * k`, a seasonal
          onset alarm can be triggered.")
        } else if (usage == "levels") {
          paste("It defines the per time-step disease threshold that has to be surpassed for the observation to be
          included in the level calculations.")
        } else if (usage == "combined") {
          paste("For seasonal onset it defines the per time-step disease threshold that has to be surpassed to possibly
          trigger a seasonal onset alarm. If the total number of cases in a window of size k exceeds
          `disease_threshold * k`, a seasonal onset alarm can be triggered. For burden levels it defines the per
          time-step disease threshold that has to be surpassed for the observation to be included in the level
          calculations.")
        })
}
rd_family <- function(type = "param", usage = NULL) {
  checkmate::assert_choice(type, "param")
  paste("A character string specifying the family for modeling",
        ifelse(usage == "combined", paste(" seasonal onset.")))
}
rd_season_weeks <- function(type = "param", usage = NULL) {
  checkmate::assert_choice(type, "param")
  paste("A numeric vector of length 2, `c(start,end)`, with the start and end weeks of the seasons to
  stratify the observations by. Must span the new year; ex: `season_weeks = c(21, 20)`.",
        ifelse(usage == "onset", paste("If set to `NULL`, is  means no stratification by season.")))
}
rd_tsd <- function(type = "param") {
  checkmate::assert_choice(type, "param")
  paste("An object containing time series data with 'time' and 'observation.'")
}
