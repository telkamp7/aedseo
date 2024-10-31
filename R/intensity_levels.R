#' Compute intensity levels with seasonal time series observations.
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function calculates the intensity levels of time series of observations that are stratified by season.
#' It uses the previous seasons to calculate the intensity levels of the newest season.
#' It can provide two different types of intensity levels, by using one of following functions:
#' - `compute_weighted_intensity_levels()` which calculates the "low", "medium" and "high" intensity levels.
#' - `compute_relative_dist_intensity_levels()` which calculates the "very low", "low", "medium" and "high"
#'    intensity levels with use of the highest intensity level from `compute_weighted_intensity_levels()`
#'    as the high intensity level and the disease_threshold as the very low intensity level.
#'
#' @param tsd A `aedseo_tsd` object containing time series data with 'time' and 'observed'.
#' @param decay_factor A numeric value between 0 and 1, that specifies the weight applied to previous seasons in
#' calculations. It is used as `decay_factor`^(number of seasons back), whereby the weight for the most recent season
#' will be `decay_factor`^0 = 1. This parameter allows for a decreasing weight assigned to prior seasons, such that
#' the influence of older seasons diminishes exponentially.
#' @param disease_threshold An integer specifying the threshold for considering a disease outbreak. It defines the per
#' time-step disease threshold that has to be surpassed for the observation to be included in the calculations.
#' @param n_peak A numeric value specifying the number of peak observations to be selected from each season in the
#' intensity level calculations. The `n_peak` observations have to surpass the `disease_threshold` to be included.
#' @param season_weeks A numeric vector of length 2, `c(start, end)`, with the start and end weeks of the seasons to
#' stratify the observations by. Must span the new year; e.g.: `season_weeks = c(21, 20)`.
#' @param ... arguments that can be passed to the `compute_weighted_intensity_levels()` function.

intensity_levels <- function(
  tsd,
  decay_factor = 0.8,
  disease_threshold = 20,
  n_peak = 6,
  season_weeks = c(21, 20),
  ...
) {
  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(tsd, add = coll)
  checkmate::assert_class(tsd, "aedseo_tsd", add = coll)
  checkmate::assert_names(colnames(tsd), identical.to = c("time", "observed"), add = coll)
  checkmate::assert_numeric(decay_factor, lower = 0, upper = 1, len = 1, add = coll)
  checkmate::assert_numeric(n_peak, lower = 1, len = 1, add = coll)
  checkmate::assert_integerish(disease_threshold, len = 1, add = coll)
  checkmate::assert_integerish(season_weeks, len = 2, lower = 1, upper = 53,
                               null.ok = FALSE, add = coll)
  checkmate::reportAssertions(coll)

  # Add the seasons to data
  seasonal_tsd <- tsd |>
    dplyr::mutate(season = epi_calendar(.data$time, start = season_weeks[1], end = season_weeks[2])) |>
    dplyr::arrange(dplyr::desc(.data$season))

  # Add weights and remove newest season to get predictions for this season
  weighted_seasonal_tsd <- seasonal_tsd |>
    dplyr::filter(.data$season != max(.data$season)) |>
    dplyr::mutate(year = purrr::map_chr(.data$season, ~ stringr::str_extract(.x, "[0-9]+")) |>
                    as.numeric()) |>
    dplyr::mutate(weight = decay_factor^(max(.data$year) - .data$year))

  # Select n_peak highest observations and filter observations >= disease_threshold
  season_observations_and_weights <- weighted_seasonal_tsd |>
    dplyr::select(-c("year", "time")) |>
    dplyr::filter(.data$observed >= disease_threshold) |>
    dplyr::slice_max(.data$observed, n = n_peak, with_ties = FALSE, by = "season")

  # Run `compute_weighted_intensity_levels` on data to get peak intensity levels
  computed_peak_levels <- compute_weighted_intensity_levels(weighted_observations = season_observations_and_weights |>
                                                              dplyr::select(.data$observed, .data$weight), ...)

  # Select the newest season for intensity level output
  season_peak_levels <- computed_peak_levels |>
    dplyr::mutate(season = max(seasonal_tsd$season)) |>
    dplyr::select(.data$season, dplyr::everything())

  return(season_peak_levels)
}
