#' Automated an Early Detection of Seasonal Epidemic Onset (aedseo)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function performs automated and early detection of seasonal epidemic onsets (aedseo) on a time series dataset. It estimates growth rates for consecutive time intervals and calculates the Sum of Cases (SoC).
#'
#' @param tsd A tsibble object containing time series data with 'time,' 'observed,' and 'periodInYear.'
#' @param k An integer specifying the window size for modeling growth rates.
#' @param level The confidence level for parameter estimates, a numeric value between 0 and 1.
#' @param family A character string specifying the family for modeling. Choose between "poisson," or "quasipoisson".
#'
#' @return A tibble containing:
#'   - 'reference_time': The time point for which the growth rate is estimated.
#'   - 'growth_rate': The estimated growth rate.
#'   - 'lower_growth_rate': The lower bound of the growth rate's confidence interval.
#'   - 'upper_growth_rate': The upper bound of the growth rate's confidence interval.
#'   - 'SoC': The Sum of Cases within the time window.
#'   - 'converged': Logical. Was the IWLS judged to have converged?
#'
#' @export
#'
#' @examples
#' # Create a tsibble object from sample data
#' tsd_data <- tsd(
#'   observed = c(100, 120, 150, 180, 220, 270),
#'   time = as.Date(c(
#'     "2023-01-01",
#'     "2023-01-02",
#'     "2023-01-03",
#'     "2023-01-04",
#'     "2023-01-05",
#'     "2023-01-06")
#'     ),
#'     time_interval = "day"
#'   )
#'
#' # Calculate AEDSEO with a 3-day window and a Poisson family model
#' aedseo_results <- aedseo(
#'   tsd = tsd_data,
#'   k = 3,
#'   level = 0.95,
#'   family = "poisson"
#'   )
#'
#' # Print the AEDSEO results
#' print(aedseo_results)
#'
aedseo <- function(
    tsd,
    k = 5,
    level = 0.95,
    family = c(
      "poisson",
      "quasipoisson"))
  {

  # Extract the length of the series
  n <- base::nrow(tsd)

  # Allocate space for growth rate estimates
  res <- tibble::tibble()

  for(i in k:n){

    # Index observations for this iteration
    obs_iter <- tsd[(i-k+1):i, ]

    # Calculate growth rates
    growth_rates <- fit_growth_rate(observations = obs_iter$observed, level = level, family = family)

    # Calculate Sum of Cases (SoC)
    SoC <- base::sum(obs_iter$observed)

    # Collect the results
    res <- dplyr::bind_rows(
      res,
      tibble::tibble(
        reference_time = tsd$time[i],
        growth_rate = growth_rates$estimate[1],
        lower_growth_rate =  growth_rates$estimate[2],
        upper_growth_rate =  growth_rates$estimate[3],
        SoC = SoC,
        converged = growth_rates$fit$converged
      )
    )

  }

  return(res)

}
