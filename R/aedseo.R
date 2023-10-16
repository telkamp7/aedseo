
aedseo <- function(
    tsd,
    k = 5,
    level = 0.95,
    family = c(
      "poisson",
      "quasipoisson",
      "negative.binomial"))
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
        SoC = SoC
      )
    )

  }

  return(res)

}
