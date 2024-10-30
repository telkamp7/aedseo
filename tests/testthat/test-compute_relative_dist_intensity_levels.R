test_that("Test if checkmate checks work", {

  # Create data
  obs <- 10
  season <- c("2018/2019", "2019/2020", "2020/2021")
  season_num_rev <- rev(seq(from = 1, to = length(season)))
  observations <- rep(stats::rnorm(obs, 1000), length(season))

  peak_input <- tibble::tibble(
    observed = observations,
    weight = 0.8^rep(season_num_rev, each = obs)
  )

  compute_relative_dist_intensity_levels(weighted_observations = peak_input)

})
