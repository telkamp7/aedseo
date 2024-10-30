# aedseo (development version)

## Features

* Added the `intensity_levels()` function, which currently calculates peak intensity levels with the `compute_weighted_intensity_levels()` function for the newest season in input tsd data. Soon the `compute_relative_dist_intensity_levels()` function will be an option to calculate intensity levels in the `intensity_levels()` function (#36).

* Added the `compute_weighted_intensity_levels()` function, which optimises a user selected distribution and calculates the intensity levels based on observations and weights. It is meant to be used within the soon coming `intensity_level()` function (#35).

## Improvements

* Improved the `epi_calendar()` function to work for a season spanning new year (#34).

* The `aedseo()` function now allows for the choice of adding season as an input argument (#34).

* `{checkmate}` assertions have been added to enhance user feedback with clearer error messages and to ensure functions operate correctly by validating inputs (#33).

* Improved the `aedseo()` function to work with `NA` values. The user now defines how many `NA` values the function should allow in each window (#32).

## Minor changes

* The `disease_threshold` argument now reflects the disease threshold in one time step. If the total number of cases in a window of size `k` exceeds  `disease_threshold * k`, a seasonal onset alarm can be triggered (#32).

# aedseo 0.1.2

## Minor changes

* Transferring maintainership of the R package to Lasse Engbo Christiansen.

# aedseo 0.1.1

## Improvements

* Enhanced clarity and user guidance in the introductory vignette, providing a more comprehensive walkthrough of the application of the 'aeddo' algorithm on time series data with detailed explanations and illustrative examples.

## Minor changes

* Updated LICENSE.md to have Statens Serum Institut as a copyright holder.

* Fixed installation guide for the development version in the README.Rmd and README.md

* Added Lasse Engbo Christiansen as an author of the R package.

* Added a new function `epi_calendar()` that determines the epidemiological season based on a given date, allowing users to easily categorize dates within or outside specified seasons.

* Introduced additional visualizations in the `autoplot()` method, enhancing the capabilities of the `plot()` method with new displays of observed cases and growth rates.

# aedseo 0.1.0

## Features

- Added the `aedseo` function, which automates the early detection of seasonal epidemic onsets by estimating growth rates for consecutive time intervals and calculating the Sum of Cases (sum_of_cases).

- Introduced `autoplot` and `plot` methods for visualizing `aedseo` and `aedseo_tsd` objects. These functions allow you to create insightful ggplot2 plots for your data.

- Included the `fit_growth_rate` function, enabling users to fit growth rate models to time series observations.

- Introduced the `predict` method for `aedseo` objects, which allows you to predict observations for future time steps given
the growth rates.

- Added the `summary` method for `aedseo` objects, providing a comprehensive summary of the results.

- Introduced the `tsd` function, allowing users to create S3 `aedseo_tsd` (time-series data) objects from observed data and corresponding dates.
