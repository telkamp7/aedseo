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
