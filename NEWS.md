# aedseo 0.1.0

## Features

- Added the `aedseo` function, which automates the early detection of seasonal epidemic onsets by estimating growth rates for consecutive time intervals and calculating the Sum of Cases (sum_of_cases).

- Introduced `autoplot` and `plot` methods for visualizing `aedseo` and `aedseo_tsd` objects. These functions allow you to create insightful ggplot2 plots for your data.

- Included the `fit_growth_rate` function, enabling users to fit growth rate models to time series observations.

- Introduced the `predict` method for `aedseo` objects, which allows you to predict observations for future time steps given 
the growth rates.

- Added the `summary` method for `aedseo` objects, providing a comprehensive summary of the results.

- Introduced the `tsd` function, allowing users to create S3 `aedseo_tsd` (time-series data) objects from observed data and corresponding dates.

Please note that this is the initial CRAN release of the aedseo package, and we look forward to adding more features and improvements in future updates. We appreciate your feedback and contributions to make this package even more useful for early detection of seasonal epidemics.
