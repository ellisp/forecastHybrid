[![Travis-CI Build Status](https://travis-ci.org/ellisp/forecastHybrid.svg?branch=master)](https://travis-ci.org/ellisp/forecastHybrid)

# forecastHybrid
Convenient functions for ensemble forecasts in R combining approaches from the [forecast](https://github.com/robjhyndman/forecast) package

The package is still under heavy development, but many basic features have been implemented. Some features (such as optimized parallelization between rather than within models, cross validation for determing model error rates, and automatically selecting the optimal combination of base models) have not yet been developed.

## Installation

The latest development version can be installed using the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package.
```r
devtools::install_github("ellisp/forecastHybrid/pkg")
```
Stable releases on CRAN are planned.


## Usage


```r
# Build a hybrid forecast on the wineind dataset using auto.arima, ets, and tbats models.
# Each model is given equal weight
hm1 <- hybridModel(wineind, models = "aet", weights = "equal")

# Now plot the forecast for the next 48 periods
plot(forecast(hm1, h = 48))

# Build the ensemble model on the same data but this time use auto.arima, nnetar, stlm, and tbats models.
hm1 <- hybridModel(wineind, models = "anst", weights = "equal")

# Now plot the forecast for the next 48 periods
plot(forecast(hm2, h = 48)

# Extract the point forecasts from this model
fc <- forecast(hm2, h = 48)
fc$mean

```

