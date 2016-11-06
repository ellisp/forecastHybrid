[![Travis-CI Build Status](https://travis-ci.org/ellisp/forecastHybrid.svg?branch=master)](https://travis-ci.org/ellisp/forecastHybrid)
[![Coverage Status](https://coveralls.io/repos/github/ellisp/forecastHybrid/badge.svg?branch=master)](https://coveralls.io/github/ellisp/forecastHybrid?branch=master)
[![CRAN version](http://www.r-pkg.org/badges/version/forecastHybrid)](http://www.r-pkg.org/pkg/forecastHybrid)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/forecastHybrid)](http://www.r-pkg.org/pkg/forecastHybrid)

# forecastHybrid
Convenient functions for ensemble forecasts in R combining approaches from the [forecast](https://github.com/robjhyndman/forecast) package

For a more detailed description of the package and usage, consult the [vignette](https://cran.r-project.org/web/packages/forecastHybrid/vignettes/forecastHybrid.html).

The package is still under heavy development, but many basic features have been implemented. Some features (such as optimized parallelization between rather than within models, cross validation for determing model error rates, and automatically selecting the optimal combination of base models) have not yet been developed.


## Installation
The stable release of the package is hosted on [CRAN](https://cran.r-project.org/web/packages/forecastHybrid/index.html) and can be installed as usual:
````r
install.packages("forecastHybrid")
````

The latest development version can be installed using the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package.



```r
devtools::install_github("ellisp/forecastHybrid/pkg")
```
Version updates to CRAN will be published frequently after new features are implemented, so the development version is not recommended unless you plan to modify the code.

## Usage


```r
library(forecastHybrid)
 
# Build a hybrid forecast on the wineind dataset using auto.arima, ets, and tbats models.
# Each model is given equal weight
hm1 <- hybridModel(wineind, models = "aet", weights = "equal")
```

```
## Fitting the auto.arima model
## Fitting the ets model
## Fitting the tbats model
```

```r
# Now plot the forecast for the next 48 periods
plot(forecast(hm1, h = 48))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
# Build the ensemble model on the same data but this time use auto.arima, nnetar, stlm, and tbats models.
hm2 <- hybridModel(wineind, models = "anst", weights = "equal")
```

```
## Fitting the auto.arima model
## Fitting the nnetar model
## Fitting the stlm model
## Fitting the tbats model
```

```r
# Now plot the forecast for the next 48 periods
plot(forecast(hm2, h = 48))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-2.png)

```r
# Extract the point forecasts from this model
fc <- forecast(hm2, h = 48)
fc$mean
```

```
##           Jan      Feb      Mar      Apr      May      Jun      Jul
## 1994                                                               
## 1995 16720.19 20510.73 23942.53 24524.77 23989.32 24461.21 29362.26
## 1996 16306.55 20716.57 23929.71 25067.83 24189.99 24385.75 29472.49
## 1997 15838.69 20398.61 23551.80 25063.00 24027.20 24478.94 29405.79
## 1998 15518.45 20343.42 23412.92 24642.51 23939.90 24512.96 29224.08
##           Aug      Sep      Oct      Nov      Dec
## 1994          25346.72 26194.46 31832.46 35272.60
## 1995 26926.20 25092.87 26120.77 31440.40 36271.90
## 1996 26375.44 24686.76 26071.69 31654.00 36466.92
## 1997 26570.11 24414.52 26043.36 31603.25 36362.85
## 1998 26309.60
```

```r
# Extract the (default) upper 80% and 95% prediction intervals
fc$upper
```

```
##            80%      95%
##  [1,] 28787.15 30519.50
##  [2,] 30475.49 32052.98
##  [3,] 36277.30 38475.03
##  [4,] 40369.80 41955.43
##  [5,] 20128.66 21356.34
##  [6,] 24420.61 26024.45
##  [7,] 27275.07 28949.58
##  [8,] 28878.89 30503.16
##  [9,] 27202.87 28837.47
## [10,] 28331.57 29976.48
## [11,] 32902.53 34557.70
## [12,] 29897.88 31763.53
## [13,] 28248.31 30062.69
## [14,] 30773.89 32595.66
## [15,] 35453.14 37287.12
## [16,] 40866.07 42714.60
## [17,] 19605.19 21469.25
## [18,] 24971.92 26851.88
## [19,] 27486.70 29382.63
## [20,] 29457.39 31369.24
## [21,] 27792.47 29720.15
## [22,] 28931.70 30875.09
## [23,] 33512.87 35471.87
## [24,] 30140.86 32115.33
## [25,] 28869.58 30993.51
## [26,] 31403.97 33539.96
## [27,] 36093.19 38246.66
## [28,] 41516.39 43689.85
## [29,] 20265.81 22460.26
## [30,] 25642.77 27858.52
## [31,] 28167.67 30404.75
## [32,] 30148.37 32406.66
## [33,] 28493.33 30772.68
## [34,] 29642.31 31942.56
## [35,] 34233.13 36554.08
## [36,] 30870.66 33212.13
## [37,] 29596.42 32085.79
## [38,] 32139.67 34645.79
## [39,] 36838.33 39366.93
## [40,] 42271.07 44824.70
## [41,] 21029.97 23609.61
## [42,] 26416.32 29022.24
## [43,] 28950.51 31582.68
## [44,] 30940.39 33598.63
## [45,] 29294.43 31978.54
## [46,] 30452.40 33162.14
## [47,] 35052.10 37787.25
## [48,] 31698.41 34458.73
```

```r
# Extract the (default) lower 80% and 95% prediction intervals
fc$lower
```

```
##            80%       95%
##  [1,] 22270.74 20694.346
##  [2,] 22902.75 21598.537
##  [3,] 29048.62 27389.280
##  [4,] 31109.96 29327.301
##  [5,] 13051.76 11457.589
##  [6,] 17863.48 16757.327
##  [7,] 20822.76 19208.789
##  [8,] 20909.59 19696.301
##  [9,] 21027.19 19392.591
## [10,] 20254.10 19071.600
## [11,] 25891.51 24375.319
## [12,] 23228.55 21563.174
## [13,] 21393.44 19579.060
## [14,] 21816.55 20527.275
## [15,] 27740.78 26096.665
## [16,] 29780.03 28009.915
## [17,] 12562.57 10698.506
## [18,] 17174.51 15989.302
## [19,] 20323.72 18427.796
## [20,] 20181.36 18967.695
## [21,] 20509.53 18581.853
## [22,] 19616.42 18429.956
## [23,] 25116.13 23592.762
## [24,] 22681.14 20706.670
## [25,] 20845.16 18721.224
## [26,] 21253.10 19953.022
## [27,] 27058.33 25398.686
## [28,] 29081.91 27293.298
## [29,] 11974.98  9780.527
## [30,] 16808.42 15055.683
## [31,] 19715.78 17478.701
## [32,] 19789.54 18559.089
## [33,] 19881.71 17602.349
## [34,] 19268.84 18064.344
## [35,] 24690.68 23143.212
## [36,] 22024.37 19682.903
## [37,] 20191.35 17701.976
## [38,] 20937.45 19614.730
## [39,] 26673.26 24756.450
## [40,] 28685.08 26863.729
## [41,] 11283.85  8704.202
## [42,] 16570.92 13964.995
## [43,] 19005.97 16373.807
## [44,] 19559.85 18239.075
## [45,] 19153.63 16469.523
## [46,] 19061.74 17504.990
## [47,] 24435.04 21983.305
## [48,] 21269.65 18509.331
```

```r
# Produce a forecast with prediction intervals at the 70%, 80%, 90%, and 95% levels
fc2 <- forecast(hm2, h = 48, level = c(70, 80, 90, 95))
```

```
## Error in forecasts[[j]]$upper[, i]: subscript out of bounds
```

