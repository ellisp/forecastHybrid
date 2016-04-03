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
library(forecastHybrid)

# Build a hybrid forecast on the wineind dataset using auto.arima, ets, and tbats models.
# Each model is given equal weight
hm1 <- hybridModel(wineind, models = "aet", weights = "equal")

# Now plot the forecast for the next 48 periods
plot(forecast(hm1, h = 48))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
# Build the ensemble model on the same data but this time use auto.arima, nnetar, stlm, and tbats models.
hm2 <- hybridModel(wineind, models = "anst", weights = "equal")

# Now plot the forecast for the next 48 periods
plot(forecast(hm2, h = 48))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png)

```r
# Extract the point forecasts from this model
fc <- forecast(hm2, h = 48)
fc$mean
```

```
##           Jan      Feb      Mar      Apr      May      Jun      Jul
## 1994                                                               
## 1995 16314.22 20579.82 24236.96 24963.26 23956.00 24646.35 29694.81
## 1996 15987.63 20537.90 24275.44 25476.30 24083.47 24604.68 29454.04
## 1997 16148.94 20558.54 23874.15 25339.74 24050.26 24581.51 29448.74
## 1998 16104.00 20690.21 23661.18 25204.17 24131.59 24679.95 29446.75
##           Aug      Sep      Oct      Nov      Dec
## 1994          25189.20 26684.39 32267.44 36041.74
## 1995 26969.64 24584.16 26533.88 32298.78 36717.84
## 1996 26366.02 24841.99 26429.56 32179.22 36733.04
## 1997 26744.37 24719.89 26401.08 32112.14 36859.22
## 1998 26555.04
```

```r
# Extract the (default) upper 80% and 95% prediction intervals
fc$upper
```

```
##            80%      95%
##  [1,] 28444.48 30243.21
##  [2,] 30438.68 31996.70
##  [3,] 37177.01 39527.19
##  [4,] 40332.81 41898.85
##  [5,] 20563.58 21865.30
##  [6,] 24383.20 25967.23
##  [7,] 27785.42 29543.28
##  [8,] 28841.00 30445.21
##  [9,] 27950.51 29719.39
## [10,] 28293.19 29917.79
## [11,] 33014.25 35104.35
## [12,] 30691.15 32634.73
## [13,] 28333.56 30131.85
## [14,] 30731.39 32530.66
## [15,] 36955.46 39302.75
## [16,] 40822.94 42648.64
## [17,] 20476.18 21778.87
## [18,] 24928.06 26784.80
## [19,] 27703.42 29465.65
## [20,] 29412.79 31301.03
## [21,] 27895.55 29671.53
## [22,] 28886.36 30805.75
## [23,] 33467.17 35401.97
## [24,] 30662.60 32617.33
## [25,] 28820.03 30917.73
## [26,] 31354.13 33463.75
## [27,] 36946.72 39310.80
## [28,] 41465.69 43612.30
## [29,] 20477.34 22381.96
## [30,] 25591.07 27779.47
## [31,] 28115.48 30324.94
## [32,] 30095.68 32326.09
## [33,] 28440.15 30691.36
## [34,] 29588.65 31860.48
## [35,] 34178.99 36471.27
## [36,] 30816.03 33128.59
## [37,] 29538.34 31996.97
## [38,] 32081.20 34556.37
## [39,] 36973.47 39357.99
## [40,] 42211.49 44733.59
## [41,] 20969.79 23517.57
## [42,] 26355.52 28929.26
## [43,] 28889.11 31488.76
## [44,] 30878.37 33503.79
## [45,] 29231.81 31882.77
## [46,] 30389.18 33065.46
## [47,] 34988.29 37689.66
## [48,] 31634.01 34360.24
```

```r
# Extract the (default) lower 80% and 95% prediction intervals
fc$lower
```

```
##            80%       95%
##  [1,] 22307.51 20750.590
##  [2,] 22632.39 21285.419
##  [3,] 29091.03 27531.036
##  [4,] 31041.85 29195.101
##  [5,] 13088.95 11514.468
##  [6,] 18096.21 16814.551
##  [7,] 20860.41 19266.375
##  [8,] 21494.53 20214.427
##  [9,] 21065.33 19450.912
## [10,] 20852.88 19610.417
## [11,] 26178.87 24619.333
## [12,] 23267.41 21622.594
## [13,] 21435.76 19643.795
## [14,] 22449.75 21107.724
## [15,] 28566.98 26755.654
## [16,] 30852.37 29008.892
## [17,] 12606.06 10765.015
## [18,] 17913.12 16056.378
## [19,] 20367.95 18495.442
## [20,] 21414.49 20132.116
## [21,] 20554.50 18650.631
## [22,] 20790.78 19544.673
## [23,] 26108.57 24222.526
## [24,] 22727.20 20777.118
## [25,] 20894.71 18797.005
## [26,] 22403.32 21054.868
## [27,] 28007.41 25880.530
## [28,] 30798.17 28945.082
## [29,] 12026.17  9858.824
## [30,] 17323.13 15134.740
## [31,] 19767.97 17558.519
## [32,] 21385.04 19438.590
## [33,] 19934.88 17683.675
## [34,] 20764.70 18733.619
## [35,] 25518.54 23226.253
## [36,] 22079.00 19766.445
## [37,] 20249.42 17790.795
## [38,] 22378.62 20254.605
## [39,] 27344.04 24846.669
## [40,] 30765.79 28900.863
## [41,] 11344.03  8796.242
## [42,] 16631.71 14057.973
## [43,] 19067.38 16467.721
## [44,] 20959.33 18333.920
## [45,] 19216.25 16565.290
## [46,] 20277.95 17601.672
## [47,] 24782.26 22080.894
## [48,] 21334.05 18607.818
```

```r
# Produce a forecast with prediction interavals at the 70%, 80%, 90%, and 95% levels
fc2 <- forecast(hm2, h = 48, level = c(70, 80, 90, 95))
```

