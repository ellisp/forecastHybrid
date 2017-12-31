## ----cran_install, eval = FALSE------------------------------------------
#  install.packages("forecastHybrid")

## ----github_install, eval = FALSE----------------------------------------
#  devtools::install_github("ellisp/forecastHybrid/pkg")

## ----load_library, message = FALSE---------------------------------------
library(forecastHybrid)

## ---- cvts_comparison, cache=TRUE----------------------------------------
stlmMod <- cvts(woolyrnq, FUN = stlm, windowSize = 100)
nnetarMod <- cvts(woolyrnq, FUN = nnetar, windowSize = 100)
accuracy(stlmMod)
accuracy(nnetarMod)

## ---- custom_cvts, cache=TRUE--------------------------------------------
library(GMDH)
GMDHForecast <- function(x, h){
  fc <- GMDH::fcast(x, f.number = h)
  # GMDH doesn't produce a ts object with correct attributes, so we build it
  end <- tsp(x)[2]
  freq <- frequency(x)
  # Set the correct start, end, and frequency for the ts forecast object
  tsProperties <- c(end + 1 / freq, end + h / freq, freq)
  tsp(fc$mean) <- tsProperties
  tsp(fc$upper) <- tsProperties
  tsp(fc$lower) <- tsProperties
  class(fc) <- "forecast"
  return(fc)
}
series <- subset(woolyrnq, end = 12)
gmdhcv <- cvts(series, FCFUN = GMDHForecast, windowSize = 10, maxHorizon = 1)

