## ----cran_install, eval = FALSE------------------------------------------
#  install.packages("forecastHybrid")

## ----github_install, eval = FALSE----------------------------------------
#  devtools::install_github("ellisp/forecastHybrid/pkg")

## ----load_library, message = FALSE---------------------------------------
library(forecastHybrid)

## ---- cvts_comparison, cache=TRUE----------------------------------------
stlmMod <- cvts(woolyrnq, FUN = stlm, windowSize = 100)
naiveMod <- cvts(woolyrnq, FUN = naive, windowSize = 100)
accuracy(stlmMod)
accuracy(naiveMod)

