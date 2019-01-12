library(Mcomp)
library(pbapply)
library(forecastHybrid)
library(thief)

data(M1)

compfc <- function(x, FUN){
  series <- x$x
  future <- x$xx
  h <- x$h
  fc <- forecast(FUN(series), h = h)
  acc <- accuracy(f = fc, x = future)["Test set", "MASE"]
  return(acc)
}

compfc_simple <- function(x, FUN){
  series <- x$x
  future <- x$xx
  h <- x$h
  fc <- FUN(series, h = h)
  acc <- accuracy(f = fc, x = future)["Test set", "MASE"]
  return(acc)
}

thief_fc_simple <- function(x, FUN){
  series <- x$x
  future <- x$xx
  h <- x$h
  fc <- thief(series, h = h, forecastfunction = FUN)
  acc <- accuracy(f = fc, x = future)["Test set", "MASE"]
  return(acc)
}

thief_fc <- function(x, FUN){
  series <- x$x
  future <- x$xx
  h <- x$h
  fc <- thief(series, h = h, forecastfunction = function(x1, h1) forecast(FUN(x1), h = h1))
  acc <- accuracy(f = fc, x = future)["Test set", "MASE"]
  return(acc)
}


numCores <- 4


####################################################################################################
# All
####################################################################################################


# M1 accuracy
arima_acc <- unlist(pblapply(M1, FUN = function(x) compfc(x, auto.arima), cl = numCores))
ets_acc <- unlist(pblapply(M1, FUN = function(x) compfc(x, ets), cl = numCores))
theta_acc <- unlist(pblapply(M1, FUN = function(x) compfc(x, thetam), cl = numCores))
nnetar_acc <- unlist(pblapply(M1, FUN = function(x) compfc(x, nnetar), cl = numCores))
tbats_acc <- unlist(pblapply(M1, FUN = function(x) compfc(x, tbats), cl = numCores))
snaive_acc <- unlist(pblapply(M1, FUN = function(x) compfc_simple(x, snaive), cl = numCores))
hm_acc <- unlist(pblapply(M1, FUN = function(x) compfc(x, function(x1, h1) forecast(hybridModel(x1, "aefnt", verbose = FALSE), PI = FALSE)), cl = numCores))

m1_df <- data.frame(arima_acc, ets_acc, theta_acc, nnetar_acc, tbats_acc, snaive_acc, hm_acc)


####################################################################################################
# Nonyearly
####################################################################################################

M1_nonyearly <- Filter(function(x) x$period != "YEARLY", M1)

arima_acc <- unlist(pblapply(M1_nonyearly, FUN = function(x) compfc(x, auto.arima), cl = numCores))
ets_acc <- unlist(pblapply(M1_nonyearly,
                           FUN = function(x) compfc(x, ets), cl = numCores))
theta_acc <- unlist(pblapply(M1_nonyearly,
                             FUN = function(x) compfc(x, thetam), cl = numCores))
nnetar_acc <- unlist(pblapply(M1_nonyearly,
                              FUN = function(x) compfc(x, nnetar), cl = numCores))
tbats_acc <- unlist(pblapply(M1_nonyearly,
                             FUN = function(x) compfc(x, tbats), cl = numCores))
snaive_acc <- unlist(pblapply(M1_nonyearly,
                              FUN = function(x) compfc_simple(x, snaive), cl = numCores))
hm_acc <- unlist(pblapply(M1_nonyearly,
                          FUN = function(x) compfc(x, function(x1, h1) forecast(hybridModel(x1, "aefnt", verbose = FALSE), PI = FALSE)), cl = numCores))
# m4 entry
hm_small <- unlist(pblapply(M1_nonyearly,
                            FUN = function(x) compfc(x, function(x1, h1) forecast(hybridModel(x1, "aft", verbose = FALSE), PI = FALSE)), cl = numCores))

m1_nonyearly_df <- data.frame(arima_acc, ets_acc, theta_acc, nnetar_acc, tbats_acc, snaive_acc, hm_acc, hm_small)
save(m1_nonyearly_df, file = "m1_nonyearly_df.RData")

# 

thief_arima <- unlist(pblapply(M1_nonyearly,
                               FUN = function(x) thief_fc(x, auto.arima), cl = numCores))
thief_ets <- unlist(pblapply(M1_nonyearly,
                             FUN = function(x) thief_fc(x, ets), cl = numCores))
thief_theta <- unlist(pblapply(M1_nonyearly,
                               FUN = function(x) thief_fc_simple(x, thetaf), cl = numCores))
#~ thief_nnetar <- unlist(pblapply(M1_nonyearly,
#~                                 FUN = function(x) thief_fc(x, nnetar), cl = numCores))
#~ thief_tbats <- unlist(pblapply(M1_nonyearly,
#~                                FUN = function(x) thief_fc(x, tbats), cl = numCores))
thief_snaive <- unlist(pblapply(M1_nonyearly,
                                FUN = function(x) thief_fc_simple(x, snaive), cl = numCores))
thief_hm <- unlist(pblapply(M1_nonyearly,
                            FUN = function(x) thief_fc_simple(x, function(x1, h1) forecast(hybridModel(x1, "aef", verbose = FALSE), h = h1, PI = FALSE)), cl = numCores))
hm_thief <- unlist(pblapply(M1_nonyearly,
                            FUN = function(x) compfc_simple(x, function(x1, h1) thiefModel(x1, "aef")), cl = numCores))

thief_df <- data.frame(thief_arima, thief_ets, thief_theta, thief_snaive, thief_hm, hm_thief)
save(thief_df, file = "thief_df.RData")
