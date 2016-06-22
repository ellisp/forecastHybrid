#' Cross validatoin for time series
#'
#' Perform cross validation on time series 
#'
#' @export
#' @param x the input time series
#' @param FUN the model function.
#' @param FCFUN a function that proces point forecasts for the model function.
#' @param rolling should a rolling procedure be used?
#' @param window llength of the window to build each model
#' @param horizon length of the forecast horizon to use for computing errors
#' @param horizonAverage should the final errors be an average over all the horizons?
#' 
cvts <- function(x, FUN = NULL, FCFUN = NULL, rolling = TRUE, windowSize = 10, useHorizon = 5, maxHorizon = 5, horizonAverage = TRUE){
   # try-catch this conversion
   x <- ts(x)
   
   # Adapted from Rob Hyndman's approach
   # http://robjhyndman.com/hyndsight/tscvexample/
   k <- windowSize # minimum data length for fitting a model
   n <- length(x)
   rmse <- mae <- mase <- matrix(NA, n - k, maxHorizon)
   st <- tsp(x)[1] + (k - 2) / maxHorizon
   fits <- vector("list", n - k)
   for(i in 1:(n - k))
   {
      print(paste(i, "of", n - k))
      xshort <- window(x, end = st + i/ maxHorizon)
      xnext <- window(x, start = st + (i + 1) / maxHorizon, end = st + (i + maxHorizon) / maxHorizon)
      fit <- auto.arima(xshort)
      fcast <- forecast(fit, h = maxHorizon)
      
      rmse[i,1:length(xnext)] <- abs(fcast[['mean']] - xnext)
      mae[i,1:length(xnext)] <- abs(fcast[['mean']] - xnext)
      mase[i,1:length(xnext)] <- abs(fcast[['mean']] - xnext)
      #temporary, for debugging
      fits[[i]] <- fit
   }
   # this will be replaced with the fitted values
#    forecasted <- data
#    return(accuracy(f = forecasted, x = data))
   result <- list(rmse = rmse, mae = mae, mase = mase, fits = fits)
   class(result) <- "cv"
   return(result)
}
