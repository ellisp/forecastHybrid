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
#' @param verbose should the current fold number and the total number of folders be printed to the console?
#' 
cvts <- function(x, FUN = NULL, FCFUN = NULL,
                 rolling = TRUE, windowSize = 84,
                 useHorizon = 5, maxHorizon = 5,
                 horizonAverage = TRUE,
                 verbose = TRUE){
   # Default forecast function
   if(is.null(FCFUN)){
      FCFUN <- forecast
   }
   # try-catch this conversion
   x <- ts(x)
   f <- frequency(x)
   if(any(sapply(c(x, windowSize, useHorizon, maxHorizon), FUN = function(x) !is.numeric(x)))){
     stop("The arguments x, windowSize, useHorizon, and maxHorizon must all be numeric.")
   }
   
   
   if(any(c(windowSize, useHorizon, maxHorizon) < 1L)){
     stop("The arguments windowSize, useHorizon, and maxHorizon must be positive integers.")
   }
   
   if(any(c(windowSize, useHorizon, maxHorizon) %% 1L != 0)){
     stop("The arguments windowSize, useHorizon, and maxHorizon must be positive integers.")
   }
   
   # Ensure at least two periods are tested
   if(windowSize + 2 * maxHorizon > length(x)){
     stop("The time series must be longer than windowSize + 2 * maxHorizon.")
   }
   
   # Rolling cv is easiest to test: larger windows are built until the windowSize + maxHorizon
   # is longer than the timeseries
   forecasts <- fits <- vector("list", 1)
   if(!rolling){
      # Number of rows will be determined by the series length, windowSize and maxHorizon
      # Number of columns is the maxHorizon
      results <- matrix(NA,
                        nrow = as.integer((length(x) - windowSize) / maxHorizon),
                        ncol = maxHorizon)
      #i <- 1
      for(i in 1:nrow(results)){
        y <- ts(x[1:windowSize], f = f)
        nextHorizon <- windowSize + maxHorizon
        ynext <- x[(windowSize + 1):nextHorizon]
        # This will be replaced with do.call on FUN
        mod <- do.call(FUN, list(y)) #auto.arima(y)
        fits[[i]] <- mod
        # This will be replaced with do.call on FCFUN
        fc <- do.call(FCFUN, list(mod, h = maxHorizon)) #forecast(mod, h = maxHorizon)
        forecasts[[i]] <- fc
        results[i, ] <- ynext - fc$mean
        windowSize <- nextHorizon
      }
#       while(windowSize + maxHorizon <= length(x)){
#          # If issue 343 is accepted in the "forecast" package, this can be replaced with subset.ts
#          # This should ultimately be cleaned up to preserve all ts attributes
#          y <- ts(x[1:windowSize], f = f)
#          ynext <- x[(windowSize + 1):(windowSize + maxHorizon)]
#          # This will be replaced with do.call on FUN
#          mod <- do.call(FUN, list(y)) #auto.arima(y)
#          fits[[i]] <- mod
#          # This will be replaced with do.call on FCFUN
#          fc <- do.call(FCFUN, list(mod, h = maxHorizon)) #forecast(mod, h = maxHorizon)
#          forecasts[[i]] <- fc
#          results[i, ] <- ynext - fc$mean
#          windowSize <- windowSize + maxHorizon
#          i <- i + 1
#       }
   } else{
      # Nothing for now
   }
   
   
#    # Adapted from Rob Hyndman's approach
#    # http://robjhyndman.com/hyndsight/tscvexample/
#    k <- windowSize # minimum data length for fitting a model
#    n <- length(x)
#    forecasts <- mae <- mase <- matrix(NA, n - k, maxHorizon)
#    st <- tsp(x)[1] + (k - 2) / maxHorizon
#    fits <- vector("list", n - k)
#    for(i in 1:(n - k))
#    {
#       if(verbose){
#          print(paste(i, "of", n - k))
#       }
#       xshort <- window(x, end = st + i/ maxHorizon)
#       xnext <- window(x, start = st + (i + 1) / maxHorizon, end = st + (i + maxHorizon) / maxHorizon)
#       fit <- auto.arima(xshort)
#       fcast <- forecast(fit, h = maxHorizon)
#       
#       forecasts[i, 1:length(xnext)] <- fcast$mean
# #       rmse[i,1:length(xnext)] <- abs(fcast[['mean']] - xnext)
# #       mae[i,1:length(xnext)] <- abs(fcast[['mean']] - xnext)
# #       mase[i,1:length(xnext)] <- abs(fcast[['mean']] - xnext)
#       #temporary, for debugging
#       fits[[i]] <- fit
#    }
#    # this will be replaced with the fitted values
# #    forecasted <- data
# #    return(accuracy(f = forecasted, x = data))
   result <- list(forecasts = forecasts, models = fits, residuals = results)
   class(result) <- "cv"
   return(result)
}
