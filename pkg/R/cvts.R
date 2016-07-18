#' Cross validation for time series
#'
#' Perform cross validation on time series 
#'
#' @export
#' @param x the input time series
#' @param FUN the model function used. See details.
#' @param FCFUN a function that proces point forecasts for the model function. This defaults to forecast().
#' See details.
#' @param rolling should a rolling procedure be used? If TRUE, nonoverlapping windows of size maxHorizon
#' will be used for fitting each model. If FALSE, the size of the dataset used for training will grow
#' by one each iteration.
#' @param windoSize length of the window to build each model. When rolling == TRUE, the each model will be
#' fit to a time series of this length, and when rolling == FALSE the first model will be fit to a series
#' of this length and grow by one each iteration
#' @param maxHorizon maximum length of the forecast horizon to use for computing errors
#' @param horizonAverage should the final errors be an average over all the horizons instead of producing
#' metrics for each individual horizon?
#' @param verbose should the current fold number and the total number of folds be printed to the console?
#' 
#' @examples
#' 
#' cvmod1 <- cvts(AirPassengers, FUN = ets, FCFUN = forecast, rolling = TRUE, windowSize = 48, maxHorizon = 12)
#' cvmod2 <- cvts(AirPassengers, FUN = hybridModel, FCFUN = forecast, rolling = TRUE, windowSize = 48, maxHorizon = 12)
cvts <- function(x, FUN = NULL, FCFUN = NULL,
                 rolling = FALSE, windowSize = 84,
                 useHorizon = 5, maxHorizon = 5,
                 horizonAverage = FALSE,
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
   
   
   # Combined code for rolling/nonrolling CV
   if(rolling){
     results <- matrix(NA,
                       nrow = length(x) - windowSize - maxHorizon,
                       ncol = maxHorizon)
     
   }
   else{
     results <- matrix(NA,
                       nrow = as.integer((length(x) - windowSize) / maxHorizon),
                       ncol = maxHorizon)
   }
   forecasts <- fits <- vector("list", nrow(results))
   
   # Needed for nonrolling
   startWindow <- 1
   endWindow <- windowSize
   # Perform the cv fits
   for(i in 1:nrow(results)){
     # Sample the correct slice for rolling
     if(verbose){
       print(paste("Fitting fold", i, "of", nrow(results)))
     }
     if(rolling){
       stsp <- tsp(x)[1]
       etsp <- stsp + (i + maxHorizon - 2) / frequency(x)
       y <- window(x, start = stsp, end = etsp)
       # y <- ts(x[1:windowSize], f = f)
       nextHorizon <- windowSize + maxHorizon
       ynext <- x[(windowSize + 1):nextHorizon]
       windowSize <- windowSize + 1
     }
     # Sample the correct slice for nonrolling
     else{
       stsp <- tsp(x)[1] + (i - 1) / frequency(x)
       etsp <- stsp + (maxHorizon - 1) / frequency(x)
       y <- window(x, start = stsp, end = etsp) 
       ynext <- x[(endWindow + 1):(endWindow + maxHorizon)]
       startWindow <- startWindow + maxHorizon
       endWindow <- endWindow + maxHorizon
     }
     # Perfom the simulation
     mod <- do.call(FUN, list(y))
     fits[[i]] <- mod
     fc <- do.call(FCFUN, list(mod, h = maxHorizon))
     forecasts[[i]] <- fc
     results[i, ] <- ynext - fc$mean
   }
   
   
#    if(rolling){
#       # Number of rows will be determined by the series length, windowSize and maxHorizon
#       # Number of columns is the maxHorizon
#       results <- matrix(NA,
#                         nrow = length(x) - windowSize - maxHorizon,
#                         ncol = maxHorizon)
#       #i <- 1
#       for(i in 1:nrow(results)){
#          if(verbose){
#             print(paste("Fitting fold", i, "of", nrow(results)))
#          }
#         # tsp properties should be preserved, "forecast" might adopt this in subset()
#         # If issue 343 is accepted in the "forecast" package, this can be replaced with subset.ts
#         stsp <- tsp(x)[1]
#         etsp <- stsp + (i + maxHorizon - 2) / frequency(x)
#         y <- window(x, start = stsp, end = etsp)
#         # y <- ts(x[1:windowSize], f = f)
#         nextHorizon <- windowSize + maxHorizon
#         ynext <- x[(windowSize + 1):nextHorizon]
#         # This will be replaced with do.call on FUN
#         mod <- do.call(FUN, list(y))
#         fits[[i]] <- mod
#         fc <- do.call(FCFUN, list(mod, h = maxHorizon))
#         forecasts[[i]] <- fc
#         results[i, ] <- ynext - fc$mean
#         windowSize <- windowSize + 1
#       }
#    }
#    else{
#      results <- matrix(NA,
#                        nrow = as.integer((length(x) - windowSize) / maxHorizon),
#                        ncol = maxHorizon)
#      startWindow <- 1
#      endWindow <- windowSize
#      for(i in 1:nrow(results)){
#         if(verbose){
#            print(paste("Fitting fold", i, "of", nrow(results)))
#         }
#        # tsp properties should be preserved, "forecast" might adopt this in subset()
#        # If issue 343 is accepted in the "forecast" package, this can be replaced with subset.ts
#        stsp <- tsp(x)[1] + (i - 1) / frequency(x)
#        etsp <- stsp + (maxHorizon - 1) / frequency(x)
#        y <- window(x, start = stsp, end = etsp) 
#        ynext <- x[(endWindow + 1):(endWindow + maxHorizon)]
#        mod <- do.call(FUN, list(y))
#         fits[[i]] <- mod
#         fc <- do.call(FCFUN, list(mod, h = maxHorizon))
#         forecasts[[i]] <- fc
#         results[i, ] <- ynext - fc$mean
#         startWindow <- startWindow + maxHorizon
#         endWindow <- endWindow + maxHorizon
#      }
#    }
   
   
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
