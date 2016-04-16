#' Hybrid forecast
#' 
#' Forecast method for hybrid models.
#' 
#' @export
#' @import forecast
#' @import fpp
#' @param object A hybrid time series model fit with hybridModel()
#' @param h Number of periods for forecasting. If \code{xreg} is used, \code{h} is ignored and the number of forecast 
#' periods is set to the number of rows of \code{xreg}.
#' @param xreg Future values of regression variables (for use if one of the ensemble methods used
#' in creating the hybrid forecast was \code{auto.arima} or \code{stlm} and a xreg was used in the fit)
#' @param level Confidence level for prediction intervals
#' @param fan If \code{TRUE}, level is set to \code{seq(51, 99, by = 3)}. This is suitable for fan plots.
#' @param ... other arguments; currently not used.
#' @seealso \code{\link{hybridModel}}
#' @details If \code{xreg} was used in construcing the \code{hybridModel}, it must also be passed into \code{forecast.hybridModel}
#' @return An object of class forecast.
#' @examples
#' \dontrun{
#' mod <- hybridModel(AirPassengers)
#' plot(forecast(mod))
#' }
#'
forecast.hybridModel <- function(object, h = ifelse(object$frequency > 1, 2 * object$frequency, 10), xreg = NULL,
                                 level = c(80, 95), fan = FALSE, ...){
  
  # Check inputs
  if(!is.hybridModel(object)){
    stop("The object must be constructed from hybridModel().")
  }
  
  # xreg should be a matrix and have same number of observations as the horizon
  if(!is.null(xreg)){
    if(!is.matrix(xreg) && !is.data.frame(xreg)){
      stop("The supplied xreg must be a matrix or data.frame.")
    }
    xreg <- as.matrix(xreg)
    if(!is.numeric(xreg)){
      stop("The supplied xreg must be numeric.")
    }
    if(nrow(xreg) != h){
      warning("The number or rows in xreg should match h. Setting h to nrow(xreg).")
      h <- nrow(xreg)
    }
  }
  
  # Check the forecast horizon
  if(!is.numeric(h)){
    stop("The forecast horizon h must be a positive integer.")
  }
  if(as.logical((h %% 1L)) || h <= 0L){
    stop("The forecast horizon h must be a positive integer.")
  }
  
  # Allow for fan prediction intervals
  if(fan){
    level <- seq(51, 99, by = 3)
  } else {
    if(min(level) > 0 && max(level) < 1){
      level <- 100 * level
    } else if(min(level) < 0 || max(level) > 99.99){
      stop("Confidence limit out of range")
    }
  }

  
  # This code is pretty ugly, There is probably a better way of doing this.
  forecastWeights <- object$weights
  weightsMatrix <- matrix(rep(forecastWeights, times = h), nrow = h, byrow = TRUE)
  includedModels <- object$models
  forecasts <- list()
  forecasts$pointForecasts <- matrix(numeric(), nrow = h, ncol = length(includedModels))
  colnames(forecasts$pointForecasts) <- includedModels
  if("auto.arima" %in% includedModels){
    forecasts$auto.arima <- forecast(object$auto.arima, h = h, xreg = xreg, level = level)
    forecasts$pointForecasts[, "auto.arima"] <- forecasts$auto.arima$mean
  }
  if("ets" %in% includedModels){
    forecasts$ets <- forecast(object$ets, h = h, level = level)
    forecasts$pointForecasts[, "ets"] <- forecasts$ets$mean
  }
  if("nnetar" %in% includedModels){
    forecasts$nnetar <- forecast(object$nnetar, h = h, xreg = xreg)
    forecasts$pointForecasts[, "nnetar"] <- forecasts$nnetar$mean
  }
  if("stlm" %in% includedModels){
    forecasts$stlm <- forecast(object$stlm, h = h, xreg = xreg, level = level)
    forecasts$pointForecasts[, "stlm"] <- forecasts$stlm$mean
  }
  if("tbats" %in% includedModels){
    forecasts$tbats <- forecast(object$tbats, h = h, level = level)
    forecasts$pointForecasts[, "tbats"] <- forecasts$tbats$mean
  }
  
  # Apply the weights to the individual forecasts and create the final point forecast
  finalForecast <- rowSums(forecasts$pointForecast * weightsMatrix)
  # Conver the final forecast into a ts object
  finalForecast <- ts(finalForecast,
                      start = start(forecasts[[object$models[1]]]$mean),
                      end = end(forecasts[[object$models[1]]]$mean),
                      frequency = object$frequency)
  
  # Apply the weights to construct the fitted values
  fits <- sapply(includedModels, FUN = function(x) fitted(object[[x]]))
  fitsWeightsMatrix <- matrix(rep(forecastWeights, times = nrow(fits)),
                              nrow = nrow(fits), byrow = TRUE)
  fits <- rowSums(fits * fitsWeightsMatrix)
  resid <- object$x - fits
  
  # Construct the prediction intervals
  nint <- length(level)
  upper <- lower <- matrix(NA, ncol = nint, nrow = length(finalForecast))
  # Prediction intervals for nnetar do not currently work, so exclude these
  piModels <- object$models[object$models != "nnetar"]
  # Produce each upper/lower limit
  for(i in 1:nint){
    # Produce the upper/lower limit for each model for a given level
    tmpUpper <- tmpLower <- matrix(NA, nrow = h, ncol = length(piModels))
    j2 <- 1
    for(j in piModels){
      tmpUpper[, j2] <- as.numeric(forecasts[[j]]$upper[, i])
      tmpLower[, j2] <- as.numeric(forecasts[[j]]$lower[, i])
      j2 <- j2 + 1
    }
    # upper/lower prediction intervals are the extreme values for now
    # We can modify it for other approaches by changing the FUN here
    upper[, i] <- apply(tmpUpper, 1, FUN = max)
    lower[, i] <- apply(tmpLower, 1, FUN = min)
  }
  if(!is.finite(max(upper)) || !is.finite(min(lower))){
    warning("Prediction intervals are not finite.")
  }
  colnames(lower) <- colnames(upper) <- paste0(level, "%")
  forecasts$lower <- lower
  forecasts$upper <- upper
  
  # Build the mean forecast as a ts object
  tsp.x <- tsp(object$x)
  if (!is.null(tsp.x)){
    start.f <- tsp(object$x)[2] + 1/object$frequency
  } else{
    start.f <- length(object$x) + 1
  }
  stop.f <- start.f + h / object$frequency
  forecasts$mean <- finalForecast
  
  # Add the fitted and residuals values
  if(is.ts(object$x)){
    fits <- ts(fits)
    resid <- ts(resid)
    tsp(fits) <- tsp(resid) <- tsp(object$x)
  }
  forecasts$fitted <- fits
  forecasts$residuals <- resid
  
  # Build a forecast object
  forecasts$x <- forecasts[[object$models[1]]]$x
  forecasts$method <- paste0(object$models, " with weight ", object$weights)
  forecasts$level <- level
  class(forecasts) <- "forecast"
  return(forecasts)
}
