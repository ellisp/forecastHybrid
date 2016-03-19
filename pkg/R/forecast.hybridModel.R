# TODO This function will need substantial work. We'll need to build an object of the "forecast" class.
# We'll need to consider prediction intervals as well. Don't include, simulate them from bootstrap,
# or use extreme values from the individual components of the model?

#' Hybrid forecast
#' 
#' Forecast method for hybrid models
#' 
#' @export
#' @import forecast
#' @param object A hybrid time series model fit with hybridModel()
#' @param h number of periods to forecast ahead
#' @seealso \code{\link{hybridModel}}
#' @details more detailed description here 
#' @return an object of class forecast.
#' @examples
#' mod <- hybridModel(AirPassengers)
#' plot(forecast(mod))
#'
forecast.hybridModel <- function(object, h = ifelse(object$frequency > 1, 2 * object$frequency, 10), xreg = NULL,
                                 level = c(80, 95)){
  
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
    forecasts$nnetar <- forecast(object$nnetar, h = h)
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
  
  # Construct the upper/lower prediction intervals
  # This is ugly and should be revisited
  # Methods besides the extreme cases should also be considered
  # This also only works with two levels (very bad!)
  lowerlimit1 <- rep(numeric(), h)
  lowerlimit2 <- lowerlimit1
  upperlimit1 <- lowerlimit1
  upperlimit2 <- lowerlimit1
  
  for(i in object$models){
    if(i != "nnetar"){
      lowerlimit1 <- cbind(lowerlimit1, matrix(forecasts[[i]]$lower[, 1], ncol = 1))
      lowerlimit2 <- cbind(lowerlimit2, matrix(forecasts[[i]]$lower[, 2], ncol = 1))
      upperlimit1 <- cbind(upperlimit1, matrix(forecasts[[i]]$upper[, 1], ncol = 1))
      upperlimit2 <- cbind(upperlimit2, matrix(forecasts[[i]]$upper[, 2], ncol = 1))
    }
  }
  lowerbounds1 <- as.numeric(apply(lowerlimit1, 1, min))
  lowerbounds2 <- as.numeric(apply(lowerlimit2, 1, min))
  upperbounds1 <- as.numeric(apply(upperlimit1, 1, max))
  upperbounds2 <- as.numeric(apply(upperlimit2, 1, max))
  forecasts$upper <- matrix(c(upperbounds1, upperbounds2), ncol = 2, byrow = TRUE)
  forecasts$lower <- matrix(c(lowerbounds1, lowerbounds2), ncol = 2, byrow = TRUE)
  colnames(forecasts$upper) <- c(paste0(level[1], "%"), paste0(level[2], "%"))
  colnames(forecasts$lower) <- c(paste0(level[1], "%"), paste0(level[2], "%"))
  
  
  # Code would unequal weights would be needed here
  finalForecast <- rowSums(forecasts$pointForecast * weightsMatrix)
  # Conver the final forecast into a ts object
  finalForecast <- ts(finalForecast,
                      start = start(forecasts[[object$models[1]]]$mean),
                      end = end(forecasts[[object$models[1]]]$mean),
                      frequency = object$frequency)
  
  # Build a forecast object
  forecasts$mean <- finalForecast
  forecasts$x <- forecasts[[object$models[1]]]$x
  forecasts$method <- paste0(object$models, " with weight ", object$weights)
  # Code for inplementing confidence intervals will go here
  #forecasts$finalForecast$lower <- NA
  #forecasts$finalForecast$uppwer <- NA
  #forecasts$finalForecast$level <- NA
  class(forecasts) <- "forecast"
  
  
  # We'll need to package this as an object of class "forecast", including prediction intervals, if applicable.
  return(forecasts)
}
