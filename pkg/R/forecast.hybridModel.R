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
#' @examples
#' print("hello world")
#'
forecast.hybridModel <- function(object, h = ifelse(object$frequency > 1, 2 * object$frequency, 10)){
   
   # This code is pretty ugly, There is probably a better way of doing this.
   forecastWeights <- object$weights
   weightsMatrix <- matrix(rep(forecastWeights, times = h), nrow = h, byrow = TRUE)
   includedModels <- names(object$weights)
   forecasts <- list()
   forecasts$pointForecasts <- matrix(0, nrow = h, ncol = length(includedModels))
   colnames(forecasts$pointForecasts) <- includedModels
   if("auto.arima" %in% includedModels){
      print("aa")
      forecasts$auto.arima <- forecast(object$auto.arima, h = h)
      forecasts$pointForecasts[, "auto.arima"] <- forecasts$auto.arima$mean
   }
   if("tbats" %in% includedModels){
      print("t")
      forecasts$tbats <- forecast(object$tbats, h = h)
      forecasts$pointForecasts[, "tbats"] <- forecasts$tbats$mean
   }
   if("ets" %in% includedModels){
      print("et")
      forecasts$ets <- forecast(object$ets, h = h)
      forecasts$pointForecasts[, "ets"] <- forecasts$ets$mean
   }
   if("nnetar" %in% includedModels){
      print("nn")
      forecasts$nnetar <- forecast(object$nnetar, h = h)
      forecasts$pointForecasts[, "nnetar"] <- forecasts$nnetar$mean
   }
   # Code would unequal weights would be needed here
   forecasts$finalForecast <- rowSums(forecasts$pointForecast * weightsMatrix)
   # We'll need to package this as an object of class "forecast", including prediction intervals, if applicable.
   return(forecasts)
}