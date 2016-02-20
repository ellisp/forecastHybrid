# TODO Add in additional arguments (e.g. max.p, max.q, model = "ZZZ" like in your implementation of hybridf())

#' Hybrid time series modelling
#' 
#' Create a hybrid time series model from one to four contributing models
#' 
#' @export
#' @param y a numeric vector or time series
#' @param models a four character string indicating which contributing models to use: 
#' a (\code{auto.arima()}), t (\code{tbats()}), e (\code{ets()}) and n (\code{nnetar()})
#' @param xreg Optionally, a vector or matrix of external regressors, which must 
#' have the same number of rows as y.  Only used if 'a' is part of the models string
#' ie if \code{auto.arima} is one of the contributing models
#' @param weights Method for weighting the forecasts of the various contributing
#' models.  Defaults to equal, which has shown to be robust and surprisingly better
#' in many cases than giving more weight to models with better past performance.
#' @param errorMethod.  Method of measuring accuracy to use if weights are not 
#' to be equal.
#' @param parallel  Should parallel processing be used?
#' @param num.cores If \code{parallel=TRUE}, how many cores to use.
#' @seealso \code{\link{forecast.hybridModel}}
#' @details more detailed description here 
#' @examples
#' print("hello world")
#'
hybridModel <- function(y, models = "aten",
                        xreg = NULL,
                        weights = c("equal", "insample.errors","cv.errors"),
                        errorMethod = c("rmse", "mae", "mase"),
                        parallel = TRUE, num.cores = 2){
   # Weights could be set to equal (the default), based on in-sample errors, or based on cv errors
   # In-sample errors are methodologically questionable but easy to implement. CV errors are probably
   # methodologicaly sound, but more difficult and computationally expensive to implement.
   
   # errorMethod will determine which type of errors to use for weights. Some choices from accuracy()
   # are not appropriate. If weights = "equal", this would be ignored.
   
   
   # This isn't robust yet. We need more robust argument matching
   numModels <- nchar(models)
   
   modelResults <- list()
   
   # We would allow for these models to run in parallel at the model level rather than within the model
   # since this has better performance. As an enhancement, users with >4 cores could benefit by running
   # parallelism both within and between models, based on the number of available cores.
   
   # auto.arima(), additional arguments to be implemented
   if(grep("a", models)){
      modelResults$auto.arima <- auto.arima(y, xreg = NULL)
   }
   # tbats(), additional arguments to be implemented
   if(grep("t", models)){
      modelResults$tbats <- tbats(y)
   }
   # ets(), additional arguments to be implemented
   if(grep("e", models)){
      modelResults$ets <- ets(y)
   }
   # nnetar(), additional arguments to be implemented
   if(grep("n", models)){
      modelResults$nnetar <- nnetar(y)
   }
   
   # Set the model weights
   includedModels <- names(modelResults)
   modelResults$weights <- rep(1 / numModels, numModels)
   names(modelResults$weights) <- includedModels
   
   class(modelResults) <- "hybridModel"
   modelResults$frequency <- frequency(y)
   return(modelResults)
}

