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
                        xreg = NULL, lambda = NULL,
                        weights = c("equal", "insample.errors", "cv.errors"),
                        errorMethod = c("rmse", "mae", "mase"),
                        parallel = TRUE, num.cores = 2L){
  # Weights could be set to equal (the default), based on in-sample errors, or based on cv errors
  # In-sample errors are methodologically questionable but easy to implement. CV errors are probably
  # methodologicaly sound, but more difficult and computationally expensive to implement.
  
  # errorMethod will determine which type of errors to use for weights. Some choices from accuracy()
  # are not appropriate. If weights = "equal", this would be ignored.
  
  # The dependent variable must be numeric and not a matrix/dataframe
  if(!is.numeric(y) || !is.null(dim(y))){
    stop("The time series must be numeric and may not be a matrix or dataframe object.")
  }
  if(!length(y)){
    stop("The time series must have obserations")
  }
  y <- as.ts(y)
  
  # Match arguments to ensure validity
  weights <- match.arg(weights)
  errorMethod <- match.arg(errorMethod)
  
  # Match the specified models
  expandedModels <- unique(tolower(unlist(strsplit(models, split = ""))))
  if(length(expandedModels) > 4L){
    stop("Invalid models specified.")
  }
  # All characters must be valid
  if(!(all(expandedModels %in% c("a", "t", "e", "n")))){
    stop("Invalid models specified.")
  }
  if(!length(expandedModels)){
    stop("At least one model type must be specified.")
  }
  
  # xreg must be a matrix and have same number of observations as the timeseries
  if(!is.null(xreg)){
    if(nrow(xreg) != length(y)){
      stop("The supplied xreg must have the same number of rows as the timeseries.")
    }
    if(!is.matrix(xreg) && !is.data.frame(xreg)){
      stop("The supplied xreg must be a matrix or data.frame.")
    }
    xreg <- as.matrix(xreg)
    if(!is.numeric(xreg)){
      stop("The supplied xreg must be numeric.")
    }
  }
  
  # Validate cores and parallel arguments
  if(!is.logical(parallel)){
    stop("The parallel argument must be TRUE/FALSE.")
  }
  if(!is.numeric(num.cores)){
    stop("The number of cores specified must be an integer greater than zero.")
  }
  if(as.logical((num.cores %% 1L)) || num.cores <= 0L){
    stop("The number of cores specified must be an integer greater than zero.")
  }

  
  modelResults <- list()
  
  # We would allow for these models to run in parallel at the model level rather than within the model
  # since this has better performance. As an enhancement, users with >4 cores could benefit by running
  # parallelism both within and between models, based on the number of available cores.
  
  # auto.arima(), additional arguments to be implemented
  if(is.element("a", expandedModels)){
    modelResults$auto.arima <- auto.arima(y, xreg = xreg, lambda = lambda)
  }
  # tbats(), additional arguments to be implemented
  if(is.element("t", expandedModels)){
    modelResults$tbats <- tbats(y)
  }
  # ets(), additional arguments to be implemented
  if(is.element("e", expandedModels)){
    modelResults$ets <- ets(y, lambda = lambda)
  }
  # nnetar(), additional arguments to be implemented
  if(is.element("n", expandedModels)){
    modelResults$nnetar <- nnetar(y, lambda = lambda)
  }
  
  # Set the model weights
  includedModels <- names(modelResults)
  # Weighting methods would go here, equal weighting for now
  if(weights == "equal"){
    modelResults$weights <- rep(1 / length(expandedModels), length(expandedModels))
  }
  names(modelResults$weights) <- includedModels
  
  # Prepare the hybridModel object
  class(modelResults) <- "hybridModel"
  modelResults$frequency <- frequency(y)
  modelResults$models <- includedModels
  return(modelResults)
}

is.hybridModel <- function(x){
  inherits(x, "hybridModel")
}

fitted.hybridModel <- function(x){
  results <- list()
  for(i in x$models){
    results[[i]] <- fitted(x[[i]])
  }
  results
}

residuals.hybridModel <- function(x){
  results <- list()
  for(i in x$models){
    results[[i]] <- residuals(x[[i]])
  }
  results
}

accuracy.hybridModel <- function(x){
  results <- list()
  for(i in x$models){
    results[[i]] <- accuracy(x[[i]])
  }
}

summary.hybridModel <- function(x){
  print.hybridModel(x)
}

print.hybridModel <- function(x){
  cat("Hybrid forecast model comprised of the following models:", x$models, "\n")
  cat("with the respective weights:", x$weights, "\n\n")
  for(i in x$models){
    cat("############\n")
    cat(i, "\n")
    cat("############\n")
    print(x[[i]])
    cat("\n\n")
  }
}
