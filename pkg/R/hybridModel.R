#' Hybrid time series modelling
#' 
#' Create a hybrid time series model from one to four contributing models
#' 
#' @export
#' @import forecast
#' @import fpp
#' @param y a numeric vector or time series
#' @param lambda 
#' Box-Cox transformation parameter.  Ignored if NULL.  Otherwise, data transformed before model is estimated.  
#' @param models a character string of up to five characters indicating which contributing models to use: 
#' a (\code{auto.arima()}), e (\code{ets()}), n (\code{nnetar()}), s (\code{stlm()}) and t (\code{tbats()})
#' @param a.args Arguments to pass to \code{auto.arima()}
#' @param e.args Arguments to pass to \code{ets()}
#' @param n.args Arguments to pass to \code{nnetar()}
#' @param s.args Arguments to pass to \code{stlm()}
#' @param t.args Arguments to pass to \code{tbats()}
#' @param weights Method for weighting the forecasts of the various contributing
#' models.  Defaults to equal, which has shown to be robust and surprisingly better
#' in many cases than giving more weight to models with better past performance. Weights
#' utilizing insample errors are also available. Cross validated errors are currently unimplemented.
#' @param errorMethod  Method of measuring accuracy to use if weights are not 
#' to be equal. Root mean square error (RMSE), mean absolute error (MAE) and mean absolute scaled error (MASE)
#' are supported.
#' @param parallel  Should parallel processing be used between models? This is currently unimplemented.
#' Parallelization will still occur within individual models that suport it and can be controlled using a.args and t.args.
#' @param num.cores If \code{parallel=TRUE}, how many cores to use.
#' @seealso \code{\link{forecast.hybridModel}}
#' @return An object of class hybridModel. The individual component models are stored inside of the object
#' and can be accessed for all the regular manipulations available in the forecast package.
#' @details The hybridModel function fits multiple individual model specifications to allow easy creation
#' of ensemble forecasts. While default settings for the individual component models work quite well
#' in most cases, fine control can be exerted by passing detailed arguments to the component models in the
#' a.args, e.args, n.args, s.args, and t.args lists.
#' @examples
#' mod1 <- hybridModel(AirPassengers)
#' plot(forecast(mod1))
#' mod2 <- hybridModel(AirPassengers, models = "aet",
#' weights = "insample.errors", errorMethod = "MASE")
#' mod3 <- hybridModel(AirPassengers, models = "aens",
#' a.args = list(max.p = 7, max.q = 7, approximation = FALSE))
#'
hybridModel <- function(y, models = "aenst",
                        lambda = NULL,
                        a.args = NULL, e.args = NULL, n.args = NULL, s.args = NULL, t.args = NULL, 
                        weights = c("equal", "insample.errors", "cv.errors"),
                        errorMethod = c("RMSE", "MAE", "MASE"),
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
  if(length(expandedModels) > 5L){
    stop("Invalid models specified.")
  }
  # All characters must be valid
  if(!(all(expandedModels %in% c("a", "e", "n", "s", "t")))){
    stop("Invalid models specified.")
  }
  if(!length(expandedModels)){
    stop("At least one component model type must be specified.")
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
  
  # Check a.args/t.args/e.args/n.args/s.args
  if(!is.null(a.args) && !is.element("a", expandedModels)){
    warning("auto.arima was not selected in the models argument, but a.args was passed. Ignoring a.args")
  }
  if(!is.null(e.args) && !is.element("e", expandedModels)){
    warning("ets was not selected in the models argument, but e.args was passed. Ignoring a.args")
  }
  if(!is.null(n.args) && !is.element("n", expandedModels)){
    warning("nnetar was not selected in the models argument, but n.args was passed. Ignoring a.args")
  }
  if(!is.null(s.args) && !is.element("s", expandedModels)){
    warning("stlm was not selected in the models argument, but s.args was passed. Ignoring a.args")
  }
  if(!is.null(t.args) && !is.element("t", expandedModels)){
    warning("tbats was not selected in the models argument, but t.args was passed. Ignoring a.args")
  }
  
  
  modelResults <- list()
  
  # We would allow for these models to run in parallel at the model level rather than within the model
  # since this has better performance. As an enhancement, users with >4 cores could benefit by running
  # parallelism both within and between models, based on the number of available cores.
  
  # auto.arima(), additional arguments to be implemented
  if(is.element("a", expandedModels)){
    if(is.null(a.args)){
      a.args <- list(lambda = lambda)
    } else if(is.null(a.args$lambda)){
      a.args$lambda <- lambda
    }
    modelResults$auto.arima <- do.call(auto.arima, c(list(y), a.args))
  }
  # ets(), additional arguments to be implemented
  if(is.element("e", expandedModels)){
    if(is.null(e.args)){
      e.args <- list(lambda = lambda)
    } else if(is.null(e.args$lambda)){
      e.args$lambda <- lambda
    }
    modelResults$ets <- do.call(ets, c(list(y), e.args))
  }
  # nnetar(), additional arguments to be implemented
  if(is.element("n", expandedModels)){
    if(is.null(n.args)){
      n.args <- list(lambda = lambda)
    } else if(is.null(n.args$lambda)){
      n.args$lambda <- lambda
    }
    modelResults$nnetar <- do.call(nnetar, c(list(y), n.args))
  }
  # stlm(), additional arguments to be implemented
  if(is.element("s", expandedModels)){
    if(is.null(s.args)){
      s.args <- list(lambda = lambda)
    } else if(is.null(s.args$lambda)){
      s.args$lambda <- lambda
    }
    modelResults$stlm <- do.call(stlm, c(list(y), s.args))
  }
  # tbats(), additional arguments to be implemented
  if(is.element("t", expandedModels)){
    modelResults$tbats <- do.call(tbats, c(list(y), e.args))
  }
  
  
  # Set the model weights
  includedModels <- names(modelResults)
  # Weighting methods would go here, equal weighting for now
  if(weights == "equal"){
    modelResults$weights <- rep(1 / length(expandedModels), length(expandedModels))
  } else if(weights == "insample.errors"){
    # There is probably a better way of accomplishing this
    # But this ugly approach will work for now
    modelResults$weights <- rep(0, length(expandedModels))
    index <- 1
    for(i in expandedModels){
      if(i == "a"){
        modelResults$weights[index] <- accuracy(modelResults$auto.arima)[1, errorMethod]
      }else if(i == "e"){
        modelResults$weights[index] <- accuracy(modelResults$ets)[1, errorMethod]
      } else if(i == "n"){
        modelResults$weights[index] <- accuracy(modelResults$nnetar)[1, errorMethod]
      } else if(i == "s"){
        modelResults$weights[index] <- accuracy(modelResults$stlm)[1, errorMethod]
      } else if(i == "t"){
        modelResults$weights[index] <- accuracy(modelResults$tbats)[1, errorMethod]
      }
      index <- index + 1
    }
    # Scale the weights
    modelResults$weights <- (1 / modelResults$weights) / sum(1 / modelResults$weights)
    
  }
  names(modelResults$weights) <- includedModels
  
  # Prepare the hybridModel object
  class(modelResults) <- "hybridModel"
  modelResults$frequency <- frequency(y)
  modelResults$x <- y
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
