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
#' Characteristics of the input series can cause problems for certain types of models and paramesters.
#' For example, \code{stlm()} models require that the input series be seasonal, and there furthemore
#' must be more than two seasonal periods of data for the decomposition. If it is not the case, \code{hybridModel()}
#' will remove the \code{stlm()} model so an error does not occur. The \code{ets()} model does not handle 
#' a series well with a seasonal period longer than 24 and will ignore the seasonality. In this case,
#' \code{hybridModel()} will also drop the \code{ets()} model from the ensemble.
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
  
  # Check for problems for specific models (e.g. long seasonality for ets and non-seasonal for stlm)
  if(is.element("e", expandedModels) && frequency(y) >=24){
    warning("frequency(y) >= 24. The ets model will not be used.")
    expandedModels <- expandedModels[expandedModels != "e"]
  }
  if(is.element("s", expandedModels)){
    if(frequency(y) < 2L){
      warning("The stlm model requires that the input data be a seasonal ts object. The stlm model will not be used.")
      expandedModels <- expandedModels[expandedModels != "s"]
    } else if(frequency(y) * 2L >= length(y)){
      warning("The stlm model requres a series more than twice as long as the seasonal period. The stlm model will not be used.")
      expandedModels <- expandedModels[expandedModels != "s"]
    }
  }
  
  # A model run should include at least two component models
  if(length(expandedModels) < 2L){
    stop("A hybridModel must contain at least two component models.")
  }
  
  modelResults <- list()
  
  # We would allow for these models to run in parallel at the model level rather than within the model
  # since this has better performance. As an enhancement, users with >4 cores could benefit by running
  # parallelism both within and between models, based on the number of available cores.
  
  # auto.arima()
  if(is.element("a", expandedModels)){
    if(is.null(a.args)){
      a.args <- list(lambda = lambda)
    } else if(is.null(a.args$lambda)){
      a.args$lambda <- lambda
    }
    modelResults$auto.arima <- do.call(auto.arima, c(list(y), a.args))
  }
  # ets()
  if(is.element("e", expandedModels)){
    if(is.null(e.args)){
      e.args <- list(lambda = lambda)
    } else if(is.null(e.args$lambda)){
      e.args$lambda <- lambda
    }
    modelResults$ets <- do.call(ets, c(list(y), e.args))
  }
  # nnetar()
  if(is.element("n", expandedModels)){
    if(is.null(n.args)){
      n.args <- list(lambda = lambda)
    } else if(is.null(n.args$lambda)){
      n.args$lambda <- lambda
    }
    modelResults$nnetar <- do.call(nnetar, c(list(y), n.args))
  }
  # stlm()
  if(is.element("s", expandedModels)){
    if(is.null(s.args)){
      s.args <- list(lambda = lambda)
    } else if(is.null(s.args$lambda)){
      s.args$lambda <- lambda
    }
    modelResults$stlm <- do.call(stlm, c(list(y), s.args))
  }
  # tbats()
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

# skeleton for the plot method
#' Plot a hybridModel object
#' 
#' Plot the original series and the fitted values from each component model
#' 
#' @export
#' @import forecast
#' @import fpp
#' @param object An object of class hybridModel to plot.
#' @seealso \code{\link{hybridModel}}
#' @return None. Function produces a plot.
#' @details The original series is plotted in black. Fitted values for the 
#' individual component models are plotted in other colors. Prior to the
#' release of forecast 6.3, stlm objects will be ignored since they
#' do not contain a fitted() or residual() method.
#' @examples
#' hm <- hybridModel(woolyrnq, models = "aenst")
#' plot(hm)
#' 
plot.hybridModel <- function(object, ...){
  # nnetar and stlm currently don't have plot methods
  plotModels <- object$models[object$models != "stlm"]
  # Set the highest and lowest axis scale
  ymax <- max(sapply(plotModels, FUN = function(i) max(fitted(object[[i]]), na.rm = TRUE)))
  ymin <- min(sapply(plotModels, FUN = function(i) min(fitted(object[[i]]), na.rm = TRUE)))
  range <- ymax - ymin
  plot(object$x, ylim = c(ymin - 0.05 * range, ymax + 0.25 * range),
       ylab = "y", xlab = "time")
  title("Plot of original series (black) and fitted component models", outer = TRUE)
  #count <- 2
  for(i in seq_along(plotModels)){
    lines(fitted(object[[plotModels[i]]]), col = i + 1)
    #count <- count + 1
  }
  legend("top", plotModels, fill = 2:(length(plotModels) + 1), horiz = TRUE)
}
