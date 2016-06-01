#' Hybrid time series modelling
#'
#' Create a hybrid time series model with two to five component models.
#'
#' @export
#' @import forecast
#' @import stats
#' @import graphics
#' @param y A numeric vector or time series.
#' @param lambda
#' Box-Cox transformation parameter.
#' Ignored if NULL. Otherwise, data transformed before model is estimated.
#' @param models A character string of up to five characters indicating which contributing models to use:
#' a (\code{\link[forecast]{auto.arima}}), e (\code{\link[forecast]{ets}}), n (\code{\link[forecast]{nnetar}}),
#' s (\code{\link[forecast]{stlm}}) and t (\code{\link[forecast]{tbats}})
#' @param a.args an optional \code{list} of arguments to pass to \code{\link[forecast]{auto.arima}}. See details.
#' @param e.args an optional \code{list} of arguments to pass to \code{\link[forecast]{ets}}. See details.
#' @param n.args an optional \code{list} of arguments to pass to \code{\link[forecast]{nnetar}}. See details.
#' @param s.args an optional \code{list} of arguments to pass to \code{\link[forecast]{stlm}}. See details.
#' @param t.args an optional \code{list} of arguments to pass to \code{\link[forecast]{tbats}}. See details.
#' @param weights method for weighting the forecasts of the various contributing
#' models.  Defaults to \code{equal}, which has shown to be robust and surprisingly better
#' in many cases than giving more weight to models with better in-sample performance. Weights
#' utilizing in-sample errors are also available. Cross validated errors are currently unimplemented.
#' @param errorMethod  method of measuring accuracy to use if weights are not
#' to be equal.
#' Root mean square error (\code{RMSE}), mean absolute error (\code{MAE})
#' and mean absolute scaled error (\code{MASE})
#' are supported.
#' @param parallel a boolean indicating if parallel processing should be used between models.
#' This is currently unimplemented.
#' Parallelization will still occur within individual models that suport it and can be controlled using \code{a.args} and \code{t.args}.
#' @param num.cores If \code{parallel=TRUE}, how many cores to use.
#' @seealso \code{\link{forecast.hybridModel}}, \code{\link[forecast]{auto.arima}},
#' \code{\link[forecast]{ets}}, \code{\link[forecast]{nnetar}},
#' \code{\link[forecast]{stlm}}, \code{\link[forecast]{tbats}}
#' @return An object of class hybridModel.
#' The individual component models are stored inside of the object
#' and can be accessed for all the regular manipulations available in the forecast package.
#' @details The \code{hybridModel} function fits multiple individual model specifications to allow easy creation
#' of ensemble forecasts. While default settings for the individual component models work quite well
#' in most cases, fine control can be exerted by passing detailed arguments to the component models in the
#' \code{a.args}, \code{e.args}, \code{n.args}, \code{s.args}, and \code{t.args} lists.
#' Note that if \code{xreg} is passed to the \code{a.args} or \code{n.args} component models
#' it must be passed as a dataframe instead of the matrix object
#' that the "forecast" package functions usually accept.
#' This is due to a limitation in how the component models are called.
#' \cr
#' Characteristics of the input series can cause problems for certain types of models and paramesters.
#' For example, \code{\link[forecast]{stlm}} models require that the input series be seasonal;
#' furthemore, the data must include at least two seasons of data (i.e. \code{length(y) >= 2 * frequency(y)})
#' for the decomposition to succeed.
#' If this is not the case, \code{hybridModel()}
#' will remove the \code{stlm} model so an error does not occur.
#' Similarly, \code{nnetar} models require that
#' \code{length(y) >= 2 * frequency(y)}, so these models will be removed if the condition is not satisfied
#' The \code{\link[forecast]{ets}} model does not handle
#' a series well with a seasonal period longer than 24 and will ignore the seasonality. In this case,
#' \code{hybridModel()} will also drop the \code{ets} model from the ensemble.
#'
#' @examples
#' \dontrun{
#'
#' mod1 <- hybridModel(AirPassengers)
#' plot(forecast(mod1))
#' mod2 <- hybridModel(AirPassengers, models = "aet",
#' weights = "insample.errors", errorMethod = "MASE")
#' mod3 <- hybridModel(AirPassengers, models = "aens",
#' a.args = list(max.p = 7, max.q = 7, approximation = FALSE))
#' # View the component auto.arima() and stlm() models
#' mod3$auto.arima
#' mod3$stlm
#' }
#'
hybridModel <- function(y, models = "aenst",
                        lambda = NULL,
                        a.args = NULL,
                        e.args = NULL,
                        n.args = NULL,
                        s.args = NULL,
                        t.args = NULL,
                        weights = c("equal", "insample.errors", "cv.errors"),
                        errorMethod = c("RMSE", "MAE", "MASE"),
                        parallel = FALSE, num.cores = 2L){
  # Weights could be set to equal (the default), based on in-sample errors, or based on cv errors
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

  # Check for problems for specific models (e.g. long seasonality for ets and non-seasonal for stlm or nnetar)
  if(is.element("e", expandedModels) && frequency(y) >=24){
    warning("frequency(y) >= 24. The ets model will not be used.")
    expandedModels <- expandedModels[expandedModels != "e"]
  }
  if(is.element("s", expandedModels)){
    if(frequency(y) < 2L){
      warning("The stlm model requires that the input data be a seasonal ts object. The stlm model will not be used.")
      expandedModels <- expandedModels[expandedModels != "s"]
    }
    if(frequency(y) * 2L >= length(y)){
       warning("The stlm model requres a series more than twice as long as the seasonal period. The stlm model will not be used.")
       expandedModels <- expandedModels[expandedModels != "s"]
    }
  }
  if(is.element("n", expandedModels)){
     if(frequency(y) * 2L >= length(y)){
        warning("The nnetar model requres a series more than twice as long as the seasonal period. The nnetar model will not be used.")
        expandedModels <- expandedModels[expandedModels != "n"]
     }
  }


  # A model run should include at least two component models
  if(length(expandedModels) < 2L){
    stop("A hybridModel must contain at least two component models.")
  }

  # Check for currently unimplemented features
  if(parallel){
    warning("The 'parallel' argument is currently unimplemented. Ignoring for now.")
  }
  if(weights == "cv.errors"){
    warning("Cross validated error weights are currently unimplemented. Ignoring for now.")
     weights <- "equal"
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

  # Apply the weights to construct the fitted values
  fits <- sapply(includedModels, FUN = function(x) fitted(modelResults[[x]]))
  fitsWeightsMatrix <- matrix(rep(modelResults$weights[includedModels], times = nrow(fits)),
                              nrow = nrow(fits), byrow = TRUE)
  fits <- rowSums(fits * fitsWeightsMatrix)
  resid <- y - fits
  if (!is.null(tsp(y))){
    fits <- ts(fits)
    resid <- ts(fits)
    tsp(fits) <- tsp(resid) <- tsp(y)
  }

  # Prepare the hybridModel object
  class(modelResults) <- "hybridModel"
  modelResults$frequency <- frequency(y)
  modelResults$x <- y
  modelResults$models <- includedModels
  modelResults$fitted <- fits
  modelResults$residuals <- resid
  return(modelResults)
}

#' Test if the object is a hybridModel object
#'
#' @export
#' @param x the input object.
#' @return A boolean indicating if the object is a \code{hybridModel} is returned.
#'
is.hybridModel <- function(x){
  inherits(x, "hybridModel")
}

#' Extract Model Fitted Values
#'
#' Extract the model fitted values from the \code{hybridModel} object.
#'
#' @param object the input hybridModel.
#' @param individual if \code{TRUE}, return the fitted values of the component models instead
#' of the fitted values for the whole ensemble model.
#' @param ... other arguments (ignored).
#' @seealso \code{\link{accuracy}}
#' @return The fitted values of the ensemble or individual component models.
#' @export
#'
fitted.hybridModel <- function(object,
                               individual = FALSE,
                               ...){
  if(individual){
    results <- list()
    for(i in object$models){
      results[[i]] <- fitted(object[[i]])
    }
    return(results)
  }
  return(object$fitted)
}

#' Extract Model Residuals
#'
#' Extract the model residuals from the \code{hybridModel} object.
#' @export
#' @param object The input hybridModel
#' @param individual If \code{TRUE}, return the residuals of the component models instead
#' of the residuals for the whole ensemble model.
#' @param ... Other arguments (ignored).
#' @seealso \code{\link{accuracy}}
#' @return The residuals of the ensemble or individual component models.
#'
residuals.hybridModel <- function(object,
                                  individual = FALSE,
                                  ...){
  if(individual){
    results <- list()
    for(i in object$models){
      results[[i]] <- residuals(object[[i]])
    }
    return(results)
  }
  return(object$residuals)
}


#' Generic method for accuracy
#'
#' @param f an object of class forecast, or a numerical vector containing forecasts.
#' It will also work with Arima, ets and lm objects if x is omitted - in which case
#' in-sample accuracy measures are returned.
#' @param ... other arguments (ignored)
#' @seealso \code{\link[forecast]{accuracy}}, \code{\link{accuracy.hybridModel}}
#' @export
#'
accuracy.default <-  function(f, ...){
   forecast::accuracy(f, ...)
}

#' Generic method for accuracy
#'
#' @param f the input object.
#' @param ... other arguments (ignored).
#' @seealso \code{\link[forecast]{accuracy}}, \code{\link{accuracy.hybridModel}}
#' @export
accuracy <- function(f,...){
  UseMethod("accuracy")
}

#' Accuracy measures for hybridModel objects
#'
#' Return the in-sample accuracy measures for the component models of the hybridModel
#'
#' @param f the input hybridModel.
#' @param individual if \code{TRUE}, return the accuracy of the component models instead
#' of the accuracy for the whole ensemble model.
#' @param ... other arguments (ignored).
#' @seealso \code{\link[forecast]{accuracy}}
#' @return The accuracy of the ensemble or individual component models.
#' @export
#'
accuracy.hybridModel <- function(f,
                                 individual = FALSE,
                                 ...){
  if(individual){
    results <- list()
    for(i in f$models){
      results[[i]] <- forecast::accuracy(f[[i]])
    }
    return(results)
  }
  return(forecast::accuracy(f$fitted, getResponse(f)))
}

#' Print a summary of the hybridModel object
#'
#' @param x the input \code{hybridModel} object.
#' @details Print the names of the individual component models and their weights.
#'
summary.hybridModel <- function(x){
  print(x)
}

#' Print information about the hybridModel object
#'
#' @param x the input \code{hybridModel} object.
#' @param ... other arguments (ignored).
#' @export
#' @details Print the names of the individual component models and their weights.
#'
print.hybridModel <- function(x, ...){
  cat("Hybrid forecast model comprised of the following models: ")
  cat(x$models, sep = ", ")
  cat("\n")
  for(i in x$models){
    cat("############\n")
    cat(i, "with weight", x$weights[i], "\n")
    #cat("############\n")
  }
}

#' Plot a hybridModel object
#'
#' Plot a representation of the hybridModel.
#'
#' @method plot hybridModel
#' @import forecast
#' @param x an object of class hybridModel to plot.
#' @param type if \code{type = "fit"}, plot the original series and the individual fitted models.
#' If \code{type = "models"}, use the regular plot methods from the component models, i.e.
#' \code{\link[forecast]{plot.Arima}}, \code{\link[forecast]{plot.ets}},
#' \code{\link[forecast]{plot.tbats}}. Note: no plot
#' methods exist for \code{nnetar} and \code{stlm} objects, so these will not be plotted with
#' \code{type = "models"}
#' @param ... other arguments (ignored).
#' @seealso \code{\link{hybridModel}}
#' @return None. Function produces a plot.
#' @details For \code{type = "fit"}, the original series is plotted in black.
#' Fitted values for the individual component models are plotted in other colors.
#' For \code{type = "models"}, each individual component model is plotted. Since
#' there is not plot method for \code{stlm} or \code{nnetar} objects, these component
#' models are not plotted.
#' @examples
#' \dontrun{
#' hm <- hybridModel(woolyrnq, models = "aenst")
#' plot(hm, type = "fit")
#' plot(hm, type = "models")
#' }
#' @export
#'
plot.hybridModel <- function(x,
                             type = c("fit", "models"),
                             ...){
  type <- match.arg(type)
  plotModels <- x$models
  if(type == "fit"){
    # Set the highest and lowest axis scale
    ymax <- max(sapply(plotModels, FUN = function(i) max(fitted(x[[i]]), na.rm = TRUE)))
    ymin <- min(sapply(plotModels, FUN = function(i) min(fitted(x[[i]]), na.rm = TRUE)))
    range <- ymax - ymin
    plot(x$x, ylim = c(ymin - 0.05 * range, ymax + 0.25 * range),
         ylab = "y", xlab = "time")
    title(main = "Plot of original series (black) and fitted component models", outer = TRUE)
    for(i in seq_along(plotModels)){
      lines(fitted(x[[plotModels[i]]]), col = i + 1)
    }
    legend("top", plotModels, fill = 2:(length(plotModels) + 1), horiz = TRUE)
  } else if(type == "models"){
    plotModels <- x$models[x$models != "stlm" & x$models != "nnetar"]
    for(i in seq_along(plotModels)){
      plot(x[[plotModels[i]]])
    }
  }
}
