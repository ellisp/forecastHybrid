#' Hybrid time series modelling
#'
#' Create a hybrid time series model with two to five component models.
#'
#' @export
#' @import forecast
#' @import stats
#' @import graphics
#' @import zoo
#' @param y A numeric vector or time series.
#' @param lambda
#' Box-Cox transformation parameter.
#' Ignored if NULL. Otherwise, data transformed before model is estimated.
#' @param models A character string of up to six characters indicating which contributing models to use:
#' a (\code{\link[forecast]{auto.arima}}), e (\code{\link[forecast]{ets}}),
#' f (\code{\link{thetam}}), n (\code{\link[forecast]{nnetar}}),
#' s (\code{\link[forecast]{stlm}}) and t (\code{\link[forecast]{tbats}}).
#' @param a.args an optional \code{list} of arguments to pass to \code{\link[forecast]{auto.arima}}. See details.
#' @param e.args an optional \code{list} of arguments to pass to \code{\link[forecast]{ets}}. See details.
#' @param n.args an optional \code{list} of arguments to pass to \code{\link[forecast]{nnetar}}. See details.
#' @param s.args an optional \code{list} of arguments to pass to \code{\link[forecast]{stlm}}. See details.
#' @param t.args an optional \code{list} of arguments to pass to \code{\link[forecast]{tbats}}. See details.
#' @param weights method for weighting the forecasts of the various contributing
#' models.  Defaults to \code{equal}, which has shown to be robust and better
#' in many cases than giving more weight to models with better in-sample performance. Cross validated errors--implemented with \code{link{cvts}}
#' should produce the best forecast, but the model estimation is also the slowest. Note that extra arguments
#' passed in \code{a.args}, \code{e.args}, \code{n.args}, \code{s.args}, and \code{t.args} are not used
#' during cross validation. See further explanation in \code{\link{cvts}}.
#' Weights utilizing in-sample errors are also available but not recommended.
#' @param errorMethod  method of measuring accuracy to use if weights are not
#' to be equal.
#' Root mean square error (\code{RMSE}), mean absolute error (\code{MAE})
#' and mean absolute scaled error (\code{MASE})
#' are supported.
#' @param parallel a boolean indicating if parallel processing should be used between models.
#' This is currently unimplemented.
#' Parallelization will still occur within individual models that suport it and can be controlled using \code{a.args} and \code{t.args}.
#' @param num.cores If \code{parallel=TRUE}, how many cores to use.
#' @param cvHorizon If \code{weights = "cv.errors"}, this controls which forecast to horizon to use
#' for the error calculations.
#' @param windowSize length of the window to build each model, only used when \code{weights = "cv.errors"}.
#' @param horizonAverage If \code{weights = "cv.errors"}, setting this to \code{TRUE} will average
#' all forecast horizons up to \code{cvHorizon} for calculating the errors instead of using
#' the single horizon given in \code{cvHorizon}.
#' @param verbose Should the status of which model is being fit/cross validated be printed to the terminal?
#' @seealso \code{\link{forecast.hybridModel}}, \code{\link[forecast]{auto.arima}},
#' \code{\link[forecast]{ets}}, \code{\link{thetam}}, \code{\link[forecast]{nnetar}},
#' \code{\link[forecast]{stlm}}, \code{\link[forecast]{tbats}}
#' @return An object of class hybridModel.
#' The individual component models are stored inside of the object
#' and can be accessed for all the regular manipulations available in the forecast package.
#' @details The \code{hybridModel} function fits multiple individual model specifications to allow easy creation
#' of ensemble forecasts. While default settings for the individual component models work quite well
#' in most cases, fine control can be exerted by passing detailed arguments to the component models in the
#' \code{a.args}, \code{e.args}, \code{n.args}, \code{s.args}, and \code{t.args} lists.
#' Note that if \code{xreg} is passed to the \code{a.args}, \code{n.args}, or \code{s.args} component models
#' it must be passed as a dataframe instead of the matrix object
#' that the "forecast" package functions usually accept.
#' This is due to a limitation in how the component models are called.
#' \cr
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
#' # Fit an auto.arima, ets, thetam, nnetar, stlm, and tbats model
#' # on the time series with equal weights
#' mod1 <- hybridModel(AirPassengers)
#' plot(forecast(mod1))
#'
#' # Use an auto.arima, ets, and tbats model with weights
#' # set by the MASE in-sample errors
#' mod2 <- hybridModel(AirPassengers, models = "aet",
#' weights = "insample.errors", errorMethod = "MASE")
#'
#' # Pass additional arguments to auto.arima() to control its fit
#' mod3 <- hybridModel(AirPassengers, models = "aens",
#' a.args = list(max.p = 7, max.q = 7, approximation = FALSE))
#'
#' # View the component auto.arima() and stlm() models
#' mod3$auto.arima
#' mod3$stlm
#' }
#'
#' @author David Shaub
#'
hybridModel <- function(y, models = "aefnst",
                        lambda = NULL,
                        a.args = NULL,
                        e.args = NULL,
                        n.args = NULL,
                        s.args = NULL,
                        t.args = NULL,
                        weights = c("equal", "insample.errors", "cv.errors"),
                        errorMethod = c("RMSE", "MAE", "MASE"),
                        cvHorizon = frequency(y),
                        windowSize = 84,
                        horizonAverage = FALSE,
                        parallel = FALSE, num.cores = 2L,
                        verbose = TRUE){
  # Weights could be set to equal (the default), based on in-sample errors, or based on cv errors
  # errorMethod will determine which type of errors to use for weights. Some choices from accuracy()
  # are not appropriate. If weights = "equal", this would be ignored.

  # The dependent variable must be numeric and not a matrix/dataframe
  if(!is.numeric(y) || !is.null(dim(y))){
    stop("The time series must be numeric and may not be a matrix or dataframe object.")
  }
  if(!length(y)){
    stop("The time series must have observations")
  }

   if(length(y) < 4){
      stop("The time series must have at least four observations")
   }

  y <- as.ts(y)

  # Match arguments to ensure validity
  weights <- match.arg(weights)
  if(weights == "insample.errors"){
     warning("Using insample.error weights is not recommended for accuracy and may be deprecated in the future.")
  }
  errorMethod <- match.arg(errorMethod)

  # Match the specified models
  expandedModels <- unique(tolower(unlist(strsplit(models, split = ""))))
  if(length(expandedModels) > 6L){
    stop("Invalid models specified.")
  }
  # All characters must be valid
  if(!(all(expandedModels %in% c("a", "e", "f", "n", "s", "t")))){
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
    warning("ets was not selected in the models argument, but e.args was passed. Ignoring e.args")
  }
  if(!is.null(n.args) && !is.element("n", expandedModels)){
    warning("nnetar was not selected in the models argument, but n.args was passed. Ignoring n.args")
  }
  if(!is.null(s.args) && !is.element("s", expandedModels)){
    warning("stlm was not selected in the models argument, but s.args was passed. Ignoring s.args")
  }
  if(!is.null(t.args) && !is.element("t", expandedModels)){
    warning("tbats was not selected in the models argument, but t.args was passed. Ignoring t.args")
  }

  # Check for problems for specific models (e.g. long seasonality for ets and non-seasonal for stlm or nnetar)
  if(is.element("e", expandedModels) && frequency(y) >=24){
    warning("frequency(y) >= 24. The ets model will not be used.")
    expandedModels <- expandedModels[expandedModels != "e"]
  }
  if(is.element("f", expandedModels) && frequency(y) >=24){
     warning("frequency(y) >= 24. The Theta model will not be used.")
     expandedModels <- expandedModels[expandedModels != "f"]
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
  # if(weights == "cv.errors"){
  #   warning("Cross validated error weights are currently unimplemented. Ignoring for now.")
  #   weights <- "equal"
  # }
  if(weights == "cv.errors" && errorMethod == "MASE"){
    warning("cv errors currently do not support MASE. Reverting to RMSE.")
    errorMethod <- "RMSE"
  }

  modelResults <- list()

  # We would allow for these models to run in parallel at the model level rather than within the model
  # since this has better performance. As an enhancement, users with >4 cores could benefit by running
  # parallelism both within and between models, based on the number of available cores.
  # auto.arima()
  if(is.element("a", expandedModels)){
    if(verbose){
      cat("Fitting the auto.arima model\n")
    }
    if(is.null(a.args)){
      a.args <- list(lambda = lambda)
    } else if(is.null(a.args$lambda)){
      a.args$lambda <- lambda
    }
    modelResults$auto.arima <- do.call(auto.arima, c(list(y), a.args))
  }
  # ets()
  if(is.element("e", expandedModels)){
    if(verbose){
      cat("Fitting the ets model\n")
    }
    if(is.null(e.args)){
      e.args <- list(lambda = lambda)
    } else if(is.null(e.args$lambda)){
      e.args$lambda <- lambda
    }
    modelResults$ets <- do.call(ets, c(list(y), e.args))
  }
  # thetam()
  if(is.element("f", expandedModels)){
     if(verbose){
        cat("Fitting the thetam model\n")
     }
     modelResults$thetam <- thetam(y)
  }
  # nnetar()
  if(is.element("n", expandedModels)){
    if(verbose){
      cat("Fitting the nnetar model\n")
    }
    if(is.null(n.args)){
      n.args <- list(lambda = lambda)
    } else if(is.null(n.args$lambda)){
      n.args$lambda <- lambda
    }
    modelResults$nnetar <- do.call(nnetar, c(list(y), n.args))
  }
  # stlm()
  if(is.element("s", expandedModels)){
    if(verbose){
      cat("Fitting the stlm model\n")
    }
    if(is.null(s.args)){
      s.args <- list(lambda = lambda)
    } else if(is.null(s.args$lambda)){
      s.args$lambda <- lambda
    }
    modelResults$stlm <- do.call(stlm, c(list(y), s.args))
  }
  # tbats()
  if(is.element("t", expandedModels)){
    if(verbose){
      cat("Fitting the tbats model\n")
    }
    modelResults$tbats <- do.call(tbats, c(list(y), t.args))
  }

  # Set the model weights
  includedModels <- names(modelResults)
  # Weighting methods would go here, equal weighting for now
  if(weights == "equal"){
    modelResults$weights <- rep(1 / length(expandedModels), length(expandedModels))
  } else if(weights %in% c("insample.errors", "cv.errors")){

    # There is probably a better way of accomplishing this
    # But this ugly approach will work for now
    # These loops and if statements can be replace
    # with do.call and/or map
    modelResults$weights <- rep(0, length(expandedModels))
    index <- 1
    modResults <- modelResults

    if(weights == "cv.errors"){
      #modResults <-
      for(i in expandedModels){
        if(i == "a"){
          if(verbose){
            cat("Cross validating the auto.arima model\n")
          }
          modResults$auto.arima <- cvts(y, FUN = auto.arima,
                                        maxHorizon = cvHorizon,
                                        horizonAverage = horizonAverage,
                                        verbose = FALSE,
                                        windowSize = windowSize)
        } else if(i == "e"){
          if(verbose){
            cat("Cross validating the ets model\n")
          }
          modResults$ets <- cvts(y, FUN = ets,
                                 maxHorizon = cvHorizon,
                                 horizonAverage = horizonAverage,
                                 verbose = FALSE,
                                 windowSize = windowSize)
        } else if(i == "f"){
           if(verbose){
              cat("Cross validating the thetam model\n")
           }
           modResults$thetam <- cvts(y, FUN = thetam,
                                  maxHorizon = cvHorizon,
                                  horizonAverage = horizonAverage,
                                  verbose = FALSE,
                                  windowSize = windowSize)
        } else if(i == "n"){
          if(verbose){
            cat("Cross validating the nnetar model\n")
          }
          modResults$nnetar <- cvts(y, FUN = nnetar,
                                    maxHorizon = cvHorizon,
                                    horizonAverage = horizonAverage,
                                    verbose = FALSE,
                                    windowSize = windowSize)
        } else if(i == "s"){
          if(verbose){
            cat("Cross validating the stlm model\n")
          }
          modResults$stlm <- cvts(y, FUN = stlm,
                                  maxHorizon = cvHorizon,
                                  horizonAverage = horizonAverage,
                                  verbose = FALSE,
                                  windowSize = windowSize)
        } else if(i == "t"){
          if(verbose){
            cat("Cross validating the tbats model\n")
          }
          modResults$tbats <- cvts(y, FUN = tbats,
                                   maxHorizon = cvHorizon,
                                   horizonAverage = horizonAverage,
                                   verbose = FALSE,
                                   windowSize = windowSize)
        }
      }
    }
    # If horizonAverage == TRUE, the resulting accuracy object will have only one row
    cvHorizon <- ifelse(horizonAverage, 1, cvHorizon)
    cvHorizon <- ifelse(weights != "cv.errors", 1, cvHorizon)
    for(i in expandedModels){
      if(i == "a"){
        modelResults$weights[index] <- accuracy(modResults$auto.arima)[cvHorizon, errorMethod]
      } else if(i == "e"){
        modelResults$weights[index] <- accuracy(modResults$ets)[cvHorizon, errorMethod]
      } else if(i == "f"){
         modelResults$weights[index] <- accuracy(modResults$thetam)[cvHorizon, errorMethod]
      } else if(i == "n"){
        modelResults$weights[index] <- accuracy(modResults$nnetar)[cvHorizon, errorMethod]
      } else if(i == "s"){
        modelResults$weights[index] <- accuracy(modResults$stlm)[cvHorizon, errorMethod]
      } else if(i == "t"){
        modelResults$weights[index] <- accuracy(modResults$tbats)[cvHorizon, errorMethod]
      }
      index <- index + 1
    }
    # Scale the weights
    modelResults$weights <- (1 / modelResults$weights) / sum(1 / modelResults$weights)

  }

  # Check for valid weights when weights = "insample.errors" and submodels produce perfect fits
  if(is.element(NaN, modelResults$weights) & weights %in% c("insample.errors", "cv.errors")){
    warning('At least one model perfectly fit the series, so accuracy measures cannot be used for weights. Reverting to weights = "equal".')
    modelResults$weights <- rep(1/ length(includedModels), length(includedModels))
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
#' Test if the object is a \code{hybridModel} object.
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
  #chkDots(...)
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
#' @param object The input hybridModel.
#' @param individual If \code{TRUE}, return the residuals of the component models instead
#' of the residuals for the whole ensemble model.
#' @param ... Other arguments (ignored).
#' @seealso \code{\link{accuracy}}
#' @return The residuals of the ensemble or individual component models.
#'
residuals.hybridModel <- function(object,
                                  individual = FALSE,
                                  ...){
  #chkDots(...)
  if(individual){
    results <- list()
    for(i in object$models){
      results[[i]] <- residuals(object[[i]])
    }
    return(results)
  }
  return(object$residuals)
}


#' Accuracy measures for hybridModel objects
#'
#' Accuracy measures for hybridModel
#' objects.
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
#' @author David Shaub
#'
accuracy.hybridModel <- function(f,
                                 individual = FALSE,
                                 ...){
  #chkDots(...)
  if(individual){
    results <- list()
    for(i in f$models){
      results[[i]] <- forecast::accuracy(f[[i]])
    }
    return(results)
  }
  return(forecast::accuracy(f$fitted, getResponse(f)))
}

#' Accuracy measures for cross-validated time series
#'
#' Returns range of summary measures of the cross-validated forecast accuracy
#' for \code{cvts} objects.
#'
#' @param f a \code{cvts} objected created by \code{\link{cvts}}.
#' @param ... other arguments (ignored).
#'
#' @details
#' Currently the method only implements \code{ME}, \code{RMSE}, and \code{MAE}. The accuracy measures
#' \code{MPE}, \code{MAPE}, and \code{MASE} are not calculated. The accuracy is calculated for each
#' forecast horizon up to \code{maxHorizon}
#' @export
#' @author David Shaub
#' 
accuracy.cvts <- function(f, ...){
  ME <- colMeans(f$residuals)
  RMSE <- apply(f$residuals, MARGIN = 2, FUN = function(x){sqrt(sum(x ^ 2)/ length(x))})
  MAE <- colMeans(abs(f$residuals))
  results <- data.frame(ME, RMSE, MAE)
  rownames(results) <- paste("Forecast Horizon ", rownames(results))
  return(results)
  # MASE TODO
  # Will require actual/fitted/residuals
}


#' Print a summary of the hybridModel object
#'
#' @param x the input \code{hybridModel} object.
#' @details Print the names of the individual component models and their weights.
#'
#'
summary.hybridModel <- function(x){
  print(x)
}

#' Print information about the hybridModel object
#'
#' Print information about the \code{hybridModel} object.
#'
#' @param x the input \code{hybridModel} object.
#' @param ... other arguments (ignored).
#' @export
#' @details Print the names of the individual component models and their weights.
#'
print.hybridModel <- function(x, ...){
  #chkDots(...)
  cat("Hybrid forecast model comprised of the following models: ")
  cat(x$models, sep = ", ")
  cat("\n")
  for(i in x$models){
    cat("############\n")
    cat(i, "with weight", round(x$weights[i], 3), "\n")
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
#' \code{type = "models"}.
#' @param ggplot should the \code{\link{autoplot}} function
#' be used (when available) for the plots?
#' @param ... other arguments passed to \link{plot}.
#' @seealso \code{\link{hybridModel}}
#' @return None. Function produces a plot.
#'
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
#' @author David Shaub
#' @importFrom ggplot2 ggplot aes autoplot geom_line scale_y_continuous
#'
plot.hybridModel <- function(x,
                             type = c("fit", "models"),
                             ggplot = FALSE,
                             ...){
   type <- match.arg(type)
   #chkDots(...)
   plotModels <- x$models
   if(type == "fit"){
      if(ggplot){
         plotFrame <- data.frame(matrix(0, nrow = length(x$x), ncol = 0))
         for(i in plotModels){
            plotFrame[i] <- fitted(x[[i]])
         }
         names(plotFrame) <- plotModels
         plotFrame$date <- as.Date(time(x$x))
         # Appease R CMD check for undeclared variable, value
         variable <- NULL
         value <- NULL
         plotFrame <- reshape2::melt(plotFrame, id = "date")
         ggplot(data = plotFrame, 
                aes(x = date, y = as.numeric(value), col = variable)) +
            geom_line() + scale_y_continuous(name = "y")
         
      } else{
         # Set the highest and lowest axis scale
         ymax <- max(sapply(plotModels, FUN = function(i) max(fitted(x[[i]]), na.rm = TRUE)))
         ymin <- min(sapply(plotModels, FUN = function(i) min(fitted(x[[i]]), na.rm = TRUE)))
         range <- ymax - ymin
         plot(x$x, ylim = c(ymin - 0.05 * range, ymax + 0.25 * range), ...)
         #title(main = "Plot of original series (black) and fitted component models", outer = TRUE)
         for(i in seq_along(plotModels)){
            lines(fitted(x[[plotModels[i]]]), col = i + 1)
         }
         legend("top", plotModels, fill = 2:(length(plotModels) + 1), horiz = TRUE)
      }
   } else if(type == "models"){
      plotModels <- x$models[x$models != "stlm" & x$models != "nnetar"]
      for(i in seq_along(plotModels)){
         # tbats isn't supported by autoplot
         if(ggplot && !(plotModels[i] %in% c("tbats", "bats", "nnetar"))){
            autoplot(x[[plotModels[i]]])
         } else if(!ggplot){
            plot(x[[plotModels[i]]])
         }
      }
   }
}
