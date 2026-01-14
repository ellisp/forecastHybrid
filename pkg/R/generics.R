#' Test if the object is a hybridModel object
#'
#' Test if the object is a \code{hybridModel} object.
#'
#' @export
#' @param x the input object.
#' @return A boolean indicating if the object is a \code{hybridModel} is returned.
#'
is.hybridModel <- function(x) { # nolint
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
#' @seealso \code{\link[forecast]{accuracy}}
#' @return The fitted values of the ensemble or individual component models.
#' @export
#'
fitted.hybridModel <- function(object,
                               individual = FALSE,
                               ...) {
  chkDots(...)
  if (individual) {
    results <- list()
    for (model in object$models) {
      results[[model]] <- fitted(object[[model]])
    }
    return(results)
  }
  object$fitted
}

#' Extract Model Residuals
#'
#' Extract the model residuals from the \code{hybridModel} object.
#' @export
#' @param object The input hybridModel.
#' @param individual If \code{TRUE}, return the residuals of the component models instead
#' of the residuals for the whole ensemble model.
#' @param ... Other arguments (ignored).
#' @seealso \code{\link[forecast]{accuracy}}
#' @return The residuals of the ensemble or individual component models.
#'
residuals.hybridModel <- function(object,
                                  individual = FALSE,
                                  ...) {
  chkDots(...)
  if (individual) {
    results <- list()
    for (model in object$models) {
      results[[model]] <- residuals(object[[model]])
    }
    return(results)
  }
  object$residuals
}


#' Accuracy measures for hybridModel objects
#'
#' Accuracy measures for hybridModel
#' objects.
#'
#' Return the in-sample accuracy measures for the component models of the hybridModel
#'
#' @param object the input hybridModel.
#' @param individual if \code{TRUE}, return the accuracy of the component models instead
#' of the accuracy for the whole ensemble model.
#' @param f Deprecated. Please use `object` instead.
#' @param ... other arguments (ignored).
#' @seealso \code{\link[forecast]{accuracy}}
#' @return The accuracy of the ensemble or individual component models.
#' @export
#'
#' @author David Shaub
#'
accuracy.hybridModel <- function(object, # nolint
                                 individual = FALSE,
                                 ...,
                                 f = NULL) {
  chkDots(...)
  if (!is.null(f)) {
    warning("Using `f` as the argument for `accuracy()` is deprecated.",
            "Please use `object` instead.", call. = FALSE)
    object <- f
  }
  if (individual) {
    results <- list()
    for (model in object$models) {
      results[[model]] <- forecast::accuracy(object[[model]])
    }
    return(results)
  }
  forecast::accuracy(object$fitted, getResponse(object))
}

#' Accuracy measures for cross-validated time series
#'
#' Returns range of summary measures of the cross-validated forecast accuracy
#' for \code{cvts} objects.
#'
#' @param object a \code{cvts} objected created by \code{\link{cvts}}.
#' @param f Deprecated. Please use `object` instead.
#' @param ... other arguments (ignored).
#'
#' @details
#' Currently the method only implements \code{ME}, \code{RMSE}, and \code{MAE}. The accuracy
#' measures \code{MPE}, \code{MAPE}, and \code{MASE} are not calculated. The accuracy
#' is calculated for each forecast horizon up to \code{maxHorizon}
#' @export
#' @author David Shaub
#'
accuracy.cvts <- function(object, # nolint
                          ...,
                          f = NULL) {
  chkDots(...)
  if (!is.null(f)) {
    warning("Using `f` as the argument for `accuracy()` is deprecated.",
            " Please use `object` instead.", call. = FALSE)
    object <- f
  }
  ME <- colMeans(object$residuals) # nolint
  RMSE <- apply(object$residuals, MARGIN = 2, # nolint
                FUN = function(x) sqrt(sum(x ^ 2) / length(x)))
  MAE <- colMeans(abs(object$residuals)) # nolint
  results <- data.frame(ME, RMSE, MAE)
  rownames(results) <- paste("Forecast Horizon ", rownames(results))
  # MASE TODO
  # Will require actual/fitted/residuals
  results
}


#' Print a summary of the hybridModel object
#'
#' @param x the input \code{hybridModel} object.
#' @details Print the names of the individual component models and their weights.
#'
#'
summary.hybridModel <- function(x) {
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
print.hybridModel <- function(x,
                              ...) {
  chkDots(...)
  cat("Hybrid forecast model comprised of the following models: ")
  cat(x$models, sep = ", ")
  cat("\n")
  for (model in x$models) {
    cat("############\n")
    cat(model, "with weight", round(x$weights[model], 3), "\n")
  }
}

#' Plot the component models of a hybridModel object
#'
#' Plot a representation of the hybridModel.
#' @param x an object of class hybridModel to plot.
#' @param ggplot should the \code{\link[forecast]{autoplot}} function be used (when available)
#' for the plots?
#' @param ... other arguments passed to \link{plot}.
#' @importFrom ggplot2 autoplot ggplot
plotModelObjects <- function(x,
                             ggplot,
                             ...) {
  chkDots(...)
  plotModels <- x$models[x$models != "stlm" & x$models != "nnetar"]
  for (i in seq_along(plotModels)) {
    # bats, tbats, and nnetar aren't supported by autoplot
    if (ggplot && !(plotModels[i] %in% c("tbats", "bats", "nnetar"))) {
      autoplot(x[[plotModels[i]]])
    } else if (!ggplot) {
      plot(x[[plotModels[i]]])
    }
  }
}

#' Plot the fitted values of a hybridModel object
#'
#' Plot a fitted values of the hybridModel.
#' @param x an object of class hybridModel to plot.
#' @param ggplot should the \code{\link[forecast]{autoplot}} function be used (when available)
#' for the plots?
#' @param ... other arguments passed to \link{plot}.
#' @importFrom ggplot2 ggplot aes geom_line scale_y_continuous
plotFitted <- function(x,
                       ggplot,
                       ...) {
  chkDots(...)
  plotModels <- x$models
  if (ggplot) {
    plotFrame <- data.frame(matrix(0, nrow = length(x$x), ncol = 0))
    for (i in plotModels) {
      plotFrame[i] <- fitted(x[[i]])
    }
    names(plotFrame) <- plotModels
    plotFrame$date <- as.Date(time(x$x))
    # Appease R CMD check for undeclared variable
    variable <- NULL
    value <- NULL
    # If anyone knows a cleaner way to transform this "wide" data to "long" data for plotting
    # with ggplot2 without using additional packages, let me know.
    pfTransform <- matrix(as.matrix(plotFrame[, plotModels]), ncol = 1)
    pfTransform <- data.frame(date = plotFrame$date,
                              variable = factor(rep(plotModels,
                                                    each = nrow(plotFrame)),
                                                levels = plotModels),
                              value = pfTransform)
    plotFrame <- pfTransform[order(pfTransform$variable, pfTransform$date), ]
    ggplot(data = plotFrame,
           aes(x = date, y = as.numeric(value), col = variable)) +
      geom_line() + scale_y_continuous(name = "y")
  } else {
    # Set the highest and lowest axis scale
    ymax <- max(sapply(plotModels,
                       FUN = function(i) max(fitted(x[[i]]), na.rm = TRUE)))
    ymin <- min(sapply(plotModels,
                       FUN = function(i) min(fitted(x[[i]]), na.rm = TRUE)))
    yRange <- ymax - ymin
    plot(x$x, ylim = c(ymin - 0.05 * yRange, ymax + 0.25 * yRange), ...)
    for (i in seq_along(plotModels)) {
      lines(fitted(x[[plotModels[i]]]), col = i + 1)
    }
    legend("top", plotModels, fill = 2:(length(plotModels) + 1), horiz = TRUE)
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
#' @param ggplot should the \code{\link[forecast]{autoplot}} function
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
plot.hybridModel <- function(x,
                             type = c("fit", "models"),
                             ggplot = FALSE,
                             ...) {
  type <- match.arg(type)
  chkDots(...)
  if (type == "fit") {
    plotFitted(x = x, ggplot = ggplot)
  } else if (type == "models") {
    plotModelObjects(x = x, ggplot = ggplot)
  }
}
