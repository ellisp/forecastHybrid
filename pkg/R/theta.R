#' Theta method 'model'
#'
#' Create a model object as an interim step to a theta method forecast.
#'
#' @export
#' @param y A numeric vector or time series.
#' @details This fits an exponential smoothing state space model with
#' \code{model = 'ANN'} to \code{y}, having first performed classic multiplicative
#' seasonal adjustment.  A drift value is also calculated by
#' \code{lsfit(0:(length(y) - 1), y)$coef[2] / 2}.  In combination with \code{forecast.thetam()},
#' this provides identical results to \code{forecast::thetaf(...)}.  The  purpose of splitting
#' it into a `model` and `forecast` functions is to make the approach consistent with other
#' modeling / forecasting approaches used in \code{hybridModel()}.
#' @return An object of class \code{thetam}
#' @author Peter Ellis
#' @examples
#' mod1 <- thetam(Nile)
#' plot(mod1)
#' @seealso \code{\link{forecast.thetam}}
thetam <- function(y) {
   if (any(class(y) %in% c("data.frame", "list", "matrix", "mts"))) {
      stop("y should be a univariate time series")
   }
   if (!is.numeric(y)) {
      stop("y should be numeric")
   }
   y <- as.ts(y)

   n <- length(y)
   m <- frequency(y)
   if (n <= m) {
      stop("there is not enough data to run the theta model")
   }
   if (m > 1) {
      r <- as.numeric(stats::acf(y, lag.max = m, plot = FALSE)$acf)[-1]
      stat <- sqrt((1 + 2 * sum(r[-m] ^ 2)) / n)
      seasonal <- (abs(r[m]) / stat > stats::qnorm(0.95))
   }  else {
      seasonal <- FALSE
   }

   origy <- y

   if (seasonal) {
      decomp <- stats::decompose(y, type = "multiplicative")
      y <- forecast::seasadj(decomp)
   }

   object <- forecast::ets(y, model = "ANN", opt.crit = "mse")

   if (seasonal) {
      object$seasadj <- utils::tail(decomp$seasonal, m)
      object$seasadjhist <- decomp$seasona
   }

   object$seasonal <- seasonal
   object$x <- origy
   object$drift <- stats::lsfit(0:(n - 1), y)$coef[2] / 2
   object$method <- "Theta"
   class(object) <- c("thetam", "ets")

   return(object)
}

#' Forecast using a Theta model
#'
#' Returns forecasts and other information for univariate Theta "models"
#'
#' @export
#' @param object An object of class "\code{thetam}.  Usually the result of a call
#' to \code{link{thetam}}.
#' @param h Number of periods for forecasting
#' @param level Confidence level for prediction intervals
#' @param fan If TRUE, level is set to \code{seq(51, 99, by = 3)}. This is suitable for fan plots.
#' @param ... Ignored
#' @return An object of class \code{forecast}
#' @examples
#' mod1 <- thetam(Nile)
#' fc1 <- forecast(mod1)
#' plot(fc1)
#' @author Peter Ellis
#' @seealso \code{\link{thetam}}
forecast.thetam <- function(object, # nolint
                            h = ifelse(object$m > 1, 2 * object$m, 10),
                            level = c(80, 95),
                            fan = FALSE, ...) {
   chkDots(...)
   if (fan) {
      level <- seq(51, 99, by = 3)
   } else {
      if (min(level) > 0 & max(level) < 1)
         level <- 100 * level
      else if (min(level) < 0 | max(level) > 99.99)
         stop("Confidence limit out of range")
   }
   fcast <- forecast.ets(object, h = h, level = level, fan = fan)
   alpha <- fcast$model$par["alpha"]
   update <- object$drift * (0:(h - 1) + (1 - (1 - alpha) ^ length(object$x)) / alpha)
   fcast$mean <- fcast$mean + update
   if (object$seasonal) {
      fcast$mean <- fcast$mean * rep(object$seasadj, trunc(1 + h / object$m))[1:h]
   }
   fcastSE <- sqrt(fcast$model$sigma) * sqrt((0:(h - 1)) * alpha ^ 2 + 1)
   nconf <- length(level)
   fcast$lower <- fcast$upper <- matrix(NA, nrow = h, ncol = nconf)
   for (i in 1:nconf) {
      zt <- -qnorm(0.5 - level[i] / 200)
      fcast$lower[, i] <- fcast$mean - zt * fcastSE
      fcast$upper[, i] <- fcast$mean + zt * fcastSE
   }
   return(fcast)
}

#' Plot components from Theta model
#'
#' Produces a plot of the level components from the ETS model underlying a Theta model
#' @export
#' @param x Object of class "thetam".
#' @param ... Other plotting parameters passed through to \code{plot}
#' @method plot thetam
#' @return None.  Function produces a plot.
#' @details The "state" component of the plot comes from the model \code{ets(..., model = "ANN")}
#' that was fit as part of the theta method.  The "seasonal" component is the multipliers
#' from multiplicative classical
#' decomposition seasonal adjustment that is performed before the \code{ets} model is fit.
#' The "linear" component shows the direction and slope of drift that is used in the forecasting
#' to come.
#' @examples
#' model <- thetam(wineind)
#' plot(model)
#' @author Peter Ellis
#' @seealso \code{\link{thetam}}
plot.thetam <- function(x, ...) {
   chkDots(...)
   # Note that "states" from an object created by ets is one element longer than observed data.
   y <- x$x
   n <- length(y)
   alpha <- x$par["alpha"]
   linear <- x$drift * (0:(n - 1) + (1 - (1 - alpha) ^ length(x$x)) / alpha)
   if (x$seasonal) {
      plotdata <- cbind(
         observed = y,
         state = x$states[-1, 1],
         seasonal = x$seasadjhist,
         linear = linear
   )} else {
      plotdata <- cbind(
         observed = y,
         state = x$states[-1, 1],
         linear = linear
      )
   }
   plot(plotdata, main = paste("Decomposition by Theta method"), ...)
}
