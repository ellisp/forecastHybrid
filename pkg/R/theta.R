#' Theta method 'model'
#' 
#' Create a model object as an interim step to a theta method forecast.
#' 
#' @export
#' @param y A numeric vector or time series.
#' @details This fits an exponential smoothing state space model with \code{model = 'ANN'} to \code{y}, 
#' having first performed classic multiplicative seasonal adjustment.  A drift value is also calculated
#' by \code{lsfit(0:(length(y) - 1), y)$coef[2] / 2}.  In combination with \code{forecast.thetam()}, this provides
#' identical results to \code{forecast::thetaf(...)}.  The  purpose of splitting it into a `model` and 
#' `forecast` functions is to make the approach consistent with other modelling / forecasting approaches
#' used in \code{hybridModel()}.
#' @return An object of class \code{thetam}
#' @author Peter Ellis
#' @seealso \code{\link{forecast.thetam}}
thetam <- function(y){
   n <- length(y)
   m <- frequency(y)
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
   
   if(seasonal) {
      object$seasadj <- utils::tail(decomp$seasonal, m)
   }
   
   object$seasonal <- seasonal
   object$x <- origy
   object$drift <- stats::lsfit(0:(n - 1), y)$coef[2] / 2
   object$method <- "Theta"
   class(object) <- "thetam"
   
   return(object)
}

#' Forecast using a Theta model
#' 
#' Returns forecasts and other information for univariate Theta "models"
#' 
#' @export
#' @param object An object of class "\code{thetam}.  Usuall the result of a call to \code{link{thetam}}.
#' @param h Number of periods for forecasting
#' @param level Confidence level for prediction intervals
#' @param fan If TRUE, level is set to \code{seq(51, 99, by = 3)}. This is suitable for fan plots.
forecast.thetam <- function(object, h = ifelse(object$m > 1, 2 * object$m, 10), 
                            level = c(80, 95), fan = FALSE, ...){
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
   fcast$mean <- fcast$mean + object$drift * (0:(h - 1) + (1 - (1 - alpha) ^ length(object$x)) / alpha)
   if(object$seasonal){
      fcast$mean <- fcast$mean * rep(object$seasadj, trunc(1 + h / object$m))[1:h]
   }
   fcast.se <- sqrt(fcast$model$sigma) * sqrt((0:(h - 1)) * alpha ^ 2 + 1)
   nconf <- length(level)
   fcast$lower <- fcast$upper <- matrix(NA, nrow = h, ncol = nconf)
   for (i in 1:nconf) {
      zt <- -qnorm(0.5 - level[i] / 200)
      fcast$lower[, i] <- fcast$mean - zt * fcast.se
      fcast$upper[, i] <- fcast$mean + zt * fcast.se
   }
   return(fcast)
}


