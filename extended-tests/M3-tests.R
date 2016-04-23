# adapted from http://robjhyndman.com/m3comparisons.R
# This is an extended test of the hybridForecast approach
# on 3,003 series; partly to identify bugs, partly to 
# assess relative performance

library(Mcomp)


num_series <- length(M3) # ie 3003
#num_series <- 10 # while developing

# 26 methods possible
all_methods <- c(
   "ae", "an", "as", "at", "en", "es", "et", "ns", "nt", "st",
   "aen", "aes", "aet", "ans", "ant", "ast", "ens", "ent", "est", "nst",
   "aens", "aent", "aest", "anst", "enst", 
   "aenst"
   )
k <- length(all_methods) # ie 26 methods


# set up list of k empty matrices with 18 columns to hold the results
# (predicting 18 periods out for each series)
forecasts <- list()
for(j in 1:k){
   forecasts[[j]] <- matrix(NA, nrow = num_series, ncol = 18)
}


# fit models
for(i in 1:num_series){
   cat("\n", i, "\n")        # let me know how it's going as it loops through...
   series <- M3[[i]]
   x <- series$x      # ie the data to be fitted
   
   for(j in 1:k){
      # stlm only works for seasonal data, so skip any hybrids containing stlm if not seasonal:
      if(M3[[i]]$period == "YEARLY" & any(grepl("s", all_methods[j]))){
         next()
      } else {
         cat(j, " ")
         forecasts[[j]][i, ] <- forecast(hybridModel(x, models = all_methods[j]), h = 18)$mean
      }
   }
}



# compute accuracy
mase <- mape <- smape <- matrix(NA, nrow = k, ncol = num_series)
for(i in 1:num_series){
   x <- M3[[i]]$xx
   n <- length(x)
   scale <- mean(abs(diff(M3[[i]]$x, lag = frequency(x))))
   for(j in 1:k)
   {
      mape[j, i] <- mean(abs((x - forecasts[[j]][i, 1:n]) / x)) * 100
      smape[j, i] <- mean(abs(x - forecasts[[j]][i, 1:n]) / (abs(x) + abs(forecasts[[j]][i, 1:n]))) * 200
      mase[j, i] <- mean(abs(x - forecasts[[j]][i, 1:n]) / scale)
   }
}



m3table <- matrix(NA, nrow = k, ncol = 3)
m3table[ ,1] <- rowMeans(smape, na.rm = TRUE)
m3table[ ,2] <- rowMeans(mape, na.rm = TRUE)
m3table[ ,3] <- rowMeans(mase, na.rm = TRUE)

m3df <- data.frame(m3table)
names(m3df) <- c("smape", "mape", "mase")
m3df$method <- all_methods
m3df



# all models with t in them failed for series 2733 in the original run but this
# is a bug that's fixed now


# series <- M3[[2733]]
# x <- series$x    
# plot(x)
# series$description
# series$period
# series$st
# frequency(x)
# 
# fc <- forecast(hybridModel(x, models = "aet"), h = 18)
# plot(fc)
# fc$mean
