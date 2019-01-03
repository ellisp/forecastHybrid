# Unit tests on the hybridModel function
if(require(forecast) &  require(testthat)){
  context("Testing input for forecast.hybridModel()")
  test_that("Testing invalid inputs", {
    inputSeries <- ts(rnorm(24), f = 2)
    hModel <- hybridModel(inputSeries)
    # time series too short
    expect_error(hybridModel(numeric(), models = "ae"))
    # h must be positive
    expect_error(forecast(object = hModel, h = -1L))
    # h must be numeric
    expect_error(forecast(object = hModel, h = "a"))
    # h should be an integer
    expect_error(forecast(object = hModel, h = 3.2))
    # xreg should be a matrix
    expect_error(forecast(object = hModel, xreg = "1"))
    # matrix should be numeric
    expect_error(forecast(object = hModel, h = 5,
                                     xreg = matrix("a", nrow = 5, ncol = 2)))
#~     expect_error(forecast(object = hModel, h = 5,
#~                                      xreg = 1:12))
    # s3 forecast method should take a hybridModel object only
    expect_error(forecast.hybridModel("a"))
  })

  test_that("Testing prediction intervals", {
    set.seed(5)
    inputSeries <- ts(rnorm(10), f = 2)
    mod <- hybridModel(inputSeries, models = "at")
    h <- 100
    fc <- forecast(mod, h = h, PI.combination = "extreme")
    arimaForecast <- forecast(mod$auto.arima, h = h)
    tbatsForecast <- forecast(mod$tbats, h = h)
    # Default PI.combination = "extreme" should guarantee PI as pessimistic as any component
    expect_true(all(arimaForecast$upper <= fc$upper))
    expect_true(all(tbatsForecast$upper <= fc$upper))
    expect_true(all(arimaForecast$lower >= fc$lower))
    expect_true(all(tbatsForecast$lower >= fc$lower))

    # Test  with PI.combination = "mean"
    fc <- forecast(mod, h = h, PI.combination = "mean")
    expect_true(all(xor(arimaForecast$upper <= fc$upper, tbatsForecast$upper <= fc$upper)))
    expect_true(all(xor(arimaForecast$lower >= fc$lower, tbatsForecast$lower >= fc$lower)))
  })

  test_that("Testing forecasts with xreg", {
    # Test a simple et model
    set.seed(5)
    inputSeries <- ts(rnorm(5), f = 2)
    hm <- hybridModel(inputSeries, models = "et")
    expect_error(forecast(hm), NA)

    # Test xregs
    inputSeries <- subset(wineind, end = 48)
    testLength <- 12
    mm <- matrix(runif(length(inputSeries)), nrow = length(inputSeries))
    fm <- matrix(runif(testLength), nrow = testLength)
    hm <- hybridModel(inputSeries, models = "af",
                      a.args = list(xreg = mm))
    fc <- forecast(hm, h = testLength, xreg = fm)
    # No xreg provided
    expect_error(forecast(hm, h = testLength))
    fc <- forecast(hm, h = testLength, xreg = fm)
    expect_true(all(fc$mean > 0))
    expect_true(length(fc$mean) == testLength)

    # stlm only works with xreg when method = "arima" is passed in s.args
    expect_error(aa <- hybridModel(inputSeries, models = "afns",
                                   a.args = list(xreg = mm),
                                   s.args = list(xreg = mm)))
    aa <- hybridModel(inputSeries, models = "aefnst",
                      a.args = list(xreg = mm),
                      n.args = list(xreg = mm),
                      s.args = list(xreg = mm, method = "arima"))
    # If xreg is used and no h is provided, overwrite h
    newXreg <- matrix(rnorm(3), nrow = 3)
    expect_error(tmp <- forecast(aa, xreg = newXreg, npaths = 5), NA)
    expect_equal(class(tmp), "forecast")
    expect_equal(length(tmp$mean), nrow(newXreg))
    # If nrow(xreg) != h, issue a warning but set h <- nrow(xreg)
    expect_warning(forecast(aa, h = 10, xreg = newXreg, PI = FALSE))

    # Fit the model using xreg for only one individual component
    # Forecast should still work (previous bug)
    mod <- hybridModel(inputSeries, models = "af", a.args = list(xreg = mm))
    expect_error(forecast(mod, xreg = newXreg), NA)
    mod <- hybridModel(inputSeries, models = "nf", n.args = list(xreg = mm))
    expect_error(forecast(mod, xreg = newXreg, PI = FALSE), NA)
    mod <- hybridModel(inputSeries, models = "sf", s.args = list(xreg = mm, method = "arima"))
    expect_error(forecast(mod, xreg = newXreg), NA)

    # Valid forecast properties
    expect_error(forecast(aa, xreg = newXreg, PI = FALSE), NA)
    expect_true(length(forecast(aa, xreg = newXreg, PI = FALSE)$mean) == nrow(newXreg))
    expect_true(class(forecast(aa, xreg = newXreg, PI = FALSE)) == "forecast")
    # Prediction intervals for nnetar are nondeterministic, so this will fail
    # Testing this is slow, so leave it out for now
    #expect_true(all(forecast(aa, xreg = mm,  h = nrow(mm), level = 0.9)$upper == forecast(aa, xreg = mm,  h = nrow(mm), level = 90)$upper))
    expect_error(forecast(aa, xreg = mm, level = 110))
    # Fan should generate 17 prediction intervals
    fc <- forecast(aa, xreg = newXreg, h = nrow(newXreg), fan = TRUE)
    expect_true(ncol(fc$upper) == 17)
    expect_true(ncol(fc$lower) == 17)
    })

    test_that("More forecast xreg tests", {
    # forecast xreg with multiple meaningful xreg
    set.seed(5)
    len <- 240 # should be mod 4 for building the xreg
    expect_true(len %% 4 == 0)
    ts <- ts(arima.sim(n = len, list(ar = c(0.8, -0.2), ma = c(-0.2, 0.8)), sd = 1), f = 3)
    xreg = as.matrix(data.frame(x1 = rep_len(0:1, len), x2 = rep_len(0:4, len)))
    ts <- ts + xreg[, "x1"] + xreg[, "x2"]
    # Ensure we have enough data to differentiate the benefits of adding xreg
    aa <- auto.arima(ts)
    aa_xreg <- auto.arima(ts, xreg = xreg)
    expect_true(AIC(aa_xreg) < AIC(aa))
    trainIndices <- 1:len <= len * 0.9
    trainTestDivide = len * 0.9
    xregTrain <- xreg[trainIndices, ]
    xregTest <- xreg[!trainIndices, ]
    tsTrain <- subset(ts, end = trainTestDivide)
    tsTest <- subset(ts, start = trainTestDivide + 1)
    aa <- auto.arima(tsTrain)
    aa_xreg <- auto.arima(tsTrain, xreg = xregTrain)
    expect_true(accuracy(forecast(aa_xreg, xreg = xregTrain))[1, "MASE"] <
                accuracy(forecast(aa))[1, "MASE"])
    hm <- hybridModel(tsTrain, models = "as", s.args = list(method = "arima"))

    h <- nrow(xregTest)
    hm_fc <- forecast(hm, h = h)
    expect_true(length(hm_fc$mean) == h)
    # Test with several different xregs
    for(colIndex in list(1, 2, 1:2)){
        xrTrain <- as.matrix(xregTrain[, colIndex])
        xrTest <- as.matrix(xregTest[, colIndex])
        hm_xreg <- hybridModel(tsTrain, models = "as",
                               a.args = list(xreg = xrTrain),
                               s.args = list(xreg = xrTrain, method = "arima"))
        # Base models should work
        expect_error(forecast(hm_xreg$auto.arima, xreg = xrTest), NA)
        expect_error(forecast(hm_xreg$stlm, xreg = xrTest), NA)
        hm_xreg_fc <- forecast(hm_xreg, xreg = xrTest)
        expect_true(length(hm_fc$mean) == length(hm_xreg_fc$mean))
        # Model with xreg should be better than model without
        expect_true(AIC(hm_xreg$auto.arima) < AIC(hm$auto.arima))
    }

    # single feature xreg should return same results when passed as matrix or numeric
    len <- h <- 24
    dat <- ts(runif(len), f = 2)
    xreg <- rnorm(len)
    xreg_mat <- matrix(xreg)
    hm_mat <- hybridModel(dat, "as", a.args = list(xreg = xreg_mat),
                          s.args = list(xreg = xreg_mat, method = "arima"))
    mat_fc <- forecast(hm_mat, h = h, xreg = xreg_mat)
    fc <- forecast(hm_mat, h = h, xreg = xreg)
    # Forecasts where the new xreg is a matrix or numeric should match
    expect_true(all.equal(fc$mean, mat_fc$mean))

    # a model trained with a vector xreg should produce the same forecasts when using a matrix xreg
    hm <- hybridModel(dat, "as", a.args = list(xreg = xreg),
                  s.args = list(xreg = xreg, method = "arima"))
    fc <- forecast(hm, h = h, xreg = xreg)
    expect_true(all.equal(fc$mean, mat_fc$mean))
    mat_fc <- forecast(hm, xreg = xreg_mat)
    expect_true(all.equal(fc$mean, mat_fc$mean))
  })
}
