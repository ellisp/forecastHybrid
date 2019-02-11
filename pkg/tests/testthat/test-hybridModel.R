# Unit tests on the hybridModel function
if(require(forecast) & require(testthat)){
  context("Testing input for hybridModel()")
  test_that("Testing invalid inputs", {
    # Invalid arguments for models
    expect_error(hybridModel(y = 1:10, models = "jten"))
    expect_error(hybridModel(y = 1:10, models = "ae32"))
    expect_error(hybridModel(y = 1:10, models = "32"))
    expect_error(hybridModel(y = 1:10, models = ""))
    # models must be characters
    expect_error(hybridModel(y = 1:10, models = 5))
    expect_error(hybridModel(y = matrix(1:10, nrow = 5, ncol = 2),
                             models = 5))
    # Test for invalid mismatch length of y and xreg in a.args/s.args later?
    badxreg <- rnorm(length(wineind) - 1)
    expect_warning(expect_error(hybridModel(y = wineind, a.args = list(xreg = badxreg))))
    # More invalid inputs
    expect_error(hybridModel(y = "hello world"))
    # must provide input series
    expect_error(hybridModel())
    # input series must have data
    expect_error(hybridModel(y = numeric()))
    # input series must be numeric
    expect_error(hybridModel(y = "aten"))
    # models must be one of a, e, f, t, n, s, z
    expect_error(hybridModel(y = wineind, models = "abcderfghijk"))
    # ensemble must have at least two models
    expect_error(hybridModel(wineind, models = "a"))
    # num.core must be positive
    expect_error(hybridModel(wineind, num.cores = -1L))
    # num.cores must be an integer
    expect_error(hybridModel(wineind, num.cores = 3.3))
    # num.cores must be numeric
    expect_error(hybridModel(wineind, num.cores = "a"))
    # models must not be empty string
    expect_error(hybridModel(wineind, models = ""))
    # parallel should be a boolean
    expect_error(hybridModel(wineind, parallel = "a"))
  })

  test_that("Testing for warnings", {
    # hybridModel creates a warning when an arg list for an unused model is passed
    expect_warning(hybridModel(wineind, models = "fs", a.args = list()))
    expect_warning(hybridModel(wineind, models = "fs", e.args = list()))
    expect_warning(hybridModel(wineind, models = "fs", n.args = list()))
    expect_warning(hybridModel(wineind, models = "fn", s.args = list()))
    expect_warning(hybridModel(wineind, models = "fs", t.args = list()))
    # thetam() requires at least one seasonal period of data
    expect_warning(hybridModel(ts(rnorm(4), f = 7), models = "aef"))
    # ts is too short: nnetar() and stlm() with 2 * frequency(y) >= length(y)
    inputSeries <- ts(rnorm(8), f = 4)
    expect_warning(hybridModel(inputSeries, models = "aesn"))
    # ts is too short: stlm() with 2 * frequency(y) >= length(y)
    inputSeries <- ts(rnorm(7), f = 4)
    expect_warning(hybridModel(inputSeries, models = "efs"))
    # ets with frequency(y) > 24
    inputSeries <- ts(rnorm(75), f = 25)
    expect_warning(hybridModel(inputSeries, models = "ens"))
    # weights = "cv.weights" with errorMethod = "MASE" is not yet implemented
    expect_warning(hybridModel(wineind, models = "fs", weights = "cv.errors",
                               errorMethod = "MASE"))
    # weights = "insample.errors" when there is a perfect fit
    expect_warning(hybridModel(ts(1:20, f = 2), weight="insample.errors"))
  })

  test_that("Testing valid inputs", {
    set.seed(54321)
    expect_error(hybridModel(wineind, models = "an", f.args = list()))
    expect_error(hybridModel(y = rnorm(10), models = "ae",
                             a.args = list(xreg = matrix(runif(10), nrow = 10))), NA)
    expect_warning(hybridModel(y = rnorm(10), models = "en",
                               a.args = list(lambda = 0.5)))
    inputSeries <- ts(rnorm(9), f = 4)
    expect_warning(hybridModel(inputSeries, models = "aensft",
                               weights = "insample.errors"))
    # soft deprecated insample.errors
    expect_warning(hybridModel(wineind, models = "fs",
                               weights = "insample.errors"))
    expect_error(hybridModel(inputSeries, models = "ae",
                             e.args = list(lambda = 0.5)), NA)
    # test parallel
    inputSeries <- ts(rnorm(8), f = 2)
    expect_warning(hybridModel(inputSeries, models = "fs", parallel = TRUE), NA)
    expect_warning(hybridModel(inputSeries, models = "fs", parallel = TRUE,
                               num.cores = 2), NA)
  })

  test_that("Testing long data", {
    set.seed(42)
    dat <- ts(rnorm(52 * 3), f = 52)
    expect_warning(hm <- hybridModel(y = dat, models = "fs"), NA)
    expect_true(length(forecast(hm)$mean) == 52 * 2)
    # Test for messy function call
    hm <- hybridModel(wineind)
    # Disable these for now because fails on r-devel
    # Less than orignal, messy function call
    #expect_true(format(object.size(hm)) < "6035456 bytes")
    # No worse than improved function call
    #expect_true(format(object.size(hm)) <= "322352 bytes")
  })

  test_that("Testing model matching", {
    set.seed(123456)
    expect_error(hybridModel(y = rnorm(20), models = "AAAAE"), NA)
    expect_error(hybridModel(y = rnorm(20), models = "AtTaaa"), NA)
    expect_error(hybridModel(y = rnorm(20), models = "nNeTEEEeA"), NA)
  })

  context("Testing cv.errors")
  test_that("Testing hybridModel(weights = \"cv.errors\")", {
  set.seed(33)
  inputSeries <- ts(rnorm(12), f = 2)
  expect_error(cvMod <- hybridModel(inputSeries, weights = "cv.errors",
                                    windowSize = 8, cvHorizon = 2), NA)
  expect_equal(length(cvMod$weights),
               length(unique(cvMod$weights)))
  })

  test_that("Testing the hybridModel object", {
    modelComparison <- list()
    for(parallel in c(FALSE, TRUE)){
      set.seed(4)
      len <- 20
      freq <- 2
      tol <- 10^-8
      testSeries <- ts(rnorm(len), f = freq)
      cols <- c("a", "b", "c")
      xreg <- matrix(rnorm(len * length(cols)), nrow = len)
      colnames(xreg) <- cols
      # Ignore nnetar for now since it isn't reproducible
      models <- "aefstz"
      hm <- hybridModel(testSeries, models = models,
                        a.args = list(xreg = xreg), lambda = 0.2,
                        parallel = parallel)
      for(obj in hm){
        expect_true(all(class(obj) != "NULL"))
        expect_true(!is.null(obj))
      }
      # Ensure numeric values
      expect_true(is.numeric(hm$fitted))
      expect_true(is.numeric(hm$residuals))
      expect_true(is.numeric(hm$x))
      expect_true(length(hm$fitted) == length(testSeries))
      expect_true(length(hm$residuals) == length(testSeries))
      expect_true(length(hm$x) == length(testSeries))
      expect_true(all(hm$weights >= 0))
      # Weights should sum to 1 but allow tolerance
      expect_true(sum(hm$weights) - 1 < tol)
      expect_true(length(hm$weights) == nchar(models))
      expect_true(hm$frequency == freq)
      # Ensure xreg is correct
      expect_true(all(hm$auto.arima$xreg == xreg))
      expect_true(hm$xreg$auto.arima)
      #expect_true(!hm$xreg$nnetar)
      expect_true(!hm$xreg$stlm)
      # Ensure other fields are correct
      expect_true(length(hm$models) == nchar(models))
      expect_true(all(names(hm$weights) == hm$models))
      # Ensure the models are of the expected classes
      expect_true("ARIMA" %in% class(hm$auto.arima))
      expect_true("ets" == class(hm$ets))
      expect_true("thetam" %in% class(hm$thetam))
      #expect_true("nnetar" == class(hm$nnetar))
      expect_true("stlm" %in% class(hm$stlm))
      expect_true("forecast" %in% class(hm$snaive))

      # Base forecasts should work
      expect_error(forecast(hm$auto.arima, xreg = xreg), NA)
      # Test forecast
      h <- nrow(xreg)
      expect_error(forecast(hm, h = h, xreg = xreg), NA)
      modelComparison[[as.character(parallel)]] <- hm
    }
    # Compare the results from parallel = TRUE and parallel = FALSE
    tol <- 10^-8
    parallelFitted <- modelComparison[["TRUE"]]$fitted
    serialFitted <- modelComparison[["FALSE"]]$fitted
    expect_true(sum(abs(parallelFitted - serialFitted), na.rm = TRUE) < tol)
    expect_true(all(is.na(serialFitted) == is.na(parallelFitted)))

    parallelResiduals <- modelComparison[["TRUE"]]$residuals
    serialResiduals <- modelComparison[["FALSE"]]$residuals
    expect_true(sum(abs(parallelResiduals - serialResiduals), na.rm = TRUE) < tol)
    expect_true(all(is.na(serialResiduals) == is.na(parallelResiduals)))
  })

  test_that("Testing the weighting methods", {
    inputSeries <- ts(rnorm(100), f = 2)
    tol <- 10^-8
    # Test two quick models since we will be performing cross validation
    models <- "fs"
    weights <- c("equal", "insample.errors", "cv.errors")
    # TODO: add another loop here for errorMethod once MASE with cv.errors is implemented
    results <- list()
    for(weight in weights){
      if(weight == "insample.errors"){
        expect_warning(hm <- hybridModel(inputSeries, models = models,
                                         weights = weight))
      } else{
        hm <- hybridModel(inputSeries, models = models, weights = weight)
      }
      expect_true(sum(hm$weights) - 1 < tol)
      expect_true(all(hm$weights >= 0))
      expect_true(all(hm$weights <= 1))
      expect_true(length(hm$weights) == nchar(models))
      results[[weight]] <- hm
    }
    # Test that weights are not equal
    weightResults <- sapply(results, function(x) x$weights)
    weights <- as.list(data.frame(weightResults))
    expect_true(all(weights$insample.errors != weights$cv.errors))
    expect_true(all(weights$insample.errors != weights$equal))
    expect_true(all(weights$equal != weights$cv.errors))
  })

  test_that("Testing the hybrid model with xreg", {
    # Test with data from issue #86
    trainSet <- beaver1[1:100, ]
    testSet <- beaver1[-(1:100), ]
    trainXreg <- as.matrix(data.frame(trainSet$activ, trainSet$time))
    beaverhm <- hybridModel(ts(trainSet$temp, f = 6),
                            models = "aenst",
                            a.args = list(xreg = trainXreg),
                            n.args = list(xreg = trainXreg),
                            s.args = list(xreg = trainXreg, method = "arima"))
    expect_true(all.equal(names(beaverhm$xreg), c("auto.arima", "nnetar", "stlm")))
    expect_true(all(beaverhm$xreg == TRUE))
  })
}
