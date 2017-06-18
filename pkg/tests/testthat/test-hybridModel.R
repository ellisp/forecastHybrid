# Unit tests on the hybridModel function
if(require(forecast) & require(testthat)){
  context("Testing input for hybridModel()")
  test_that("Testing invalid inputs", {
    expect_error(hybridModel(y = 1:10, models = "jten"))
    expect_error(hybridModel(y = 1:10, models = 5))
    expect_error(hybridModel(y = matrix(1:10, nrow = 5, ncol = 2),
                             models = 5))
    # Test for invalid mismatch length of y and xreg in a.args/s.args later?
    badxreg <- data.frame(rnorm(length(wineind) - 1))
    expect_warning(expect_error(hybridModel(y = wineind,
                                            a.args = list(xreg = badxreg))))
    expect_error(hybridModel(y = "hello world"))
    expect_error(hybridModel())
    expect_error(hybridModel(y = numeric()))
    expect_error(hybridModel(y = "aten"))
    expect_error(hybridModel(y = wineind, models = "abcderfghijk"))
    expect_error(hybridModel(wineind, models = "a"))
    expect_error(hybridModel(wineind, num.cores = -1L))
    expect_error(hybridModel(wineind, num.cores = 3.3))
    expect_error(hybridModel(wineind, num.cores = "a"))
    expect_error(hybridModel(wineind, models = ""))
    expect_error(hybridModel(wineind, parallel = "a"))
    # warning when component model fits perfectly and weights = "insample.error"
    #expect_warning(hybridModel(y = ts(1:9, frequency = 4), weights = "insample.errors"))

  })
  test_that("Testing for warnings", {
    expect_warning(hybridModel(wineind, models = "fs", a.args = list()))
    expect_warning(hybridModel(wineind, models = "fs", e.args = list()))
    expect_warning(hybridModel(wineind, models = "fs", n.args = list()))
    expect_warning(hybridModel(wineind, models = "fn", s.args = list()))
    expect_warning(hybridModel(wineind, models = "fs", t.args = list()))
    # nnetar() with 2 * frequency(y) >= length(y)
    expect_warning(hybridModel(ts(rnorm(50), f = 24), models = "fsn"))
    # stlm() with 2 * frequency(y) >= length(y)
    expect_warning(hybridModel(ts(rnorm(20), f = 12), models = "efs"))
    # ets with frequency(y) > 24
    expect_warning(hybridModel(ts(rnorm(100), f = 25), models = "ens"))
    expect_warning(hybridModel(wineind, models = "fs", parallel = TRUE))
  })
  test_that("Testing valid inputs", {
    set.seed(54321)
    expect_error(hybridModel(wineind, models = "an", f.args = list()))
    expect_error(hybridModel(y = rnorm(100), models = "ae",
                             a.args = list(xreg = matrix(runif(100), nrow = 100))), NA)
    expect_warning(hybridModel(y = rnorm(10), models = "en",
                               a.args = list(lambda = 0.5)))
    inputSeries <- ts(rnorm(9), f = 4)
    expect_error(hybridModel(inputSeries, models = "aensft",
                             weights = "insample.errors"), NA)
    # soft deprecated insample.errors
    expect_warning(hybridModel(wineind, models = "fs", weights = "insample.errors"))
    expect_error(hybridModel(inputSeries, models = "ae",
                             e.args = list(lambda = 0.5)), NA)
  })
  test_that("Testing model matching", {
    set.seed(123456)
    expect_error(hybridModel(y = rnorm(20), models = "AAAAE"), NA)
    expect_error(hybridModel(y = rnorm(20), models = "AtTaaa"), NA)
    expect_error(hybridModel(y = rnorm(20), models = "nNeTEEEeA"), NA)
  })
  context("Testing generic functions")
  test_that("Testing is.hybridModel(), fitted.hybridModel(), residuals.hybridModel(), and accuracy.hybridModel()", {
    inputSeries <- wineind
    exampleModel <- hybridModel(inputSeries)
    expect_true(is.hybridModel(exampleModel))
    expect_true(length(fitted(exampleModel)) == length(residuals(exampleModel)))
    expect_true(length(fitted(exampleModel, individual = TRUE)) == length(residuals(exampleModel, individual = TRUE)))
    expect_error(accuracy(exampleModel), NA)
    expect_error(accuracy(exampleModel, individual = TRUE), NA)
    expect_error(plot(exampleModel, type = "fit", ggplot = FALSE), NA)
    expect_error(plot(exampleModel, type = "models", ggplot = FALSE), NA)
    expect_error(plot(exampleModel, type = "fit", ggplot = TRUE), NA)
    expect_error(plot(exampleModel, type = "models", ggplot = TRUE), NA)
  })
  context("Testing cv.errors")
    test_that("Testing hybridModel(weights = \"cv.errors\")", {
      inputSeries <- ts(rnorm(20), f = 4)
      expect_error(cvMod <- hybridModel(inputSeries, weights = "cv.errors", windowSize = 14, cvHorizon = 3), NA)
      expect_true(length(cvMod$weights) == length(unique(cvMod$weights)))
    })
}
