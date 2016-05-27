# Unit tests on the hybridModel function
if(require(forecast) & require(testthat)){
  context("Testing input for hybridModel()")
  test_that("Testing invalid inputs", {
    expect_error(hybridModel(y = 1:10, models = "jten"))
    expect_error(hybridModel(y = 1:10, models = 5))
    expect_error(hybridModel(y = matrix(1:10, nrow = 5, ncol = 2),
                             models = 5))
    # Test for invalid mistmatch length of y and xreg in a.args/s.args later?
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

  })
  test_that("Testing for warnings", {
    expect_warning(hybridModel(wineind, models = "en", a.args = list()))
    expect_warning(hybridModel(wineind, models = "an", e.args = list()))
    expect_warning(hybridModel(wineind, models = "ae", n.args = list()))
    expect_warning(hybridModel(wineind, models = "an", s.args = list()))
    expect_warning(hybridModel(wineind, models = "an", t.args = list()))
    # nnetar() with 2 * frequency(y) >= length(y)
    expect_warning(hybridModel(ts(rnorm(50), f = 24), models = "aen"))
    # stlm() with 2 * frequency(y) >= length(y)
    expect_warning(hybridModel(ts(rnorm(20), f = 12), models = "aes"))
    # Currently unimplemented features
    expect_warning(hybridModel(wineind, models = "ae", parallel = TRUE))
    expect_warning(hybridModel(wineind, models = "ae", weights = "cv.errors"))
  })
  test_that("Testing valid inputs", {
    set.seed(54321)
    expect_error(hybridModel(y = rnorm(100), models = "ae",
                             a.args = list(xreg = matrix(runif(100), nrow = 100))), NA)
    expect_warning(hybridModel(y = rnorm(10), models = "en",
                               a.args = list(lambda = 0.5)))
    expect_error(hybridModel(wineind, models = "atens"), NA)
    expect_error(hybridModel(wineind, models = "aenst",
                             weights = "insample.errors"), NA)
    expect_error(hybridModel(wineind, models = "ae",
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
    exampleModel <- hybridModel(wineind)
    expect_true(is.hybridModel(exampleModel))
    expect_true(length(fitted(exampleModel)) == length(residuals(exampleModel)))
    expect_true(length(fitted(exampleModel, individual = TRUE)) == length(residuals(exampleModel, individual = TRUE)))
    expect_error(accuracy(exampleModel), NA)
    expect_error(accuracy(exampleModel, individual = TRUE), NA)
    expect_error(plot(exampleModel, type = "fit"), NA)
    expect_error(plot(exampleModel, type = "models"), NA)
  })
}
