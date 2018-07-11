# Unit tests on the generic function
if(require(forecast) & require(testthat)){
  context("Testing helper functions")
  test_that("Testing internal helper functions", {
    # Test getModelName()
    expect_true(getModelName("a") == "auto.arima")
    expect_true(getModelName("e") == "ets")
    expect_true(getModelName("f") == "thetam")
    expect_true(getModelName("n") == "nnetar")
    expect_true(getModelName("s") == "stlm")
    expect_true(getModelName("t") == "tbats")
    expect_true(getModelName("z") == "snaive")

    # Test getModel()
    expect_true(class(getModel("a")) == "function")
    expect_true(class(getModel("e")) == "function")
    expect_true(class(getModel("f")) == "function")
    expect_true(class(getModel("n")) == "function")
    expect_true(class(getModel("s")) == "function")
    expect_true(class(getModel("t")) == "function")
    expect_true(class(getModel("z")) == "function")

    # Test unwrapParallelModels
    models <- list(thetam(wineind), stlm(wineind))
    modelNames <- c("f", "s")
    results <- unwrapParallelModels(models, modelNames)
    expect_true(length(results) == 2)
    expect_true(all(names(results) == c("thetam", "stlm")))
  })

  test_that("Testing tsSubsetWithIndices()", {
    expect_true(all(head(wineind) == tsSubsetWithIndices(wineind, 1:5)))
    # Invalid subset outside of range
    expect_error(tsSubsetWithIndices(wineind, length(wineind) + 1))
    # Non-continuous indices
    expect_error(tsSubsetWithIndices(wineind, 4:1))
    })
}
