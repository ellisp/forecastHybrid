# Unit tests on the generic function
if (require(forecast) & require(testthat)) {
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
    expect_true(class(tsSubsetWithIndices(wineind, 3:17)) == "ts")
    expect_true(all(head(wineind, 5) == tsSubsetWithIndices(wineind, 1:5)))
    expect_equal(head(wineind, 5), tsSubsetWithIndices(wineind, 1:5))

    # Invalid subset outside of range
    expect_error(tsSubsetWithIndices(wineind, length(wineind) + 1))

    # Non-continuous indices
    expect_error(tsSubsetWithIndices(wineind, 4:1))
    })
  test_that("Testing tsCombine", {
    ts1 <- tsSubsetWithIndices(AirPassengers, 1:50)
    ts2 <- tsSubsetWithIndices(AirPassengers, 50:100)
    ts3 <- tsSubsetWithIndices(AirPassengers, 101:144)
    combined_ts <- tsCombine(ts1, ts2, ts3)

    expect_true(class(combined_ts) == "ts")
    expect_true(length(combined_ts) == length(AirPassengers))
    expect_true(all(combined_ts == AirPassengers))
    expect_equal(combined_ts, AirPassengers)

    # Test that a recursive combine works as well
    r_combined_ts <- tsCombine(ts1, tsCombine(ts2, ts3))
    expect_true(class(r_combined_ts) == "ts")
    expect_true(length(r_combined_ts) == length(AirPassengers))
    expect_true(all(r_combined_ts == AirPassengers))
    expect_equal(r_combined_ts, AirPassengers)
    })
}
