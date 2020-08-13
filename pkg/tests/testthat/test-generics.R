# Unit tests on the generic function
if (require(forecast) & require(testthat)) {
  numObs <- 100L
  context("Testing generic functions")
  set.seed(2345)
  # Seaosnal data is required for the stlm model and will throw a warning
  inputSeries <- 10 + rnorm(numObs)
  expect_warning(hm <- hybridModel(y = inputSeries))
  test_that("Testing summary and print methods", {
    # The generic methods should not throw an error
    expect_error(summary(hm), NA)
    expect_error(print(hm), NA)
    expect_true(is.hybridModel(hm))
  })

  test_that("Testing generics is.hybridModel(), fitted(), residuals(), and accuracy()", {
    inputSeries <- subset(USAccDeaths, end = 25)
    # add some seasonality so there are roots to plot in the arima model
    inputSeries <- 100 * (1:12) + USAccDeaths
    exampleModel <- hybridModel(inputSeries)
    expect_true(is.hybridModel(exampleModel))
    expect_equal(length(fitted(exampleModel)),
                 length(residuals(exampleModel)))
    expect_equal(length(fitted(exampleModel, individual = TRUE)),
                 length(residuals(exampleModel, individual = TRUE)))
    expect_error(accuracy(exampleModel), NA)
    expect_error(accuracy(exampleModel, individual = TRUE), NA)
    expect_error(plot(exampleModel, type = "fit", ggplot = FALSE), NA)
    expect_error(plot(exampleModel, type = "models", ggplot = FALSE), NA)
    expect_error(plot(exampleModel, type = "fit", ggplot = TRUE), NA)
    expect_error(plot(exampleModel, type = "models", ggplot = TRUE), NA)
  })

  test_that("Testing fitted and residual methods", {
    # The generic methods should not throw an error
    expect_error(fitted(hm), NA)
    expect_error(residuals(hm), NA)
    expect_error(fitted(hm, individual = TRUE), NA)
    expect_error(residuals(hm, individual = TRUE), NA)
    # There should be a fitted and residual for each observation in the input series
    expect_true(length(fitted(hm)) == numObs)
    expect_true(length(residuals(hm)) == numObs)
    expect_true(length(residuals(hm, individual = TRUE)$nnetar) == numObs)
    expect_true(length(fitted(hm, individual = TRUE)$tbats) == numObs)
    # TSP attributes should match when the input series is not a ts object
    expect_equal(tsp(residuals(hm)), tsp(fitted(hm)))
    # Residuals should be smaller than actual or fitted values
    hm <- hybridModel(wineind, model = "fs")
    expect_true(all(residuals(hm) < fitted(hm), na.rm = TRUE))
    expect_true(all(residuals(hm) < wineind, na.rm = TRUE))
    # TSP attributes should match when the input series is a ts object
    expect_equal(tsp(residuals(hm)), tsp(fitted(hm)))
    expect_equal(tsp(wineind), tsp(fitted(hm)))
  })
}
