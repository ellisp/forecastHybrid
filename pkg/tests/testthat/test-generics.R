# Unit tests on the generic function
if(require(forecast) & require(testthat)){
  numObs <- 100L
  context("Testing generic functions")
  set.seed(2345)
  # Seaosnal data is required for the stlm model and will throw a warning
  expect_warning(hm <- hybridModel(y = 10 + rnorm(numObs)))
  test_that("Testing summary and print methods", {
    # The generic methods should not throw an error
    expect_error(summary(hm), NA)
    expect_error(print(hm), NA)
    expect_true(is.hybridModel(hm))
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
  })
}
