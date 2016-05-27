Sys.setenv("R_TESTS" = "")
if(require(testthat) & require(forecast)){
  test_check("forecastHybrid")
  }
