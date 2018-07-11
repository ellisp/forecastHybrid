test_that("the forecast(thetam(...)) returns the same results as thetaf(...)", {
   # real seasonal data
   fc1 <- thetaf(AirPassengers, h = 12)
   fc2 <- forecast(thetam(AirPassengers), h = 12)
   
   expect_equal(sum(fc2$mean - fc1$mean), 0)
   expect_equal(sum(fc2$upper - fc1$upper), 0)
   expect_equal(sum(fc2$lower - fc1$lower), 0)
   
   
   # simulated seasonal data
   x <- ts(rnorm(100) + c(0, 1, 2, 3), frequency = 4)
   expect_equal(sum(forecast(thetam(x), h = 4)$mean - thetaf(x, h = 4)$mean ), 0)
   
   
   # simulated non-seasonal data
   x <- ts(rnorm(100))
   expect_equal(sum(forecast(thetam(x), h = 4)$mean - thetaf(x, h = 4)$mean ), 0)
   
   
   # fan
   fan1 <- forecast(thetam(AirPassengers), fan = TRUE)
   fan2 <- thetaf(AirPassengers, fan = TRUE)
   expect_equal(sum(fan1$upper - fan2$upper), 0)
   expect_equal(sum(fan1$lower - fan2$lower), 0)
   
   # arbitrary level
   fan1 <- forecast(thetam(AirPassengers), level = c(75, 90))
   fan2 <- thetaf(AirPassengers, level = c(75, 90))
   expect_equal(sum(fan1$upper - fan2$upper), 0)
   expect_equal(sum(fan1$lower - fan2$lower), 0)
   
   # another example
   fc1 <- thetaf(Nile)
   fc2 <- forecast(thetam(Nile))
   expect_equal(sum(fc2$mean - fc1$mean), 0)
   expect_equal(sum(fc2$upper - fc1$upper), 0)
   expect_equal(sum(fc2$lower - fc1$lower), 0)
   
   fc3 <- forecast(thetam(Nile), level = c(0.7, 0.9))
   expect_equal(sum(fc3$mean - fc1$mean), 0)
   expect_lt(sum(fc3$upper - fc1$upper), 0)
   expect_gt(sum(fc3$lower - fc1$lower), 0)
})

test_that("invalid inputs to thetam", {
   expect_error(thetam("hello"))
   expect_error(thetam(mtcars))
   shortSeries <- ts(rnorm(6), f = 7)
   expect_error(thetam(shortSeries))
})

test_that("hybrid models with theta give same results as when done manually", {
  mod1 <- hybridModel(gas, models = "fs") 
  fc1 <- forecast(mod1, h = 12)
  fc2a <- forecast(stlm(gas), h = 12)
  fc2b <- thetaf(gas, h = 12)
  expect_equal(sum((fc2a$mean + fc2b$mean) / 2 - fc1$mean), 0)
})

test_that("Generic `forecast` methods work on thetam objects", {
   mod1 <- thetam(wineind)
   mod2 <- thetam(rnorm(100))
   expect_error(plot(mod1), NA)
   expect_error(plot(mod2), NA)
   expect_error(forecast(mod1), NA)
   expect_error(forecast(mod1, level = 101))
   expect_error(accuracy(mod1), NA)
   expect_error(residuals(mod1), NA)
   expect_error(fitted(mod1), NA)
})
