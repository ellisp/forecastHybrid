library(devtools)
library(knitr)
library(forecastHybrid)

# compile Readme
knit("README.Rmd", "README.md")

document("pkg")
build_vignettes("pkg")
test("pkg")
check("pkg")

build("pkg")


