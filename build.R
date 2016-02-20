library(devtools)
library(knitr)


# compile Readme
knit("README.Rmd", "README.md")

document("pkg")
build("pkg")
