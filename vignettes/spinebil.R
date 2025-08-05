## ----example------------------------------------------------------------------
library(spinebil)
## sample from the spiral distribution
d <- spiralData(4, 100)
## the first two parameters are noise
## parameters 3 and 4 contain a spiral
## we write a list with the nuisance and structured plane
m <- list(basisMatrix(1,2,4), basisMatrix(3,4,4))
## the index functions to be evaluated should also be passed in a list
indexList <- list(tourr::holes(), tourr::cmass())
indexLabels <- c("holes", "cmass")
## we can now compute the index traces and plot them
trace <- getTrace(d, m, indexList, indexLabels)
plotTrace(trace)


## -----------------------------------------------------------------------------
library(spinebil)
library(cassowaryr)

result <- ppi_samplesize_effect(scagIndex("stringy"), n_sim = 10)
knitr::kable(result)

