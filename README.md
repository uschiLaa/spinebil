
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spinebil

<!-- badges: start -->

<!-- badges: end -->

Studying Projection Pursuit INdex functions through Exploration Based on
Interpolated tour paths and Line graphs. spinebil provides
functionalities to evaluate the performance of projection pursuit index
functions using tour methods as presented in [this
paper](https://arxiv.org/abs/1902.00181).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("uschiLaa/spinebil")
```

## Example

To evaluate the index behaviour on a known input distribution we can
trace its value when interpolating a tour path, for example moving from
nuisance and structured projection.

``` r
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
```

<img src="man/figures/README-example-1.png" width="100%" />
