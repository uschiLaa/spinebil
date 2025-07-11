#' Test rotation invariance of index functions for selected 2-d data set.
#'
#' Ideally a projection pursuit index should be roation invariant, we
#' test this explicitly by profiling the index while rotating a
#' distribution.
#'
#' @param d data (2 column matrix containing distribution to be rotated)
#' @param indexList list of index functions to calculate for each entry
#' @param indexLabels labels used in the output
#' @param n number of steps in the rotation (default = 200)
#' @return index values for each rotation step
#' @export
#' @examples 
#' d <- as.matrix(sinData(2, 30))
#' indexList <- list(tourr::holes(), scagIndex("stringy"), mineIndexE("MIC"))
#' indexLabels <- c("holes", "skinny", "mic")
#' pRot <- profileRotation(d, indexList, indexLabels, n = 50)
#' plotRotation(pRot)
profileRotation <- function(d, indexList, indexLabels, n=200){
  # initialise results storage
  resMat <- matrix(ncol = length(indexLabels)+1, nrow = n+1)
  colnames(resMat) <- c(indexLabels, "alpha")

  # loop over rotation angles
  i <- 1
  for (a in seq(0,2*pi, 2*pi/n)){
    rotM <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), ncol = 2)
    dprj <- d %*% rotM
    res <- c()
    for (idx in indexList){
      res <- c(res, idx(dprj))
    }
    resMat[i,] <- c(res, a)
    i <- i+1
  }
  resMat
}

#' Plot rotation traces of indexes obtained with profileRotation.
#'
#' @param resMat data (result of profileRotation)
#' @return ggplot visualisation of the tracing data
#' @export
plotRotation <- function(resMat){
  PPI <- colnames(resMat)
  PPI <- PPI[PPI != "alpha"] # columns are index names or angle alpha
  resMelt <- tibble::as_tibble(resMat) |>
    dplyr::mutate(angle=alpha*360/(2*pi)) |>
    dplyr::select(-alpha) |>
    tidyr::gather(PPI, value, -angle) |>
    ggplot2::ggplot(ggplot2::aes(x=angle, y=value)) +
    ggplot2::facet_wrap(~PPI) +
    ggplot2::coord_polar() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::geom_line() +
    ggplot2::ylim(c(-1,1.1))
  resMelt
}
