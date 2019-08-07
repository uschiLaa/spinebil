#' Tracing the index over an interpolated planned tour path.
#'
#' Tracing is used to test if the index value varies smoothly
#' over an interpolated tour path. The index value is calculated
#' for the data d in each projection in the interpolated sequence.
#' Note that all index functions must take the data in 2-d matrix
#' format and return the index value.
#'
#' @param d data
#' @param m list of projection matrices for the planned tour
#' @param indexList list of index functions to calculate for each entry
#' @param indexLabels labels used in the output
#' @return index values for each interpolation step
#' @export
#' @examples
#' d <- spiralData(4, 100)
#' m <- list(basisMatrix(1,2,4), basisMatrix(3,4,4))
#' indexList <- list(tourr::holes(), tourr::cmass())
#' indexLabels <- c("holes", "cmass")
#' trace <- getTrace(d, m, indexList, indexLabels)
#' plotTrace(trace)
getTrace <- function(d, m, indexList, indexLabels){
  mX <- m[[1]]
  if(ncol(mX) != 2){
    print("Each projection matrix must have exactly two columns!")
    return(NULL)
  }
  # problem with planned tour: skipping first two entries
  # so we generate proxies (not used)
  mSkip1 <- tourr::basis_random(nrow(mX))
  mSkip2 <- tourr::basis_random(nrow(mX))
  mList <- append(list(mSkip1, mSkip2), m)
  # get tour path object
  tPath <- tourr::save_history(d, tour_path=tourr::planned_tour(mList))
  # get interpolated path and unformat to list
  tFullPath <- as.list(tourr::interpolate(tPath))

  # initialise results storage
  resMat <- matrix(ncol = length(indexLabels)+1, nrow = length(tFullPath))
  colnames(resMat) <- c(indexLabels, "t")

  # loop over path and index functions
  for (i in seq_along(tFullPath)){
    dprj <- as.matrix(d) %*% tFullPath[[i]]
    res <- c()
    for (idx in indexList){
      res <- c(res, idx(dprj))
    }
    resMat[i,] <- c(res, i)
  }
  resMat
}

#' Plot traces of indexes obtained with \code{\link{getTrace}}.
#'
#' @param resMat data (result of getTrace)
#' @param rescY bool to fix y axis range to \[0,1\]
#' @return ggplot visualisation of the tracing data
#' @export
plotTrace <- function(resMat, rescY=TRUE){
  PPI <- colnames(resMat)
  PPI <- PPI[PPI != "t"] # columns are index names or time counter
  resMelt <- tibble::as_tibble(resMat) %>%
    tidyr::gather(PPI, value, -t) %>%
    ggplot2::ggplot(ggplot2::aes(x=t, y=value)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(PPI~.) +
    ggplot2::theme(legend.position="none") +
    ggplot2::xlab("Sequence of projections (t)") +
    ggplot2::ylab("PPI value")
  if (rescY) {
    resMelt <- resMelt +
      ggplot2::ylim(c(0,1)) # usually we want index values between 0 and 1
  }
  resMelt
}
