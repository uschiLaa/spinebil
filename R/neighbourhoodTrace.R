# neighbourhood traces

#' Tracing the index over an interpolated planned tour path.
#'
#' Tracing is used to test if the index value varies smoothly
#' over an interpolated tour path. The index value is calculated
#' for the data d in each projection in the interpolated sequence.
#' Note that all index functions must take the data in 2-d matrix
#' format and return the index value.
#'
#' @param d data
#' @param index index functions to calculate for each entry
#' @param start starting plane from which to explore neighbourhood
#' @param h slice thickness
#' @param n number of randomly selected directions
#' @param tmax cutoff on interpolated planes
#' @return index values for each interpolation step
#' @export
#' @examples
#' d <- spiralData(4, 100)
#' m <- list(basisMatrix(1,2,4), basisMatrix(3,4,4))
#' indexList <- list(tourr::holes(), tourr::cmass())
#' indexLabels <- c("holes", "cmass")
#' trace <- getTrace(d, m, indexList, indexLabels)
#' plotTrace(trace)
getNeighbourhoodTrace <- function(d, index, start, h, n, tmax=20){
  mX <- m[[1]]
  if(ncol(mX) != 2){
    print("Each projection matrix must have exactly two columns!")
    return(NULL)
  }
  # problem with planned tour: skipping first two entries
  # so we generate proxies (not used)
  mSkip1 <- tourr::basis_random(nrow(mX))
  mSkip2 <- tourr::basis_random(nrow(mX))

  # initialise results storage
  res <- tibble::tibble(n = numeric(), t = numeric(), value = numeric())

  # loop over path and index functions
  i <- 1
  while (i <= n){
    mTarget <- tourr::basis_random(nrow(mX))
    mList <- list(mSkip1, mSkip2, start, mTarget)
    tPath <- tourr::save_history(d, tour_path=tourr::planned_tour(mList))
    # get interpolated path and unformat to list
    tFullPath <- as.list(tourr::interpolate(tPath))
    fullLength <- length(tFullPath)
    j <- 1
    while (j <= tmax) {
      if (j > fullLength) break
      dprj <- as.matrix(d) %*% tFullPath[[j]]
      dist_data <- tourr:::anchored_orthogonal_distance(tFullPath[[j]], d)
      idx <- index(dprj, dist_data, h)
      res <- dplyr::add_row(res, n = i, t = j, value = idx)
      j <- j + 1
    }
    i <- i+1
  }
  res
}

#' Plot traces of indexes obtained with \code{\link{getTrace}}.
#'
#' @param resMat data (result of getTrace)
#' @param rescY bool to fix y axis range to \[0,1\]
#' @return ggplot visualisation of the tracing data
#' @export
plotNeighbourhoodTrace <- function(res, rescY=TRUE){
  resMelt <- tibble::as_tibble(res) %>%
    ggplot2::ggplot(ggplot2::aes(x=t, y=value)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(n~.) +
    ggplot2::theme(legend.position="none") +
    ggplot2::xlab("Sequence of projections (t)") +
    ggplot2::ylab("PPI value")
  if (rescY) {
    resMelt <- resMelt +
      ggplot2::ylim(c(0,1)) # usually we want index values between 0 and 1
  }
  resMelt
}
