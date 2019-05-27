#' Tracing the index over interpolated planned tour path.
#'
#' Over an interpolated tour path between the planes specified
#' by basis vectors listed in x, y we calculate the index value
#' obtained for the data d in each projection. All index functions
#' must take the data in 2-d matrix format and return the index value.
#'
#' @param d data
#' @param m list of projection matrices for the planned tour
#' @param indexList list of index functions to calculate for each entry
#' @param indexLabels labels used in the output
#' @return index values for each interpolation step
#' @export
getTrace <- function(d, m, indexList, indexLabels){
  mX <- m[[1]]
  if(ncol(mX) != 2){
    print("Each projection matrix must have exactly two columns!")
    return(NULL)
  }
  # problem with planned tour: skipping first two entries, so we generate proxies (not used)
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
  for (i in 1:length(tFullPath)){
    dprj <- as.matrix(d) %*% tFullPath[[i]]
    res <- c()
    for (idx in indexList){
      res <- c(res, idx(dprj))
    }
    resMat[i,] <- c(res, i)
  }
  resMat
}

#' Plot traces of indexes obtained with getTrace.
#'
#' @param resMat data (result of getTrace)
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
  if (rescY) resMelt <- resMelt + ggplot2::ylim(c(0,1)) # usually we want index values between 0 and 1
  resMelt
}
