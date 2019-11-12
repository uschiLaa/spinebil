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
getSliceTrace <- function(d, m, indexList, indexLabels, h){
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
    dist_data <- tourr:::anchored_orthogonal_distance(tFullPath[[i]], d)
    res <- c()
    for (idx in indexList){
      res <- c(res, idx(dprj, dist_data, h))
    }
    resMat[i,] <- c(res, i)
  }
  resMat
}

l1idx <- function(nbin=10, direction=-1, bintype="square", max=FALSE, l2=FALSE){
  function(mat, dists, h){
    n <- nrow(mat)
    d <- ncol(mat)
    colnames(mat) <- c("x","y")
    mat <- as.tibble(mat)
    # Assuming d=2!
    # defining bins
    if (bintype == "square") {
      mat$xbin <- cut(mat$x, nbin)
      mat$ybin <- cut(mat$y, nbin)
    }
    else if (bintype == "polar") {
      # Assuming center is 0,0
      rad <- (mat$x^2+mat$y^2)^2
      ang <- atan2(mat$y, mat$x)
      mat$xbin <- cut(rad, nbin)
      mat$ybin <- cut(ang, nbin)
    }
    else if (bintype == "radial") {
      # Assuming center is 0,0
      rad <- (mat$x^2+mat$y^2)^2
      mat$xbin <- cut(rad, nbin)
      mat$ybin <- "all"
    }
    else if (bintype == "angular") {
      # Assuming center is 0,0
      ang <- atan2(mat$y, mat$x)
      mat$xbin <- "all"
      mat$ybin <- cut(ang, nbin)
    }
    else {
      cat(bintype, " is not a recognised bin type\n")
      return(0)
    }
    mat$inSlice <- factor(if_else(dists > h, 0, 1))
    # binning data
    mat_tab <- mat %>% count(xbin, ybin, inSlice, .drop=FALSE)
    # FIXME what if no data inside the slice?
    # also need to think about minimum number of points that are required inside the slice
    # for the index to make sense at all?
    if (length(mat_tab$n[mat_tab$inSlice=="1"])==0){return(0)}
    # getting binwise density differences
    if (l2)
      x <- sqrt(mat_tab$n[mat_tab$inSlice=="0"]/sum(mat_tab$n[mat_tab$inSlice=="0"])) -
      sqrt(mat_tab$n[mat_tab$inSlice=="1"]/sum(mat_tab$n[mat_tab$inSlice=="1"]))
    else
      x <- (mat_tab$n[mat_tab$inSlice=="1"]/sum(mat_tab$n[mat_tab$inSlice=="1"]) -
              (mat_tab$n[mat_tab$inSlice=="0"]/sum(mat_tab$n[mat_tab$inSlice=="0"])))*direction
    # for normalisation consider only positive entries in final sum
    x <- x[x>0]
    if (max)
      max(x)
    else {
      if (l2)
        sum(x^2)
      else
        sum(x)
    }
  }
}
