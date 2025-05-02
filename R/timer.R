#' Time each index evaluation for projections in the tour path.
#'
#' Index evaluation timing may depend on the data distribution, we
#' evaluate the computing time for a set of different projections to
#' get an overview of the distribution of computing times.
#'
#' @param d Input data in matrix format
#' @param t List of projection matrices (e.g. interpolated tour path)
#' @param idx Index function
#' @param pmax Maximum number of projections to evaluate (cut t if longer than pmax)
#' @return numeric vector containing all distances
#' @export
#' @examples \donttest{
#' library(purrr)  # Ensure purrr is loaded
#' d <- spiralData(4, 1000)
#' t <- map(1:10, tourr::basis_random(4))
#' idx <- scagIndex("Skinny")
#' timeSequence(d, t, idx, 10)
#' }
timeSequence <- function(d, t, idx, pmax){
  i <- 1
  dfTimer <- data.frame(t= numeric(), i=numeric())
  if (is.null(d) || !is.numeric(as.matrix(d))) {
    stop("Error: 'd' must be a numeric matrix.")
  }
  d <- as.matrix(d)
  for(pMatrix in t){
    if(i>pmax) break
    if (is.null(pMatrix) || !is.numeric(as.matrix(pMatrix))) {
      warning("Skipping iteration: 'pMatrix' is NULL or not numeric.")
      next
    }
    pMatrix <- as.matrix(pMatrix)
    tictoc::tic.clearlog()
    tictoc::tic() #start timer
    dProj <- d %*% pMatrix
    res <- idx(dProj)
    tictoc::toc(log=TRUE,quiet=TRUE)
    log_values <- tictoc::tic.log(format = FALSE)
    
    if (length(log_values) > 0 && is.list(log_values[[1]])) {
      resT <- log_values[[1]]$toc - log_values[[1]]$tic
    } else {
      resT <- NA  # Assign NA if no valid timing data
      warning("tic.log() did not return expected values.")
    }
    
    dfTimer <- tibble::add_row(dfTimer, t=resT, i=i)
    i <- i + 1
  }
  return(dfTimer)
}
