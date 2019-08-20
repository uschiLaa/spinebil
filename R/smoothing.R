#' Re-evaluate index after jittering the projection by an angle alpha.
#'
#' @param proj Original projection plane
#' @param d Data matrix
#' @param alpha Jitter angle
#' @param idx Index function
#' @return New index value
#' @export
jitterAngle <-function(proj, d, alpha, idx){
  newProj <- basis_nearby(proj, alpha = alpha, method = "geodesic")
  newD <- d %*% newProj
  return(idx(newD))
}

#' Re-evaluate index after jittering all points by an amount alpha.
#'
#' @param projData Original projected data points
#' @param alpha Jitter amount (passed into the jitter() function)
#' @param idx Index function
#' @return New index value
#' @export
jitterPoints <-function(projData, alpha, idx){
  newD <- jitter(projData, amount=alpha)
  return(idx(newD))
}

#' Evaluate mean index value over n jittered views.
#'
#' @param proj Original projection plane
#' @param d Data matrix
#' @param alpha Jitter amount (for jittering angle or points)
#' @param idx Index function
#' @param method Select between "jitterAngle" (default) and "jitterPoints"
#'     (otherwise we return original index value)
#' @param n Number of evaluations entering mean value calculation
#' @return Mean index value
#' @export
getIndexMean <- function(proj, d, alpha, idx, method="jitterAngle", n=10){
  dProj <- d %*% proj
  orig <- idx(dProj)
  if(method == "jitterAngle"){
    valVec <- replicate(n, jitterAngle(proj, d, alpha, idx))
  } else if (method=="jitterPoints"){
    valVec <- replicate(n, jitterPoints(dProj, alpha, idx))
  }
  else { return(orig)}
  return(mean(c(orig, valVec)))
}

#' Compare traces with different smoothing options.
#'
#' @param d Data matrix
#' @param tPath Interpolated tour path (as list of projections)
#' @param idx Index function
#' @param alphaV Jitter amounts to compare (for jittering angle or points)
#' @param n Number of evaluations entering mean value calculation
#' @return Table of mean index values
#' @export
#' @examples \donttest{
#' d <- spiralData(4, 100)
#' tPath <- tourr::save_history(d, max_bases=2)
#' tPath <- as.list(tourr::interpolate(tPath))
#' idx <- scagIndex("Skinny")
#' compS <- compareSmoothing(d, tPath, idx, n=5)
#' plotSmoothingComparison(compS)
#' }
compareSmoothing <- function(d, tPath, idx, alphaV=c(0.01, 0.05, 0.1), n=10){
  sc <- tibble::tibble(
    indexMean=numeric(),
    t=numeric(),
    method=character(),
    alpha=numeric())
  for (method in c("jitterAngle", "jitterPoints", "noSmoothing")){
    for (alpha in alphaV){
      for (i in seq_along(tPath)) {
        idM <- getIndexMean(tPath[[i]], d, alpha, idx, method, n)
        sc <- sc %>%
          tibble::add_row(indexMean=idM, t=i, method=method, alpha=alpha)
      }
    }
  }
  return(sc)
}

#' Plot the comparison of smoothing methods.
#'
#' Plotting method for the results of compareSmoothing.
#' The results are mapped by facetting over values of alpha and
#' mapping the method (jitterAngle, jitterPoints, noSmoothing) to
#' linestyle and color (black dashed, black dotted, red solid). By
#' default legend drawing is turned off, but can be turned on via
#' the lPos argument, e.g. setting to "bottom" for legend below the plot.
#'
#' @param smMat Result from compareSmoothing
#' @param lPos Legend position passed to ggplot2 (default is none for no legend shown)
#' @return ggplot visualisation of the comparison
#' @export
plotSmoothingComparison <- function(smMat, lPos="none"){
  smMat <- smMat %>%
    dplyr::mutate(method = factor(
      method, levels = c("jitterAngle", "jitterPoints", "noSmoothing")
      )) %>%
    ggplot2::ggplot(
      ggplot2::aes(x=t, y=indexMean, color=method, linetype=method)
      ) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values = c("black", "black", "red")) +
    ggplot2::scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
    ggplot2::theme(legend.position = lPos)+
    ggplot2::ylim(0,1) +
    ggplot2::facet_grid(alpha~.) +
    ggplot2::xlab("Sequence of projections (t)") +
    ggplot2::ylab("PPI value")
  smMat
}

