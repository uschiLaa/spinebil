#' Re-evaluate index after jittering the projection by an angle alpha.
#'
#' @param proj Original projection plane
#' @param d Data matrix
#' @param alpha Jitter angle
#' @param idx Index function
#' @return New index value
#' @export
jitterAngle <-function(proj, d, alpha, idx){
  newProj <- tourr:::basis_nearby(proj, alpha = alpha, method = "geodesic")
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
jitterPointsScagnostics <-function(projData, alpha, idx){
  newD <- jitter(projData, amount=alpha)
  return(idx(newD))
}
