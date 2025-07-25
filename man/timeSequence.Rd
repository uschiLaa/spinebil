% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timer.R
\name{timeSequence}
\alias{timeSequence}
\title{Time each index evaluation for projections in the tour path.}
\usage{
timeSequence(d, t, idx, pmax)
}
\arguments{
\item{d}{Input data in matrix format}

\item{t}{List of projection matrices (e.g. interpolated tour path)}

\item{idx}{Index function}

\item{pmax}{Maximum number of projections to evaluate (cut t if longer than pmax)}
}
\value{
numeric vector containing all distances
}
\description{
Index evaluation timing may depend on the data distribution, we
evaluate the computing time for a set of different projections to
get an overview of the distribution of computing times.
}
\examples{
d <- spiralData(4, 500)
t <- purrr::map(1:10, ~ tourr::basis_random(4))
idx <- scagIndex("stringy")
timeSequence(d, t, idx, 10)
}
