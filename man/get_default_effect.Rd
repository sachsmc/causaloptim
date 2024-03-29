% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph-utilities.R
\name{get_default_effect}
\alias{get_default_effect}
\title{Define default effect for a given graph}
\usage{
get_default_effect(graphres)
}
\arguments{
\item{graphres}{The graph object, should have vertex attributes "outcome" and "exposure"}
}
\value{
A string that can be passed to \link{parse_effect}
}
\description{
Define default effect for a given graph
}
\examples{
graphres <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y)
V(graphres)$leftside <- c(1, 0, 0, 1, 0)
V(graphres)$latent <- c(0, 0, 0, 1, 1)
V(graphres)$nvals <- c(3, 2, 2, 2, 2)
V(graphres)$exposure <- c(0, 1, 0, 0, 0)
V(graphres)$outcome <- c(0, 0, 1, 0, 0)
E(graphres)$rlconnect <- c(0, 0, 0, 0, 0)
E(graphres)$edge.monotone <- c(0, 0, 0, 0, 0)
get_default_effect(graphres = graphres) == "p{Y(X = 1)=1} - p{Y(X = 0)=1}" # TRUE
}
