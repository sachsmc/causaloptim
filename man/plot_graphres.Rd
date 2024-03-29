% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph-utilities.R
\name{plot_graphres}
\alias{plot_graphres}
\title{Plot the analyzed graph object}
\usage{
plot_graphres(graphres)
}
\arguments{
\item{graphres}{an igraph object}
}
\value{
None
}
\description{
Special plotting method for igraphs of this type
}
\examples{
b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
V(b)$leftside <- c(0,0,0)
V(b)$latent <- c(0,0,1)
V(b)$nvals <- c(2,2,2)
V(b)$exposure <- c(1,0,0)
V(b)$outcome <- c(0,1,0)
E(b)$rlconnect <- c(0,0,0)
E(b)$edge.monotone <- c(0,0,0)
plot(b)
}
\seealso{
\link{plot.linearcausalproblem} which plots a graph with attributes
}
