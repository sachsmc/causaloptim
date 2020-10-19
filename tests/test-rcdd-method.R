library(causaloptim)
library(rcdd)

b <- readRDS("tests/test-graphs/instrument.RData")
eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"

obj <- analyze_graph(b, NULL, eff)


hrep0dual <- makeH(a1 = t(obj$R), b1 = obj$c0)
vrep0dual <- scdd(hrep0dual)
vrep0dualmatrix <- vrep0dual$output
vertexindices0dual <- vrep0dualmatrix[ , 1] == 0 & vrep0dualmatrix[ , 2] == 1
vertices0dual <- vrep0dualmatrix[vertexindices0dual, -c(1, 2)]
