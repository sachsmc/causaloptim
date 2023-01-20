## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(causaloptim)

b <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y)
V(b)$leftside <- c(1,0,0,1,0)
V(b)$latent <- c(0,0,0,1,1)
V(b)$nvals <- c(2,2,2,2,2)
E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0,0,0)

obj <- analyze_graph(b, constraints = "X(Z = 1) >= X(Z = 0)", effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
opp <- optimize_effect_2(obj)

results <- list(graphres = b, obj = obj, bounds.obs = opp, 
                constraints = "X(Z = 1) >= X(Z = 0)", effect = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}",
                boundsFunction = interpret_bounds(opp$bounds, obj$parameters))

## ----setup, eval = FALSE------------------------------------------------------
#  library(causaloptim)

## ---- eval = FALSE------------------------------------------------------------
#  results <- specify_graph()

## -----------------------------------------------------------------------------
names(results)

print(results$bounds.obs)

print(results$boundsFunction)

## -----------------------------------------------------------------------------
sim <- simulate_bounds(results$obj, results$bounds.obs, nsim = 100)
head(sim)

## ---- results = "asis"--------------------------------------------------------
cat(latex_bounds(results$bounds.obs$bounds, results$obj$parameters))


