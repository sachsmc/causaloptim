## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

results <- readRDS("example.RData")

## ----setup---------------------------------------------------------------
library(causaloptim)

## ---- eval = FALSE-------------------------------------------------------
#  results <- specify_graph()

## ------------------------------------------------------------------------
names(results)

print(results$boundsFunction)

## ------------------------------------------------------------------------
sim <- simulate_bounds(results$obj, results$bounds.obs, nsim = 100)
head(sim)

