library(causaloptim)


## simple unconfounded X -> Y

b <- readRDS("test-graphs/simple-unconfounded.RData")
eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\np0_0 - p0_1\n}\n\n", "\nMIN {\np0_0 - p0_1\n}\n\n"))

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
all(new_version_bound$bounds == c("min-bound: MAX {\n  p0_0 - p0_1\n}\n", "max-bound: MIN {\n  p0_0 - p0_1\n}\n"))
# visual comparison:
cat(bound$bounds) # old version output string
cat(new_version_bound$bounds) # new version output string

## simple confounded X -> Y

b <- readRDS("test-graphs/simple-confounded.RData")
eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\n- p10_ - p01_\n}\n\n", "\nMIN {\n- p10_ - p01_ + 1\n}\n\n"))

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
all(new_version_bound$bounds == c("min-bound: MAX {\n  -p10_ - p01_\n}\n", "max-bound: MIN {\n  1 - p10_ - p01_\n}\n"))
# visual comparison:
cat(bound$bounds) # old version output string
cat(new_version_bound$bounds) # new version output string

## instrument Z -> X -> Y

b <- readRDS("test-graphs/instrument.RData")
eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"

obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\np00_0 - p00_1 + p10_0 - 2 p10_1 - 2 p01_1\n- p00_0 + p00_1 - p10_0 - p01_0\n- p00_0 + p00_1 - 2 p10_0 + p10_1 - 2 p01_0\np00_0 - p00_1 - p10_1 - p01_1\n- p10_0 - p01_0\n- p10_1 - p01_1\np00_0 - p00_1 - p10_0 - p10_1 - p01_0\n- p00_0 + p00_1 - p10_0 - p10_1 - p01_1\n}\n\n", "\nMIN {\n- p00_0 - p10_0 - p10_1 - 2 p01_1 + 2\n- p00_1 - p10_0 - p10_1 - 2 p01_0 + 2\n- p10_0 - p01_1 + 1\np00_1 - 2 p10_0 + p10_1 - p01_0 + 1\n- p10_0 - p01_0 + 1\n- p10_1 - p01_0 + 1\n- p10_1 - p01_1 + 1\np00_0 + p10_0 - 2 p10_1 - p01_1 + 1\n}\n\n" ))

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
all(new_version_bound$bounds == c("min-bound: MAX {\n  p00_0 - p00_1 - p10_1 - p01_1,\n  p00_0 - p00_1 - p10_0 - p10_1 - p01_0,\n  p00_0 - p00_1 + p10_0 - 2p10_1 - 2p01_1,\n  -p10_1 - p01_1,\n  -p10_0 - p01_0,\n  -p00_0 + p00_1 - 2p10_0 + p10_1 - 2p01_0,\n  -p00_0 + p00_1 - p10_0 - p10_1 - p01_1,\n  -p00_0 + p00_1 - p10_0 - p01_0\n}\n", "max-bound: MIN {\n  1 - p10_1 - p01_0,\n  1 + p00_0 + p10_0 - 2p10_1 - p01_1,\n  2 - p00_1 - p10_0 - p10_1 - 2p01_0,\n  1 - p10_1 - p01_1,\n  1 - p10_0 - p01_0,\n  1 + p00_1 - 2p10_0 + p10_1 - p01_0,\n  2 - p00_0 - p10_0 - p10_1 - 2p01_1,\n  1 - p10_0 - p01_1\n}\n"))
# visual comparison:
cat(bound$bounds) # old version output string
cat(new_version_bound$bounds) # new version output string

## with monotonocity

mono <- list("X(Z = 1) >= X(Z = 0)")

obj <- analyze_graph(b, constraints = mono, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\np00_0 - p00_1 - p10_1 - p01_1\n}\n\n", "\nMIN {\n- p10_1 - p01_0 + 1\n}\n\n"))

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
all(new_version_bound$bounds == c("min-bound: MAX {\n  p00_0 - p00_1 - p10_1 - p01_1\n}\n", "max-bound: MIN {\n  1 - p10_1 - p01_0\n}\n"))
# visual comparison:
cat(bound$bounds) # old version output string
cat(new_version_bound$bounds) # new version output string

## compliers minus defiers effect

eff <- "p{X(Z = 1) = 1; X(Z = 0) = 0} - p{X(Z = 1) = 0; X(Z = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\np00_0 - p00_1 + p01_0 - p01_1\n}\n\n", "\nMIN {\np00_0 - p00_1 + p01_0 - p01_1\n}\n\n"))

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
all(new_version_bound$bounds == c("min-bound: MAX {\n  p00_0 - p00_1 + p01_0 - p01_1\n}\n", "max-bound: MIN {\n  p00_0 - p00_1 + p01_0 - p01_1\n}\n"))
# visual comparison:
cat(bound$bounds) # old version output string
cat(new_version_bound$bounds) # new version output string

## treatment effect among the treated? This one is not actually liner under the dag

eff <- "p{Y(X = 1) = 1; X = 1} - p{Y(X = 0) = 1; X = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
#all(new_version_bound$bounds == c("min-bound: MAX {\n  p00_0 - p00_1 - p10_0 - p10_1,\n  p00_0 - p00_1 - p10_1 - p01_1,\n  p00_0 - p00_1 + p10_0 - 2p10_1 - 2p01_1,\n  -p10_0 - p01_0,\n  -p10_1 - p01_1,\n  -p00_0 + p00_1 - 2p10_0 + p10_1 - 2p01_0,\n  -p00_0 + p00_1 - p10_0 - p10_1,\n  -p00_0 + p00_1 - p10_0 - p01_0\n}\n", "max-bound: MIN {\n  1 - p10_1 - p01_0,\n  2 - p00_0 - p00_1 - p10_0 - p10_1 - 2p01_0,\n  1 + p00_0 + p10_0 - 2p10_1 - p01_1,\n  1 - p10_1 - p01_1,\n  1 - p10_0 - p01_0,\n  1 + p00_1 - 2p10_0 + p10_1 - p01_0,\n  1 - p10_0 - p01_1,\n  2 - p00_0 - p00_1 - p10_0 - p10_1 - 2p01_1\n}\n"))
# visual comparison:
cat(bound$bounds) # old version output string
cat(new_version_bound$bounds) # new version output string

## mediator X -> Z -> Y
## bounds should match https://onlinelibrary.wiley.com/doi/full/10.1111/j.1541-0420.2007.00949.x


b <- readRDS("test-graphs/mediator.RData")

## total effect: identifiable

eff <- "p{Y(X = 1) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
#bound1 <- optimize_effect(obj) # with old version
# with new version of optimize_effect:
new_version_bound1 <- optimize_effect_2(obj)
# visual comparison:
#cat(bound1$bounds) # old version output string
#cat(new_version_bound1$bounds) # new version output string

eff <- "p{Y(X = 1, Z(X = 1)) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
#bound2 <- optimize_effect(obj) # with old version
# with new version of optimize_effect:
new_version_bound2 <- optimize_effect_2(obj)
# visual comparison:
#cat(bound2$bounds) # old version output string
#cat(new_version_bound2$bounds) # new version output string

#all(bound1$bounds == bound2$bounds)
all(new_version_bound1$bounds == new_version_bound2$bounds) # with new version

## controlled direct effect

eff <- "p{Y(X = 1, Z = 0) = 1} - p{Y(X = 0, Z = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
#bound <- optimize_effect(obj) # with old version


## check against equation (3)
#all(bound$bounds == c("\nMAX {\np00_0 + p01_1 - 1\n}\n\n", "\nMIN {\n- p00_1 - p01_0 + 1\n}\n\n"))

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
all(new_version_bound$bounds == c("min-bound: MAX {\n  -1 + p00_0 + p01_1\n}\n", "max-bound: MIN {\n  1 - p00_1 - p01_0\n}\n"))
# visual comparison:
#cat(bound$bounds) # old version output string
cat("\nMAX {\np00_0 + p01_1 - 1\n}\n\n", "\nMIN {\n- p00_1 - p01_0 + 1\n}\n\n")#
cat(new_version_bound$bounds) # new version output string

## with monotoncity


mono2 <- list("Z(X = 1) >= Z(X = 0)",
              "Y(X = 1, Z = 0) >= Y(X = 0, Z = 0)",
              "Y(X = 1, Z = 1) >= Y(X = 0, Z = 1)",
              "Y(X = 0, Z = 1) >= Y(X = 0, Z = 0)",
              "Y(X = 1, Z = 1) >= Y(X = 1, Z = 0)")

obj <- analyze_graph(b, constraints = mono2, effectt = eff)
#bound <- optimize_effect(obj) # with old version

## check againts equation (5)
#all(bound$bounds == c("\nMAX {\n- p01_0 + p01_1\n0\n}\n\n", "\nMIN {\n- p00_1 - p10_1 - p01_0 + 1\n}\n\n"))

# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
all(new_version_bound$bounds == c("min-bound: MAX {\n  -p01_0 + p01_1,\n  0\n}\n", "max-bound: MIN {\n  1 - p00_1 - p10_1 - p01_0\n}\n"))
# visual comparison:
#cat(bound$bounds) # old version output string
cat("\nMAX {\n- p01_0 + p01_1\n0\n}\n\n", "\nMIN {\n- p00_1 - p10_1 - p01_0 + 1\n}\n\n")#
cat(new_version_bound$bounds) # new version output string

## natural direct effect

eff <- "p{Y(X = 1, Z(X = 0)) = 1} - p{Y(X = 0, Z(X = 0)) = 1}"

obj <- analyze_graph(b, constraints = NULL, effectt = eff)
#bound <- optimize_effect(obj) # with old version
# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
#all(new_version_bound$bounds == c("min-bound: MAX {\n  -p00_1 + p10_0 - p10_1 - p01_0 - p01_1,\n  -2 + 2p00_0 + p10_0 + p01_0 + p01_1,\n  -1 + p00_0 + p10_0\n}\n", "max-bound: MIN {\n  2p00_0 + p10_0 - p10_1 + p01_0,\n  p00_0 + p10_0,\n  1 - p00_1 + p10_0 - p01_0\n}\n"))
# visual comparison:
#cat(bound$bounds) # old version output string
#cat(new_version_bound$bounds) # new version output string

obj <- analyze_graph(b, constraints = mono2, effectt = eff)
#bound <- optimize_effect(obj) # with old version
# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
#all(new_version_bound$bounds == c("min-bound: MAX {\n  p10_0 - p10_1 - p01_0 + p01_1,\n  p10_0 - p10_1,\n  -p01_0 + p01_1,\n  0\n}\n", "max-bound: MIN {\n  p00_0 - p00_1 + p10_0 - p10_1\n}\n"))
# visual comparison:
#cat(bound$bounds) # old version output string
#cat(new_version_bound$bounds) # new version output string

## compare to Arvids paper 2009 


obj <- analyze_graph(b, constraints = list("Z(X=0)<=Z(X=1)"), effectt = eff)
#bound <- optimize_effect(obj) # with old version
# with new version of optimize_effect:
new_version_bound <- optimize_effect_2(obj)
#all(new_version_bound$bounds == c("min-bound: MAX {\n  p10_0 - p10_1 - p01_0 + p01_1,\n  -1 + p00_0 + p10_0 + p01_1\n}\n", "max-bound: MIN {\n  p00_0 - p00_1 + p10_0,\n  2p00_0 - 2p00_1 + p10_0 - p10_1 + p01_0 - p01_1\n}\n"))
# visual comparison:
#cat(bound$bounds) # old version output string
#cat(new_version_bound$bounds) # new version output string

## error check

graph <- graph_from_literal(Ul -+ X -+ Y, Ur -+ Y, W -+ Y, Ur -+ W)
V(graph)$leftside <- c(1, 1, 0, 0, 0)
V(graph)$latent <- c(1, 0, 0, 1, 0)
E(graph)$rlconnect <- c(0, 0, 0, 0, 0)
E(graph)$edge.monotone <- c(0, 0, 0, 0, 0)

tryerror <- tryCatch(analyze_graph(graph, NULL, 'p{Y(W = 1) = 0}'), 
                     error = function(e) TRUE)
tryerror