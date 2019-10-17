library(causaloptim)


## simple unconfounded X -> Y

b <- readRDS("tests/test-graphs/simple-unconfounded.RData")
eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\np0_0 - p0_1\n}\n\n", "\nMIN {\np0_0 - p0_1\n}\n\n"))

## simple confounded X -> Y

b <- readRDS("tests/test-graphs/simple-confounded.RData")
eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\n- p10_ - p01_\n}\n\n", "\nMIN {\n- p10_ - p01_ + 1\n}\n\n"))

## instrument Z -> X -> Y

b <- readRDS("tests/test-graphs/instrument.RData")
eff <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"

obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\np00_0 - p00_1 + p10_0 - 2 p10_1 - 2 p01_1\n- p00_0 + p00_1 - p10_0 - p01_0\n- p00_0 + p00_1 - 2 p10_0 + p10_1 - 2 p01_0\np00_0 - p00_1 - p10_1 - p01_1\n- p10_0 - p01_0\n- p10_1 - p01_1\np00_0 - p00_1 - p10_0 - p10_1 - p01_0\n- p00_0 + p00_1 - p10_0 - p10_1 - p01_1\n}\n\n", "\nMIN {\n- p00_0 - p10_0 - p10_1 - 2 p01_1 + 2\n- p00_1 - p10_0 - p10_1 - 2 p01_0 + 2\n- p10_0 - p01_1 + 1\np00_1 - 2 p10_0 + p10_1 - p01_0 + 1\n- p10_0 - p01_0 + 1\n- p10_1 - p01_0 + 1\n- p10_1 - p01_1 + 1\np00_0 + p10_0 - 2 p10_1 - p01_1 + 1\n}\n\n" ))

## with monotonocity

mono <- list("X(Z = 1) >= X(Z = 0)")

obj <- analyze_graph(b, constraints = mono, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\np00_0 - p00_1 - p10_1 - p01_1\n}\n\n", "\nMIN {\n- p10_1 - p01_0 + 1\n}\n\n"))

## compliers minus defiers effect

eff <- "p{X(Z = 1) = 1; X(Z = 0) = 0} - p{X(Z = 1) = 0; X(Z = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

all(bound$bounds == c("\nMAX {\np00_0 - p00_1 + p01_0 - p01_1\n}\n\n", "\nMIN {\np00_0 - p00_1 + p01_0 - p01_1\n}\n\n"))

## mediator X -> Z -> Y
## bounds should match https://onlinelibrary.wiley.com/doi/full/10.1111/j.1541-0420.2007.00949.x


b <- readRDS("tests/test-graphs/mediator.RData")

## controlled direct effect

eff <- "p{Y(X = 1, Z = 0) = 1} - p{Y(X = 0, Z = 0) = 1}"
obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)


## check against equation (3)
all(bound$bounds == c("\nMAX {\np00_0 + p01_1 - 1\n}\n\n", "\nMIN {\n- p00_1 - p01_0 + 1\n}\n\n"))


## with monotoncity


mono2 <- list("Z(X = 1) >= Z(X = 0)",
              "Y(X = 1, Z = 0) >= Y(X = 0, Z = 0)",
              "Y(X = 1, Z = 1) >= Y(X = 0, Z = 1)",
              "Y(X = 0, Z = 1) >= Y(X = 0, Z = 0)",
              "Y(X = 1, Z = 1) >= Y(X = 1, Z = 0)")

obj <- analyze_graph(b, constraints = mono2, effectt = eff)
bound <- optimize_effect(obj)

## check againts equation (5)
all(bound$bounds == c("\nMAX {\n- p01_0 + p01_1\n0\n}\n\n", "\nMIN {\n- p00_1 - p10_1 - p01_0 + 1\n}\n\n"))


## natural direct effect

eff <- "p{Y(X = 1, Z(X = 0)) = 1} - p{Y(X = 0, Z(X = 0)) = 1}"

obj <- analyze_graph(b, constraints = NULL, effectt = eff)
bound <- optimize_effect(obj)

obj <- analyze_graph(b, constraints = mono2, effectt = eff)
bound <- optimize_effect(obj)

## no idea if these are right or published 