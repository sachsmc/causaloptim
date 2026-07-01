# Sample a distribution of observable probabilities that satisfy the causal model

Sample a distribution of observable probabilities that satisfy the
causal model

## Usage

``` r
sample_distribution(
  obj,
  simplex_sampler = function(k) {
     rdirichlet(k, alpha = 1)
 }
)
```

## Arguments

- obj:

  An object of class "causalmodel"

- simplex_sampler:

  A function to generate a random sample from the simplex in k
  dimensions, where k is the number of variables (q parameters,
  obj\$data\$variables). By default this is uniform (symmetric dirichlet
  with parameter 1).

## Value

A vector of observable probabilities that satisfy the causal model

## Examples

``` r

graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y))
prob.form <- list(out = c("X", "Y"), cond = "Z")

iv_model <- create_causalmodel(graph, prob.form = prob.form)
sample_distribution(iv_model)
#>     p00_0     p10_0     p01_0     p11_0     p00_1     p10_1     p01_1     p11_1 
#> 0.2118650 0.1877245 0.2308965 0.3695140 0.2485062 0.1850746 0.2028510 0.3635681 
```
