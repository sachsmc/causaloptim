# Find all paths in a causal model

Given a set of response functions, find all directed paths from from to
to

## Usage

``` r
find_all_paths(respvars, from, to)
```

## Arguments

- respvars:

  A set of response functions as created by
  [create_response_function](https://sachsmc.github.io/causaloptim/reference/create_response_function.md)

- from:

  A character string indicating the start of the path

- to:

  A character string indicating the end of the path

## Value

A list with all the paths or a list with NULL if there are none

## Examples

``` r
 b <- initialize_graph(igraph::graph_from_literal(X -+ Z, Z -+ Y, X -+ Y, Ur -+ Z, Ur -+ Y))
 medmod <- create_response_function(b)
 find_all_paths(medmod, "X", "Y")
#> [1] "X -> Y"      "X -> Z -> Y"
 igraph::all_simple_paths(b, "X", "Y", mode = "out")
#> [[1]]
#> + 3/4 vertices, named, from ead6346:
#> [1] X Z Y
#> 
#> [[2]]
#> + 2/4 vertices, named, from ead6346:
#> [1] X Y
#> 
```
