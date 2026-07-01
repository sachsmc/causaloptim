# Create linear causal problem from causal model and effect

A more flexible alternative to
[analyze_graph](https://sachsmc.github.io/causaloptim/reference/analyze_graph.md)
that takes as inputs the causal model and effect.

## Usage

``` r
create_linearcausalproblem(causal_model, effectt)
```

## Arguments

- causal_model:

  An object of class "causalmodel" as produce by
  [create_causalmodel](https://sachsmc.github.io/causaloptim/reference/create_causalmodel.md)

- effectt:

  A character string that represents the causal effect of interest

## Value

A an object of class "linearcausalproblem", which is a list with the
following components. This list can be passed to
[optimize_effect_2](https://sachsmc.github.io/causaloptim/reference/optimize_effect_2.md)
which interfaces with the symbolic optimization program. Print and plot
methods are also available.

- variables:

  Character vector of variable names of potential outcomes, these start
  with 'q' to match Balke's notation

- parameters:

  Character vector of parameter names of observed probabilities, these
  start with 'p' to match Balke's notation

- constraints:

  Character vector of parsed constraints

- objective:

  Character string defining the objective to be optimized in terms of
  the variables

- p.vals:

  Matrix of all possible values of the observed data vector,
  corresponding to the list of parameters.

- q.vals:

  Matrix of all possible values of the response function form of the
  potential outcomes, corresponding to the list of variables.

- parsed.query:

  A nested list containing information on the parsed causal query.

- objective.nonreduced:

  The objective in terms of the original variables, before algebraic
  variable reduction. The nonreduced variables can be obtained by
  concatenating the columns of q.vals.

- response.functions:

  List of response functions.

- graph:

  The graph as passed to the function.

- R:

  A matrix with coefficients relating the p.vals to the q.vals p = R \*
  q

- c0:

  A vector of coefficients relating the q.vals to the objective function
  theta = c0 \* q

- iqR:

  A matrix with coefficients to represent the inequality constraints

## Details

The effectt parameter describes your causal effect of interest. The
effectt parameter must be of the form

`p{V11(X=a)=a; V12(X=a)=b;...} op1 p{V21(X=b)=a; V22(X=c)=b;...} op2 ...`

where Vij are names of variables in the graph, a, b are numeric values
from 0:(nvals - 1), and op are either - or +. You can specify a single
probability statement (i.e., no operator). Note that the probability
statements begin with little p, and use curly braces, and items inside
the probability statements are separated by ;. The variables may be
potential outcomes which are denoted by parentheses. Variables may also
be nested inside potential outcomes. Pure observations such as
`p{Y = 1}` are not allowed if the left side contains any variables.
There are 2 important rules to follow: 1) Only variables on the right
side can be in the probability events, and if the left side is not
empty: 2) none of the variables in the left side that are intervened
upon can have any children in the left side, and all paths from the left
to the right must be blocked by the intervention set. Here the
intervention set is anything that is inside the smooth brackets (i.e.,
variable set to values).

All of the following are valid effect statements:

`p{Y(X = 1) = 1} - p{Y(X = 0) = 1}`

`p{X(Z = 1) = 1; X(Z = 0) = 0}`

`p{Y(M(X = 0), X = 1) = 1} - p{Y(M(X = 0), X = 0) = 1}`

## Examples

``` r
### confounded exposure and outcome
b <- initialize_graph(igraph::graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y))
confmod <- create_causalmodel(graph = b, prob.form =  list(out = c("X", "Y"), cond = NULL))
create_linearcausalproblem(confmod, effectt = "p{Y(X = 1) = 1}")
#> Ready to compute bounds for the effect p{Y(X = 1) = 1} 
#>  Under the assumption encoded in the graph: + 3/3 edges from 08a197f (vertex names):
#> [1] X ->Y Ur->X Ur->Y
#> Number of possible values of each variable: 
#>  X: 2, Y: 2 
#> No constraints have been specified 
#>  The bounds will be reported in terms of parameters of the form pab_, which represents the probability P(X = a, Y = b). 
#> Additional information is available in the following list elements: [1] "variables"            "parameters"           "constraints"         
#>  [4] "objective"            "p.vals"               "q.vals"              
#>  [7] "parsed.query"         "unparsed.query"       "user.constraints"    
#> [10] "objective.nonreduced" "response.functions"   "graph"               
#> [13] "R"                    "c0"                   "causal_model"        
```
