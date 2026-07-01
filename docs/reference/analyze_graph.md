# Analyze the causal graph and effect to determine constraints and objective

The graph must contain certain edge and vertex attributes which are
documented in the Details below. The shiny app run by
[specify_graph](https://sachsmc.github.io/causaloptim/reference/specify_graph.md)
will return a graph in this format.

## Usage

``` r
analyze_graph(graph, constraints, effectt)
```

## Arguments

- graph:

  An
  [igraph-package](https://r.igraph.org/reference/aaa-igraph-package.html)
  object that represents a directed acyclic graph with certain
  attributes. See Details.

- constraints:

  A vector of character strings that represent the constraints on
  counterfactual quantities

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

The graph object must contain the following named vertex attributes:

- name:

  The name of each vertex must be a valid R object name starting with a
  letter and no special characters. Good candidate names are for
  example, Z1, Z2, W2, X3, etc.

- leftside:

  An indicator of whether the vertex is on the left side of the graph, 1
  if yes, 0 if no.

- latent:

  An indicator of whether the variable is latent (unobserved). There
  should always be a variable Ul on the left side that is latent and a
  parent of all variables on the left side, and another latent variable
  Ur on the right side that is a parent of all variables on the right
  side.

- nvals:

  The number of possible values that the variable can take on, the
  default and minimum is 2 for 2 categories (0,1). In general, a
  variable with nvals of K can take on values 0, 1, ..., (K-1).

In addition, there must be the following edge attributes:

- rlconnect:

  An indicator of whether the edge goes from the right side to the left
  side. Should be 0 for all edges.

- edge.monotone:

  An indicator of whether the effect of the edge is monotone, meaning
  that if V1 -\> V2 and the edge is monotone, then a \> b implies V2(V1
  = a) \>= V2(V1 = b). Only available for binary variables (nvals = 2).

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

The constraints are specified in terms of potential outcomes to
constrain by writing the potential outcomes, values of their parents,
and operators that determine the constraint (equalities or
inequalities). For example, `X(Z = 1) >= X(Z = 0)`

## Examples

``` r
### confounded exposure and outcome
b <- initialize_graph(igraph::graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y))
analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#> Ready to compute bounds for the effect p{Y(X = 1) = 1} - p{Y(X = 0) = 1} 
#>  Under the assumption encoded in the graph: + 3/3 edges from 4514554 (vertex names):
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
