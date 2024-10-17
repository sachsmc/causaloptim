#' @import igraph shiny
#' @importFrom graphics legend plot
#' @importFrom stats runif rgamma
#' @importFrom rcdd makeH makeV scdd
NULL


#' Analyze the causal graph and effect to determine constraints and objective
#' 
#' The graph must contain certain edge and vertex attributes which are documented
#' in the Details below. The shiny app run by \link{specify_graph} will return a
#' graph in this format. 
#' 
#' @param graph An \link[igraph]{igraph-package} object that represents a directed acyclic graph with certain attributes. See Details.
#' @param constraints A vector of character strings that represent the constraints on counterfactual quantities
#' @param effectt A character string that represents the causal effect of interest
#' 
#' @details The graph object must contain the following named vertex attributes: \describe{
#' \item{name}{The name of each vertex must be a valid R object name starting with a letter and no special characters. Good candidate names are for example, Z1, Z2, W2, X3, etc. }
#' \item{leftside}{An indicator of whether the vertex is on the left side of the graph, 1 if yes, 0 if no.}
#' \item{latent}{An indicator of whether the variable is latent (unobserved). There should always be a variable Ul on the left side that is latent and a parent of all variables on the left side, and another latent variable Ur on the right side that is a parent of all variables on the right side. }
#' \item{nvals}{The number of possible values that the variable can take on, the default and minimum is 2 for 2 categories (0,1). In general, a variable with nvals of K can take on values 0, 1, ..., (K-1).}
#' }
#' In addition, there must be the following edge attributes: \describe{
#' \item{rlconnect}{An indicator of whether the edge goes from the right side to the left side. Should be 0 for all edges.}
#' \item{edge.monotone}{An indicator of whether the effect of the edge is monotone, meaning that if V1 -> V2 and the edge is monotone, then a > b implies V2(V1 = a) >= V2(V1 = b). Only available for binary variables (nvals = 2).}
#' }
#' The effectt parameter describes your causal effect of interest. The effectt parameter must be of the form
#' 
#' \code{p{V11(X=a)=a; V12(X=a)=b;...} op1 p{V21(X=b)=a; V22(X=c)=b;...} op2 ...}
#' 
#' where Vij are names of variables in the graph, a, b are numeric values from 0:(nvals - 1), and op are either - or +. You can specify a single probability statement (i.e., no operator). Note that the probability statements begin with little p, and use curly braces, and items inside the probability statements are separated by ;. The variables may be potential outcomes which are denoted by parentheses. Variables may also be nested inside potential outcomes. Pure observations such as \code{p{Y = 1}} are not allowed if the left side contains any variables. 
#' There are 2 important rules to follow: 1) Only variables on the right side can be in the probability events, and if the left side is not empty: 2) none of the variables in the left side that are intervened upon can have any children in the left side, and all paths from the left to the right must be blocked by the intervention set. Here the intervention set is anything that is inside the smooth brackets (i.e., variable set to values).
#' 
#'  All of the following are valid effect statements:
#' 
#' \code{p{Y(X = 1) = 1} - p{Y(X = 0) = 1}}
#' 
#' \code{p{X(Z = 1) = 1; X(Z = 0) = 0}}
#' 
#' \code{p{Y(M(X = 0), X = 1) = 1} - p{Y(M(X = 0), X = 0) = 1}}
#' 
#' The constraints are specified in terms of potential outcomes to constrain by writing the potential outcomes, values of their parents, and operators that determine the constraint (equalities or inequalities). For example,
#' \code{X(Z = 1) >= X(Z = 0)}
#' 
#' @return A an object of class "linearcausalproblem", which is a list with the
#'   following components. This list can be passed to \link{optimize_effect_2}
#'   which interfaces with the symbolic optimization program. Print and plot methods are also
#'   available. \describe{ 
#'   \item{variables}{Character vector of variable names
#'   of potential outcomes, these start with 'q' to match Balke's notation}
#'   \item{parameters}{Character vector of parameter names of observed
#'   probabilities, these start with 'p' to match Balke's notation}
#'   \item{constraints}{Character vector of parsed constraints}
#'   \item{objective}{Character string defining the objective to be optimized in
#'   terms of the variables} 
#'   \item{p.vals}{Matrix of all possible values of the
#'   observed data vector, corresponding to the list of parameters.}
#'   \item{q.vals}{Matrix of all possible values of the response function form
#'   of the potential outcomes, corresponding to the list of variables.}
#'   \item{parsed.query}{A nested list containing information on the parsed
#'   causal query.} 
#'   \item{objective.nonreduced}{The objective in terms of the
#'   original variables, before algebraic variable reduction. The nonreduced
#'   variables can be obtained by concatenating the columns of q.vals.}
#'   \item{response.functions}{List of response functions.} 
#'   \item{graph}{The graph as passed to the function.} 
#'   \item{R}{A matrix with coefficients
#'   relating the p.vals to the q.vals p = R * q} 
#'   \item{c0}{A vector of coefficients relating the q.vals to the 
#'   objective function theta = c0 * q} 
#'   \item{iqR}{A matrix with coefficients to represent the inequality
#'   constraints} }
#'
#' @export
#' @examples 
#' ### confounded exposure and outcome

#' b <- initialize_graph(igraph::graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y))
#' analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")

analyze_graph <- function(graph, constraints, effectt) {
    if (length(E(graph)) == 0 | effectt == "") {
        return(NULL)
    }
    leftind <- vertex_attr(graph)$leftside
    
    if(sum(edge_attr(graph)$rlconnect) > 0) stop("No edges can go from right to left")
    
    cond.vars <- V(graph)[leftind == 1 & names(V(graph)) != "Ul"]
    right.vars <- V(graph)[leftind == 0 & names(V(graph)) != "Ur"] 
   
    obsvars <- c(right.vars, cond.vars)
    observed.variables <- V(graph)[V(graph)$latent == 0]
    
    var.values <- lapply(names(observed.variables), function(varname) seq(from = 0, to = numberOfValues(graph, varname) - 1))
    names(var.values) <- names(observed.variables)
    
    p.vals <- do.call(expand.grid, var.values)  # p vals need to be based on observed variables only
    
    jd <- do.call(paste0, p.vals[, names(right.vars[right.vars$latent == 0]), drop = FALSE])
    cond <- do.call(paste0, p.vals[, names(cond.vars[cond.vars$latent == 0]), drop = FALSE])
    
    causalmodel <- create_causalmodel(graph = graph, respvars = NULL, 
                                      p.vals = p.vals, 
                                      prob.form = list(out = names(right.vars[right.vars$latent == 0]), 
                                                       cond = names(cond.vars[cond.vars$latent == 0])), 
                                      constraints = constraints)
    
    create_linearcausalproblem(causalmodel, effectt)
    
    
}

#' Plot the graph from the causal problem with a legend describing attributes
#' 
#' @param x object of class "linearcausalproblem"
#' @param ... Not used
#' @return Nothing
#' @seealso \link{plot_graphres} which plots the graph only
#' @export
#' @examples
#' b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(2,2,2)
#' V(b)$exposure <- c(1,0,0)
#' V(b)$outcome <- c(0,1,0)
#' E(b)$rlconnect <- c(0,0,0)
#' E(b)$edge.monotone <- c(0,0,0)
#' q <- "p{Y(X=1)=1}-p{Y(X=0)=1}"
#' obj <- analyze_graph(graph = b, constraints = NULL, effectt <- q)
#' plot(obj)

plot.linearcausalproblem <- function(x, ...) {
  
    if(is.null(x$graph)) {
        message("No graph is present to describe the causal model. Nothing to plot.")
    } else {
        plot_graphres(x$graph)
    }
  
}



#' Print the causal problem
#' 
#' @param x object of class "linearcausaloptim"
#' @param ... Not used
#' @return x, invisibly
#' @export

print.linearcausalproblem <- function(x, ...) {
  
  effecttext <- sprintf("Ready to compute bounds for the effect %s", x$unparsed.query)
  
  lkey <- letters[1:length(attr(x$parameters, "rightvars"))]
  rkey <- letters[(length(attr(x$parameters, "rightvars")) + 1):(length(attr(x$parameters, "rightvars")) + 
                                                                       length(attr(x$parameters, "condvars")))]
  
  if(length(attr(x$parameters, "condvars")) == 0) rkey <- NULL
  
  sampparm <- paste0("p", paste(lkey, collapse = ""), "_", 
                     paste(rkey, collapse = ""))
  
  probstate <- paste0("P(", paste(paste0(attr(x$parameters, "rightvars"), " = ", lkey), collapse = ", "), " | ", 
                      paste0(attr(x$parameters, "condvars"), " = ", rkey, collapse = ", "), ")")
  
  if(length(attr(x$parameters, "condvars")) == 0) {
    probstate <- paste0("P(", paste(paste0(attr(x$parameters, "rightvars"), " = ", lkey), collapse = ", "), ")")
  }
  
  variabletext <- sprintf("The bounds will be reported in terms of parameters of the form %s, which represents the probability %s.", 
                          sampparm, probstate)
  
  if(!is.null(x$user.constraints)) {
    constrainttext <- sprintf("This following constraints have been specifed: \n %s", paste(x$user.constraints, collapse = "\n"))
  } else constrainttext <- "No constraints have been specified"
  
  if(!is.null(x$graph)) {
    cat(effecttext, "\n Under the assumption encoded in the graph: ")
    print(E(x$graph))
    cat("Number of possible values of each variable:", "\n", print_nvals(x$graph), "\n")
  } else {
      cat(effecttext, "\n Under the assumptions encoded in the response functions. \n")
  }
    cat(constrainttext, "\n", variabletext, "\n")
  
  cat("Additional information is available in the following list elements:")
  print(names(x))
  
  invisible(x)
}



