#' @import igraph shiny
#' @importFrom graphics legend plot
#' @importFrom stats runif
#' @importFrom rcdd makeH scdd
NULL


#' Analyze the causal graph to determine constraints and objective
#' 
#' The graph must contain edge attributes named "leftside" and "lrconnect"
#' that takes values 0 and 1. Only one edge may have a value 1 for lrconnect. 
#' The shiny app returns a graph in this format. 
#' 
#' @param graph An \link[igraph]{aaa-igraph-package} object that represents a directed acyclic graph
#' @param constraints A vector of character strings that represent the constraints
#' @param effectt A character string that represents the causal effect of interest
#' 
#' @return A an object of class "linearcausalproblem", which is a list with the
#'   following components. This list can be passed to \link{optimize_effect}
#'   which interfaces with Balke's code. Print and plot methods are also
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

#' b <- igraph::graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
#' analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")

analyze_graph <- function(graph, constraints, effectt) {
    
    leftind <- vertex_attr(graph)$leftside
    
    if(sum(edge_attr(graph)$rlconnect) > 0) stop("No edges can go from right to left")
    
    cond.vars <- V(graph)[leftind == 1 & names(V(graph)) != "Ul"]
    right.vars <- V(graph)[leftind == 0 & names(V(graph)) != "Ur"] 
   
    obsvars <- c(right.vars, cond.vars)
    observed.variables <- V(graph)[V(graph)$latent == 0]
    
    var.values <- lapply(names(observed.variables), function(i) c(0, 1))
    names(var.values) <- names(observed.variables)
    
    p.vals <- do.call(expand.grid, var.values)  # p vals need to be based on observed variables only
    
    jd <- do.call(paste0, p.vals[, names(right.vars[right.vars$latent == 0]), drop = FALSE])
    cond <- do.call(paste0, p.vals[, names(cond.vars[cond.vars$latent == 0]), drop = FALSE])
    
    parameters <- paste0("p", paste(jd, cond, sep = "_"))
    parameters.key <- paste(paste(names(right.vars[right.vars$latent == 0]), collapse = ""), 
                            paste(names(cond.vars[cond.vars$latent == 0]), collapse = ""), sep = "_")
    
    
    ## response variable for each variable observed or unobserved
    
    respvars <- create_response_function(graph, right.vars, cond.vars)
    
    ## matrix of unobserved counterfactual probabilities
    
    q.list <- create_q_matrix(respvars, right.vars, cond.vars, constraints)
    variables <- as.character(unique(q.list$q.vals.all.lookup$vars))
    ## constraints identify set of qs that correspond to observed p.vals
    
    linconstr.list <- create_R_matrix(graph, obsvars, respvars, 
                                      p.vals, parameters, q.list, variables)
    
    ## determine objective based on exposure and outcome in terms of qs

    effect <- parse_effect(effectt)
    
    chk0 <- lapply(effect$vars, btm_var)
    
    interven.vars <- unique(unlist(chk0))
    
    ## check that children of intervention sets are on the right
    
    any.children.onleft <- sapply(interven.vars, function(v) {
      
      children <- neighbors(graph, V(graph)[v], mode = "out")
      any(children$leftside == 1)
      
    })
    
    if(any(any.children.onleft) == TRUE) {
      stop(sprintf("Cannot intervene on %s because it has children on the leftside!", 
                       paste(interven.vars[which(any.children.onleft)], collapse = ", ")))
    }
    
    if("oper" %in% names(chk0) & !chk0["oper"] %in% c("+", "-")) {
      stop(sprintf("Operator '%s' not allowed!", chk0["oper"]))
    }
    
    allnmes <- unique(unlist(lapply(effect$vars, names)))
    
    realnms <- names(V(graph))
    if(any(!allnmes %in% realnms)) {
      
      stop(sprintf("Names %s in effect not specified in graph!", 
                       paste(allnmes[which(!allnmes %in% realnms)], collapse = ", ")))
      
    }
    
    if(length(names(cond.vars)) > 0) {
      
      chkpaths <- unlist(lapply(cond.vars, function(x){ 
        pths <- all_simple_paths(graph, from = x, to = allnmes, mode = "out")
        unlist(lapply(pths, function(pth) {
          any(interven.vars %in% names(pth))
          
        }))
      }))
      
      if(any(!chkpaths)) {
        stop(sprintf("Leftside variables %s not ancestors of intervention sets. Condition 6 violated.", 
                     paste(names(chkpaths)[!chkpaths], collapse = ", ")))
      }
      
    }
    
    
    ## handle addition and subtraction based on operator
    ## accumulate final effect based on subtraction and addition
    var.eff <- create_effect_vector(effect, graph, obsvars, respvars, q.list, variables)
   
    
    objective <- list(var.eff[[1]])
    if(length(var.eff) > 1 & is.null(effect$oper) | (length(effect$oper) != length(var.eff) -1)){
      stop("Missing operator")
    }
    
    if(!is.null(effect$oper) & length(effect$oper) > 0) {
    curreff <- 2
    for(opp in 1:length(effect$oper)) {
      
      if(effect$oper[[opp]] == "-") {
        
        resss <- symb.subtract(objective[[curreff - 1]], var.eff[[curreff]])
        objective[[curreff - 1]] <- resss[[1]]
        objective[[curreff]] <- resss[[2]]
        curreff <- curreff + 1
        
      } else if(effect$oper[[opp]] == "+") {
        
        objective[[curreff]] <- var.eff[[curreff]]
        curreff <- curreff + 1
        
      }
      
    }
    }
    
    
   
    
    objective.fin <- paste(objective[[1]], collapse = " + ")
    c0 <- matrix(0, nrow = length(variables))
    c0[match(objective[[1]], variables)] <- c0[match(objective[[1]], variables)] + 1
   
    if(!is.null(effect$oper) & length(effect$oper) > 0 & length(objective) > 1) {
      
      for(opp in 1:length(effect$oper)) {
        
        thiscol <- ifelse(effect$oper[[opp]] == "-", " - ", " + ")
        objective.fin <- paste(objective.fin, effect$oper[[opp]], 
                               paste(objective[[opp + 1]], collapse = thiscol))
        c0[match(objective[[opp + 1]], variables)] <- c0[match(objective[[opp + 1]], variables)] + 
          ifelse(thiscol == " - ", -1, 1)
        
      }
      
    }
    
    attr(parameters, "key") <- parameters.key
    attr(parameters, "rightvars") <- names(right.vars[right.vars$latent == 0])
    attr(parameters, "condvars") <- names(cond.vars[cond.vars$latent == 0])
    
    res <- list(variables = variables, parameters = parameters, 
         constraints = linconstr.list$p.constraints, 
         objective = objective.fin, p.vals = p.vals, q.vals = q.list$q.vals, 
         parsed.query = effect, unparsed.query = effectt, 
         user.constraints = constraints, 
         objective.nonreduced = objective, response.functions = respvars, 
         graph = graph, R = linconstr.list$R, c0 = c0)
    class(res) <- "linearcausalproblem"
    res
    
}

#' Plot the graph from the causal problem
#' 
#' @param x object of class "linearcausaloptim"
#' @param ... Not used
#' @return Nothing
#' @export

plot.linearcausalproblem <- function(x, ...) {
  
  plot_graphres(x$graph)
  
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
  
  cat(effecttext, "\n Under the assumption encoded in the graph: ")
  print(E(x$graph))
  cat(constrainttext, "\n", variabletext, "\n")
  
  cat("Additional information is available in the following list elements:")
  print(names(x))
  
  invisible(x)
}



