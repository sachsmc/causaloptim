#' Update the effect in a linearcausalproblem object
#' 
#' If you want to use the same graph and response function, but change the 
#' effect of interest, this can save some computation time. 
#' 
#' @param obj An object as returned by \link{analyze_graph}
#' @param effectt A character string that represents the causal effect of interest
#' 
#' @return A object of class linearcausalproblem, see \link{analyze_graph} for details
#' @export
#' @examples
#' b <- igraph::graph_from_literal(X -+ Y, X -+ M, M -+ Y, Ul -+ X, Ur -+ Y, Ur -+ M)
#' V(b)$leftside <- c(1, 0, 0, 1, 0)
#' V(b)$latent <- c(0, 0, 0, 1, 1)
#' V(b)$nvals <- c(2, 2, 2, 2, 2)
#' E(b)$rlconnect <- c(0, 0, 0, 0, 0, 0)
#' E(b)$edge.monotone <- c(0, 0, 0, 0, 0, 0)
#' CDE0_query <- "p{Y(M = 0, X = 1) = 1} - p{Y(M = 0, X = 0) = 1}"
#' CDE0_obj <- analyze_graph(b, constraints = NULL, effectt = CDE0_query)
#' CDE0_bounds <- optimize_effect_2(CDE0_obj)
#' CDE0_boundsfunction <- interpret_bounds(bounds = CDE0_bounds$bounds, 
#' parameters = CDE0_obj$parameters)
#' CDE1_query <- "p{Y(M = 1, X = 1) = 1} - p{Y(M = 1, X = 0) = 1}"
#' CDE1_obj <- update_effect(CDE0_obj, effectt = CDE1_query)
#' CDE1_bounds <- optimize_effect_2(CDE1_obj)
#' CDE1_boundsfunction <- interpret_bounds(bounds = CDE1_bounds$bounds, 
#' parameters = CDE1_obj$parameters)
update_effect <- function(obj, effectt) {
    
    leftind <- vertex_attr(obj$graph)$leftside
    
    if(sum(edge_attr(obj$graph)$rlconnect) > 0) stop("No edges can go from right to left")
    
    cond.vars <- V(obj$graph)[leftind == 1 & names(V(obj$graph)) != "Ul"]
    right.vars <- V(obj$graph)[leftind == 0 & names(V(obj$graph)) != "Ur"] 
    
    obsvars <- c(right.vars, cond.vars)
    observed.variables <- V(obj$graph)[V(obj$graph)$latent == 0]
    
    
    ## determine objective based on exposure and outcome in terms of qs
    
    effect <- parse_effect(effectt)
    
    chk0 <- lapply(effect$vars, btm_var)
    
    interven.vars <- unique(unlist(chk0))
    
    ## check that children of intervention sets are on the right
    
    any.children.onleft <- sapply(interven.vars, function(v) {
        
        children <- neighbors(obj$graph, V(obj$graph)[v], mode = "out")
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
    
    realnms <- names(V(obj$graph))
    if(any(!allnmes %in% realnms)) {
        
        stop(sprintf("Names %s in effect not specified in graph!", 
                     paste(allnmes[which(!allnmes %in% realnms)], collapse = ", ")))
        
    }
    
    if(length(names(cond.vars)) > 0) {
        
        chkpaths <- unlist(lapply(cond.vars, function(x){ 
            pths <- all_simple_paths(obj$graph, from = x, to = allnmes, mode = "out")
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
    q.list <- create_q_matrix(obj$response.functions, right.vars, cond.vars, obj$user.constraints)
    
    var.eff <- create_effect_vector(effect, obsvars, obj$response.functions,
                                    q.list, obj$variables)
    
    
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
    c0 <- matrix(0, nrow = length(obj$variables))
    c0[match(objective[[1]], obj$variables)] <- c0[match(objective[[1]], obj$variables)] + 1
    
    if(!is.null(effect$oper) & length(effect$oper) > 0 & length(objective) > 1) {
        
        for(opp in 1:length(effect$oper)) {
            
            thiscol <- ifelse(effect$oper[[opp]] == "-", " - ", " + ")
            objective.fin <- paste(objective.fin, effect$oper[[opp]], 
                                   paste(objective[[opp + 1]], collapse = thiscol))
            c0[match(objective[[opp + 1]], obj$variables)] <- c0[match(objective[[opp + 1]], obj$variables)] + 
                ifelse(thiscol == " - ", -1, 1)
            
        }
        
    }
    

    obj$parsed.query <- effect
    obj$unparsed.query <- effectt
    obj$objective.nonreduced <- objective
    obj$c0 <- c0
          
    obj
}


#' Determine constraints and objective from response function and probabilities
#' 
#' A more flexible alternative to \link{analyze_graph} that takes as inputs the 
#' list of response functions and p.vals object directly. 
#' 
#' @param respvars A list of response function specifications. See the `response.functions` element of an object of class `linearcausalproblem` for an example.
#' @param p.vals A data from with the observed variables and their values
#' @param prob.form A list denoting which variables are conditioned upon in `p.vals`. See examples.
#' @param effectt A character string that represents the causal effect of interest
#' @param constraints A vector of character strings that represent the constraints on counterfactual quantities
#' 
#' @details 
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
#' p.vals <- expand.grid(X = 0:1, Y = 0:1)
#' confmod <- create_causalmodel(graph = b, p.vals = p.vals, prob.form =  list(out = c("X", "Y"), cond = NULL))
#' create_linearcausalproblem(confmod, effectt = "p{Y(X = 1) = 1}")

create_linearcausalproblem <- function(causal_model, effectt, constraints = NULL) {
    
    prob.form <- causal_model$prob.form
    
    jd <- do.call(paste0, p.vals[, prob.form$out, drop = FALSE])
    cond <- do.call(paste0, p.vals[, prob.form$cond, drop = FALSE])
    
    parameters <- paste0("p", paste(jd, cond, sep = "_"))
    
    parameters.key <- paste(paste(prob.form$out, collapse = ""), 
                            paste(prob.form$cond, collapse = ""), sep = "_")
    
    ## matrix of unobserved counterfactual probabilities
    
    q.list <- create_q_matrix(respvars, right.vars = prob.form$out, 
                              cond.vars = prob.form$cond, constraints = constraints)
    variables <- as.character(unique(q.list$q.vals.all.lookup$vars))
    ## constraints identify set of qs that correspond to observed p.vals
    
    obsvars <- names(q.list$q.vals.all.lookup)[-ncol(q.list$q.vals.all.lookup)]
    names(obsvars) <- unlist(obsvars)
    
    glist <- lapply(1:length(respvars), \(df) {
        pas <- unlist(lapply(respvars[[df]]$values, \(fn) {
            names(formals(fn))
        })) |> unique()
        do.call(rbind, lapply(pas, \(ss) {
            if(length(ss) == 0) NULL else c(ss, names(respvars)[df])
          }))
    })
    graph <- graph_from_edgelist(do.call(rbind, glist))
    nvals <- lapply(respvars, \(ve) {
        unlist(lapply(ve$values, \(fn) {
            
            bd <- body(fn)
            if(is.numeric(bd)) bd else {
                bd2 <- as.character(bd)
                bd2[-c(1:2)] |> as.numeric()
            }
            
        })) |> unique() |> length()
    })
    
    V(graph)$nvals <- unlist(nvals[names(V(graph))])
    
    
    linconstr.list <- create_R_matrix(graph, obsvars, respvars, 
                                      p.vals, parameters, q.list, variables)
    
    parameters <- linconstr.list$newparams
    p.vals <- linconstr.list$newpvals
    ## determine objective based on exposure and outcome in terms of qs
    
    effect <- parse_effect(effectt)
    
    chk0 <- lapply(effect$vars, causaloptim:::btm_var)
    
    interven.vars <- unique(unlist(chk0))
    
    allnmes <- unique(c(interven.vars, unlist(lapply(effect$vars, names))))
    
    realnms <- names(respvars)
    if(any(!allnmes %in% realnms)) {
        
        stop(sprintf("Names %s in effect not specified in response functions!", 
                     paste(allnmes[which(!allnmes %in% realnms)], collapse = ", ")))
        
    }
    
    ## check linearity
    
    stopifnot(check_linear_objective(respvars, effectt, prob.form))
    
    if("oper" %in% names(chk0) & !chk0["oper"] %in% c("+", "-")) {
        stop(sprintf("Operator '%s' not allowed!", chk0["oper"]))
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
    attr(parameters, "rightvars") <- prob.form$out
    attr(parameters, "condvars") <- prob.form$cond
    
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

