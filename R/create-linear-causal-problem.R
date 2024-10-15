
#' Create linear causal problem from causal model and effect
#' 
#' A more flexible alternative to \link{analyze_graph} that takes as inputs the 
#' causal model and effect. 
#' 
#' @param causal_model An object of class "causalmodel" as produce by \link{create_causalmodel}
#' @param effectt A character string that represents the causal effect of interest
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
#' confmod <- create_causalmodel(graph = b, prob.form =  list(out = c("X", "Y"), cond = NULL))
#' create_linearcausalproblem(confmod, effectt = "p{Y(X = 1) = 1}")

create_linearcausalproblem <- function(causal_model, effectt) {
    
    linobj.if.true <- check_linear_objective(causal_model, effectt = effectt)
    if(!linobj.if.true) {
        stop("Specified effect does not imply a linear objective function!")
    }
    
    if(!causal_model$counterfactual_constraints$linear.if.true) {
        stop("Specified causal model does not imply linear constraints!")
    }
    
    prob.form <- causal_model$data$prob.form
    
    q.list <- NULL
    q.list$q.vals.all.lookup = causal_model$data$q.vals[, -ncol(causal_model$data$q.vals)]
    q.list$q.vals.all = q.list$q.vals.all.lookup[, -ncol(q.list$q.vals.all.lookup)]
    q.list$q.vals = unique(q.list$q.vals.all[, unlist(causal_model$data$prob.form), drop = FALSE])
    
    variables <- causal_model$data$variables
    ## constraints identify set of qs that correspond to observed p.vals
    
    #linconstr.list <- create_R_matrix(graph, obsvars, respvars, 
    #                                 p.vals, parameters, q.list, variables)
    
    linconstr.list = list(p.constraints = causal_model$counterfactual_constraints$character, 
                          R = causal_model$counterfactual_constraints$numeric$R)
    
    
    effect <- parse_effect(effectt)
    
    chk0 <- lapply(effect$vars, btm_var)
    
    interven.vars <- unique(unlist(chk0))
    
    allnmes <- unique(c(interven.vars, unlist(lapply(effect$vars, names))))
    
    graph <- causal_model$data$graph
    if(!is.null(graph)) {
    realnms <- names(V(graph))
    if(any(!allnmes %in% realnms)) {
        
        stop(sprintf("Names %s in effect not specified in graph!", 
                     paste(allnmes[which(!allnmes %in% realnms)], collapse = ", ")))
        
    }
    
    ## check that children of intervention sets are on the right
    
    any.children.onleft <- sapply(interven.vars, function(v) {
        
        children <- neighbors(graph, V(graph)[v], mode = "out")
        any(children$leftside == 1)
        
    })
    
    if(any(any.children.onleft) == TRUE) {
        stop(sprintf("Cannot intervene on %s because it has children on the leftside!", 
                     paste(interven.vars[which(any.children.onleft)], collapse = ", ")))
    }
    }
    
    if("oper" %in% names(chk0) & !chk0["oper"] %in% c("+", "-")) {
        stop(sprintf("Operator '%s' not allowed!", chk0["oper"]))
    }
    
    if(!is.null(graph)) {
        cond.vars <- prob.form$cond
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
        
    }}
    
    
    ## handle addition and subtraction based on operator
    ## accumulate final effect based on subtraction and addition
    var.eff <- create_effect_vector(causal_model, effect)
    
    
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
    
    parameters <- causal_model$data$parameters
    attr(parameters, "rightvars") <- prob.form$out
    attr(parameters, "condvars") <- prob.form$cond
    
    res <- list(variables = variables, parameters = parameters, 
                constraints = linconstr.list$p.constraints, 
                objective = objective.fin, p.vals = causal_model$data$p.vals, q.vals = q.list$q.vals, 
                parsed.query = effect, unparsed.query = effectt, 
                user.constraints = causal_model$data$user_constraints, 
                objective.nonreduced = objective, 
                response.functions = causal_model$data$response_functions, 
                graph = graph, R = linconstr.list$R, c0 = c0, 
                causal_model = causal_model)
    class(res) <- "linearcausalproblem"
    res
    
}

