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
    
    var.eff <- create_effect_vector(effect, obj$graph, obsvars, obj$response.functions,
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