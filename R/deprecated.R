
#' Simulate bounds
#' 
#' Run a simple simulation based on the bounds. For each simulation, sample the set of counterfactual probabilities 
#' from a uniform distribution, translate into a multinomial distribution, and then compute the objective and the 
#' bounds in terms of the observable variables. 
#' 
#' @param obj Object as returned by \link{analyze_graph}
#' @param bounds Object as returned by \link{optimize_effect_2}
#' @param nsim Number of simulation replicates
#' 
#' @return A data frame with columns: objective, bound.lower, bound.upper
#' 
#' @export
#' @examples
#' b <- initialize_graph(graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y))
#' obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#' bounds <- optimize_effect_2(obj)
#' simulate_bounds(obj, bounds, nsim = 5)

simulate_bounds <- function(obj, bounds, nsim = 1e3) {
    
    
    f.bounds <- interpret_bounds(bounds$bounds, obj$parameters)
    result <- matrix(NA, ncol = 3, nrow = nsim)
    
    for(i in 1:nsim) {
        
        sim.qs <- runif(length(obj$variables))
        sim.qs <- sim.qs / sum(sim.qs)
        
        names(sim.qs) <- obj$variables
        objective <- eval(parse(text = obj$objective), envir = as.list(sim.qs))
        
        inenv <- new.env()
        for(j in 1:length(sim.qs)) {
            
            assign(names(sim.qs)[j], sim.qs[j], inenv)
            
        }
        res <- lapply(as.list(obj$constraints[-1]), function(x){
            x1 <- strsplit(x, " = ")[[1]]
            x0 <- paste(x1[1], " = ", x1[2])
            eval(parse(text = x0), envir = inenv)
        })
        
        params <- lapply(obj$parameters, function(x) get(x, envir = inenv))
        names(params) <- obj$parameters
        
        bees <- c(unlist(do.call(f.bounds, params)))
        result[i, ] <- c(objective, bees)
        
        if(objective < bees[1] | objective > bees[2]) {
            stop("Objective outside bounds, optimizer failed!")
            break
        }
        
    }
    colnames(result) <- c("objective", "bound.lower", "bound.upper")
    as.data.frame(result)
    
    
}




#' Create constraint matrix
#' 
#' Matrix and text representation of constraints on observed probabilities
#' 
#' @param graph The graph
#' @param obsvars Vector of observed variable vertices from the graph 
#' @param respvars Response function, as returned by \link{create_response_function}
#' @param p.vals Observed probability matrix
#' @param parameters Vector of ps names
#' @param q.list List with q matrices, as returned by \link{create_q_matrix}
#' @param variables Vector of qs names
#' 
#' @noRd
#' @return A list with the R matrix and the string representation

create_R_matrix <- function(graph, obsvars, respvars, p.vals, parameters, q.list, variables) {
    
    q.vals <- q.list$q.vals
    q.vals.all <- q.list$q.vals.all
    q.vals.all.lookup <- q.list$q.vals.all.lookup
    
    parent_lookup <- lapply(respvars, \(var) {
        
        unlist(lapply(var$values, \(fun) {
            names(formals(fun))
        })) |> unique()
        
    })
    
    obsvars <- as.list(names(q.vals.all.lookup)[-c(ncol(q.vals.all.lookup))])
    names(obsvars) <- unlist(obsvars)
    
    gee_r <- function(r, i) {
        parents <- parent_lookup[[obsvars[[i]]]]
        if (length(parents) == 0) {
            x <- respvars[[obsvars[[i]]]]$values[[which(respvars[[obsvars[[i]]]]$index == 
                                                            r[i])]]
            do.call(x, list())
        }
        else {
            lookin <- lapply(parents, function(gu) {
                as.numeric(gee_r(r, which(obsvars == gu)))
            })
            names(lookin) <- parents
            inres <- respvars[[obsvars[[i]]]]$values[[which(respvars[[obsvars[[i]]]]$index == 
                                                                r[i])]]
            do.call(inres, lookin)
        }
    }
    
    
    
    res.mat <- matrix(NA, ncol = ncol(q.vals.all), nrow = nrow(q.vals.all))
    q.vals.mat <- as.matrix(q.vals.all.lookup[, -ncol(q.vals.all.lookup)])
    for(k in 1:nrow(q.vals.all)) {
        for(j in 1:ncol(q.vals.all)) {
            res.mat[k, j] <- gee_r(r = q.vals.mat[k, ], i = j)
            
        }
    }
    colnames(res.mat) <- names(obsvars)
    
    R <- matrix(0, nrow = nrow(p.vals) + 1, ncol = nrow(q.vals))
    R[1, ] <- 1
    removeprows <- rep(0, nrow(p.vals))
    p.constraints <- rep(NA, nrow(p.vals) + 1)
    p.constraints[1] <- paste(paste(variables, collapse= " + "), " = 1")
    pvalmat <- as.matrix(p.vals)
    pnames <- colnames(p.vals)
    for(pj in 1:nrow(p.vals)) {
        
        #p.chk <- do.call("rbind", lapply(1:nrow(res.mat), function(i) p.vals[pj, , drop = FALSE]))
        p.chk <- matrix(pvalmat[pj,], nrow = nrow(res.mat), ncol = ncol(pvalmat), byrow = TRUE)
        inp <- apply(res.mat[, pnames, drop = FALSE] == p.chk, 1, all)
        
        if(!any(inp)) {
            removeprows[pj] <- 1
            next
            
        } else {
            q.match <- q.vals.all.lookup[inp, ncol(q.vals.all.lookup)]
            
            R[pj + 1, match(unique(q.match), variables)] <- 1
            if(length(q.match) == 0) q.match <- "0"
            p.constraints[pj + 1] <- paste(parameters[pj], "=", paste(unique(q.match), collapse = " + "))
            
        }
    }
    
    p.vals <- p.vals[removeprows == 0, , drop = FALSE]
    parameters <- parameters[removeprows == 0]
    p.constraints <- p.constraints[!is.na(p.constraints)]
    R <- R[c(TRUE, removeprows == 0), , drop = FALSE]
    
    baseind <- rep(FALSE, length(p.constraints))
    baseind[1:nrow(p.vals)] <- TRUE
    attr(p.constraints, "baseconstr") <- baseind
    
    list(p.constraints = p.constraints, R = R, newparams = parameters, newpvals = p.vals)
    
    
}


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
    
    var.eff <- create_effect_vector(obj$causal_model, effect)
    
    
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



