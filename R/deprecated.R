
#' Simulate bounds
#' 
#' Run a simple simulation based on the bounds. For each simulation, sample the set of counterfactual probabilities 
#' from a uniform distribution, translate into a multinomial distribution, and then compute the objective and the 
#' bounds in terms of the observable variables. 
#' 
#' @param obj Object as returned by \link{analyze_graph}
#' @param bounds Object as returned by \link{optimize_effect}
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
#' @export
#' @return A list with the R matrix and the string representation
#' @examples
#' graph <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y)
#' V(graph)$leftside <- c(1, 0, 0, 1, 0)
#' V(graph)$latent <- c(0, 0, 0, 1, 1)
#' V(graph)$nvals <- c(3, 2, 2, 2, 2)
#' V(graph)$exposure <- c(0, 1, 0, 0, 0)
#' V(graph)$outcome <- c(0, 0, 1, 0, 0)
#' E(graph)$rlconnect <- c(0, 0, 0, 0, 0)
#' E(graph)$edge.monotone <- c(0, 0, 0, 0, 0)
#' constraints <- "X(Z = 1) >= X(Z = 0)"
#' effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
#' leftind <- vertex_attr(graph)$leftside
#' cond.vars <- V(graph)[leftind == 1 & names(V(graph)) != "Ul"]
#' right.vars <- V(graph)[leftind == 0 & names(V(graph)) != "Ur"] 
#' obsvars <- c(right.vars, cond.vars)
#' observed.variables <- V(graph)[V(graph)$latent == 0]
#' var.values <- lapply(names(observed.variables), 
#' function(varname) seq(from = 0, to = causaloptim:::numberOfValues(graph, varname) - 1))
#' names(var.values) <- names(observed.variables)
#' p.vals <- do.call(expand.grid, var.values)
#' jd <- do.call(paste0, p.vals[, names(right.vars[right.vars$latent == 0]), drop = FALSE])
#' cond <- do.call(paste0, p.vals[, names(cond.vars[cond.vars$latent == 0]), drop = FALSE])
#' parameters <- paste0("p", paste(jd, cond, sep = "_"))
#' parameters.key <- paste(paste(names(right.vars[right.vars$latent == 0]), collapse = ""), 
#' paste(names(cond.vars[cond.vars$latent == 0]), collapse = ""), sep = "_")
#' respvars <- create_response_function(graph, right.vars, cond.vars)
#' q.list <- create_q_matrix(respvars, right.vars, cond.vars, constraints)
#' variables <- as.character(unique(q.list$q.vals.all.lookup$vars))
#' linconstr.list <- create_R_matrix(graph, obsvars, respvars, p.vals, parameters, q.list, variables)

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
