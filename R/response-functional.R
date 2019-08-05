#' Convert an igraph object to the response functional
#' 
#' The graph must contain edge attributes named "leftside" and "lrconnect"
#' that takes values 0 and 1. Only one edge may have a value 1 for lrconnect. 
#' The shiny app returns a graph in this format. 
#' 
#' @export

igraph_to_response_function <- function(graph) {
    
    leftind <- vertex_attr(graph)$leftside
    
    if(sum(edge_attr(graph)$rlconnect) > 0) stop("No edges can go from right to left")
    
    cond.vars <- V(graph)[leftind == 1 & V(graph)$latent == 0]
    right.vars <- V(graph)[leftind == 0 & V(graph)$latent == 0]
    
    var.values <- lapply(names(c(right.vars, cond.vars)), function(i) c(0, 1))
    names(var.values) <- names(c(right.vars, cond.vars))
    
    p.vals <- do.call(expand.grid, var.values)
    
    jd <- do.call(paste0, p.vals[, names(right.vars)])
    cond <- do.call(paste0, p.vals[, names(cond.vars), drop = FALSE])
    
    parameters <- paste0("p", paste(jd, cond, sep = "_"))
    parameters.key <- paste(paste(names(right.vars), collapse = ""), paste(names(cond.vars), collapse = ""), sep = "_")
    
    
    ## response variable for each observed variable
    
    obsvars <- c(right.vars, cond.vars)
    respvars <- vector(mode = "list", length = length(right.vars))
    names(respvars) <- names(right.vars)
    for(ini in 1:length(right.vars)) {
        
        i <- right.vars[ini]
        intoi <- graph[from = obsvars, to = rep(i, length(obsvars))]
        nstart <- prod(c(2 , 2 ^ intoi))
        
        parents <- names(obsvars[as.logical(intoi)])
        arglist <- vector(mode = "list", length = length(parents))
        names(arglist) <- parents
        
        ifs <- expand.grid(lapply(parents, function(x) c(paste0("(", x, ")"), paste0("(1 - ", x, ")"))))
        values <- c("0", do.call(pastestar, ifs), "1")
        
        respvars[[ini]] <- list(index = 0:(nstart - 1), values = values)
        
    }
    
    q.vals <- do.call(expand.grid, lapply(respvars, "[[", 1))
    variables <- paste0("q", do.call(paste0, q.vals))
    
    ## constraints 
    
    p.constraints <- rep(NA, nrow(p.vals))
    for(j in 1:nrow(p.vals)) {
        tmpenvir <- p.vals[j, ]
        
        tmp.match <- vector("list", length = length(right.vars))
        names(tmp.match) <- names(right.vars)
        for(i in 1:length(right.vars)) {
            
            cf.vals <- unlist(lapply(respvars[[i]]$values, function(x) {
                eval(parse(text = x), envir=tmpenvir, enclos = parent.frame())
            }))
            
            tmp.match[[i]] <- respvars[[i]]$index[cf.vals == tmpenvir[, i]]
            
        }
        
        q.match <- paste0("q", do.call(paste0, expand.grid(tmp.match)))
        p.constraints[j] <- paste(parameters[j], "=", paste(q.match, collapse = " + "))
        
    }
    
    p.constraints <- c(p.constraints, paste(paste(variables, collapse= " + "), " = 1"))
    
    ## determine objective based on exposure and outcome in terms of qs 
    
    expo.var <- V(graph)[vertex_attr(graph, "exposure") == 1]
    outcome <- V(graph)[vertex_attr(graph, "outcome") == 1]
    
    response.paths <- all_simple_paths(graph, from = expo.var, to = outcome)
    
    var.eff <- list(NULL, NULL) 
    for(do.x in 1:0){
        path.env1 <- new.env()
        assign(names(expo.var), do.x, path.env1)
        
        cur.env <- list(path.env1)
        track.path <- data.frame(dex= 1)
        i <- 2
        while(TRUE) {
            targ.tmp <- names(response.paths[[1]])[i]
            targ.poss <- lapply(cur.env, function(penv) {
                unlist(lapply(respvars[[targ.tmp]]$values, function(x) {
                    eval(parse(text = x), envir=penv, enclos = parent.frame())
                }))
            })
            
            track.path.in <- data.frame(respvars[[targ.tmp]]$index, unlist(targ.poss))
            colnames(track.path.in) <- c(paste0("R_", targ.tmp), targ.tmp)
            track.path <- cbind(as.data.frame(lapply(track.path, function(col) rep(col,  each = length(cur.env)))), 
                  track.path.in)
            
            i <- i + 1
            if(targ.tmp == names(outcome)) break
            
            cur.env <- lapply(unlist(targ.poss), function(x) {
                env.in <- new.env()
                assign(targ.tmp, x, env.in)
                env.in
            })
        }
        
        rout.dex <- track.path[, names(outcome)] == 1
        tomatch <- track.path[rout.dex, paste0("R_", names(response.paths[[1]])[-1]) , drop = FALSE]
        names(tomatch) <- names(response.paths[[1]])[-1]
        
        var.dex <- merge(cbind(q.vals, .index = 1:nrow(q.vals)), tomatch)$.index
        
        var.eff[[(1 - do.x) + 1]] <- variables[var.dex]
    }
    
    
    objterm1 <- setdiff(var.eff[[1]], var.eff[[2]])
    objterm2 <- setdiff(var.eff[[2]], var.eff[[1]])
    
    objective <- paste(paste(objterm1, collapse = " + "), " - ", paste(objterm2, collapse = " - "))
    
}

pastestar <- function(...) paste(..., sep = "*")

make_function <- function(args, body, env = parent.frame()) {
    
    args <- as.pairlist(args)
    eval(call("function", args, body), env)
    
}

do_var1 <- function(var, envir = ) {
    
    if(all(graph[from = rep(var, length(right.vars)), to = right.vars] == 0)) {
        
        respvars[[paste0("R_", names(var))]]
        
    }
    
}

