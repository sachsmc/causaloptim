#' Analyze the causal graph to determine constraints and objective
#' 
#' The graph must contain edge attributes named "leftside" and "lrconnect"
#' that takes values 0 and 1. Only one edge may have a value 1 for lrconnect. 
#' The shiny app returns a graph in this format. 
#' 
#' @export

analyze_graph <- function(graph) {
    
    leftind <- vertex_attr(graph)$leftside
    
    if(sum(edge_attr(graph)$rlconnect) > 0) stop("No edges can go from right to left")
    
    cond.vars <- V(graph)[leftind == 1 & V(graph)$latent == 0]
    right.vars <- V(graph)[leftind == 0 & V(graph)$latent == 0]
    
    var.values <- lapply(names(c(right.vars, cond.vars)), function(i) c(0, 1))
    names(var.values) <- names(c(right.vars, cond.vars))
    
    p.vals <- do.call(expand.grid, var.values)
    
    jd <- do.call(paste0, p.vals[, names(right.vars), drop = FALSE])
    cond <- do.call(paste0, p.vals[, names(cond.vars), drop = FALSE])
    
    parameters <- paste0("p", paste(jd, cond, sep = "_"))
    parameters.key <- paste(paste(names(right.vars), collapse = ""), paste(names(cond.vars), collapse = ""), sep = "_")
    
    
    ## response variable for each observed variable
    
    obsvars <- c(right.vars, cond.vars)
    respvars <- vector(mode = "list", length = length(obsvars))
    names(respvars) <- names(obsvars)
    for(ini in 1:length(obsvars)) {
        
        i <- obsvars[ini]
        intoi <- graph[from = obsvars, to = rep(i, length(obsvars))]
        nstart <- prod(c(2 , 2 ^ intoi))
        
        parents <- names(obsvars[as.logical(intoi)])
        arglist <- vector(mode = "list", length = length(parents))
        names(arglist) <- parents
        
        if(length(parents) == 0) {
            values <- c("0", "1")
        } else {
            
            bigboy <- lapply(1:length(parents), function(x) combn(parents, x, simplify = FALSE))
            
            bigifs <- lapply(bigboy, function(rents) {
                ifs <- expand.grid(lapply(rents, function(x) c(paste0("(", x, ")"), paste0("(1 - ", x, ")"))))
                do.call(pastestar, ifs)
            })
            
            values <- c("0", unlist(bigifs), "1")
        }
        
        respvars[[ini]] <- list(index = 0:(length(values) - 1), values = values)
        
    }
    
    q.vals.all <- do.call(expand.grid, lapply(respvars, "[[", 1))
    q.vals <- do.call(expand.grid, lapply(respvars, "[[", 1)[which(obsvars %in% right.vars)])
    
    variables <- paste0("q", do.call(paste0, q.vals))
    
    q.vals.tmp <- cbind(q.vals, vars = variables)
    q.vals.all.lookup <- merge(q.vals.all, q.vals.tmp, by = names(right.vars), sort = TRUE)
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
    
    ## check for any monotonicity assumptions
    if(any(E(graph)$edge.monotone == 1)) {
        which.monotone <- which(E(graph)$edge.monotone == 1)
        for(j in which.monotone) {
            
            head.mono <- names(head_of(graph, j))
            tail.mono <- names(tail_of(graph, j))
            
            settozeroindex <- respvars[[head.mono]]$index[grepl(paste0("(1 - ", tail.mono, ")"), respvars[[head.mono]]$values)]
            
            p.constraints <- c(p.constraints, 
                               paste0(variables[q.vals[, head.mono] %in% settozeroindex],  " = 0"))
            
        }
    }
    
    baseind <- rep(FALSE, length(p.constraints))
    baseind[1:nrow(p.vals)] <- TRUE
    attr(p.constraints, "baseconstr") <- baseind
    ## determine objective based on exposure and outcome in terms of qs 
    expo.var <- V(graph)[vertex_attr(graph, "exposure") == 1]
    outcome <- V(graph)[vertex_attr(graph, "outcome") == 1]
    var.eff <- list(NULL, NULL)
    for(do.x in 0:1) {
        intervene <- list(do.x)
        names(intervene) <- names(expo.var)
        gee_r <- function(r, i) {
            
            parents <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
            parents <- parents[vertex_attr(graph, name="latent", index = parents) == 0 ]
            
            if(names(obsvars)[i] %in% names(intervene)) {
                as.numeric(intervene[[names(right.vars[i])]])
            } else if (length(parents) == 0){
                as.numeric(respvars[[names(obsvars[[i]])]]$values[which(respvars[[names(obsvars[[i]])]]$index == r[i])])
            } else {
                
                lookin <- lapply(names(parents), function(gu) {
                    
                    as.numeric(gee_r(r, which(names(obsvars) == gu)))
                    
                })
                names(lookin) <- names(parents)
                inres <- respvars[[names(obsvars[[i]])]]$values[which(respvars[[names(obsvars[[i]])]]$index == r[i])]
                with(lookin, eval(parse(text = inres)))
                
            }
        }
        
        
        res.mat <- matrix(NA, ncol = ncol(q.vals.all), nrow = nrow(q.vals.all))
        for(k in 1:nrow(q.vals.all)) {
            for(j in 1:ncol(q.vals.all)) {
                res.mat[k, j] <- gee_r(r = unlist(q.vals.all.lookup[k, -ncol(q.vals.all.lookup)]), i = j)
                
            }
        }
        colnames(res.mat) <- names(obsvars)
        var.dex <- res.mat[, names(outcome)] == 1
        var.eff[[(1 - do.x) + 1]] <- as.character(q.vals.all.lookup[var.dex, "vars"])
        
    }
    
    objterm1 <- setdiff(var.eff[[1]], var.eff[[2]])
    objterm2 <- setdiff(var.eff[[2]], var.eff[[1]])
    
    objective <- paste(paste(objterm1, collapse = " + "), " - ", paste(objterm2, collapse = " - "))
    
    attr(parameters, "key") <- parameters.key
    
    list(variables = variables, parameters = parameters, constraints = p.constraints, 
         objective = objective, p.vals = p.vals, q.vals = q.vals)
    
    
}

pastestar <- function(...) paste(..., sep = "*")


