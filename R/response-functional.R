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
    
    jd <- do.call(paste0, p.vals[, names(right.vars), drop = FALSE])
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
    
    
    ## determine objective based on exposure and outcome in terms of qs 
    expo.var <- V(graph)[vertex_attr(graph, "exposure") == 1]
    outcome <- V(graph)[vertex_attr(graph, "outcome") == 1]
    var.eff <- list(NULL, NULL)
    for(do.x in 0:1) {
        intervene <- list(do.x)
        names(intervene) <- names(expo.var)
        gee_r <- function(r, i) {
            
            parents <- adjacent_vertices(graph, right.vars[i], "in")[[1]]
            parents <- parents[vertex_attr(graph, name="latent", index = parents) == 0 & 
                                   vertex_attr(graph, name="leftside", index = parents) == 0]
            
            if(names(right.vars)[i] %in% names(intervene)) {
                as.numeric(intervene[[names(right.vars[i])]])
            } else if (length(parents) == 0){
                as.numeric(respvars[[names(right.vars[[i]])]]$values[which(respvars[[names(right.vars[[i]])]]$index == r[i])])
            } else {
                
                lookin <- lapply(names(parents), function(gu) {
                    
                    as.numeric(gee_r(r, which(names(right.vars) == gu)))
                    
                })
                names(lookin) <- names(parents)
                inres <- respvars[[names(right.vars[[i]])]]$values[which(respvars[[names(right.vars[[i]])]]$index == r[i])]
                with(lookin, eval(parse(text = inres)))
                
            }
        }
        
        
        res.mat <- matrix(NA, ncol = ncol(q.vals), nrow = nrow(q.vals))
        for(k in 1:nrow(q.vals)) {
            for(j in 1:ncol(q.vals)) {
                res.mat[k, j] <- gee_r(r = unlist(q.vals[k, ]), i = j)
                
            }
        }
        colnames(res.mat) <- names(right.vars)
        var.dex <- res.mat[, names(outcome)] == 1
        var.eff[[(1 - do.x) + 1]] <- variables[var.dex]
        
    }
    
    objterm1 <- setdiff(var.eff[[1]], var.eff[[2]])
    objterm2 <- setdiff(var.eff[[2]], var.eff[[1]])
    
    objective <- paste(paste(objterm1, collapse = " + "), " - ", paste(objterm2, collapse = " - "))
    
    list(variables = variables, parameters = parameters, constraints = p.constraints, 
         objective = objective)
    
    
}

pastestar <- function(...) paste(..., sep = "*")



balke_optimize <- function(obj) {
    
    tbl.file <- tempfile(pattern = c("max", "min"))
    cat("VARIABLES\n", file = tbl.file[1])
    cat(obj$variables, file = tbl.file[1], append = TRUE, sep = "\n")
    cat("\nPARAMETERS\n", file = tbl.file[1], append = TRUE)
    cat(obj$parameters, file = tbl.file[1], append = TRUE, sep = "\n")
    cat("\nCONSTRAINTS\n", file = tbl.file[1], append = TRUE)
    cat(obj$constraints, file= tbl.file[1], append = TRUE, sep = "\n")
    cat("\nMAXIMIZE\n", file = tbl.file[1], append = TRUE)
    cat("\nOBJECTIVE\n", file =tbl.file[1], append = TRUE)
    cat(obj$objective, file = tbl.file[1], append = TRUE)
    cat("\nEND\n", file = tbl.file[1], append = TRUE)
    
    
    test <- COptimization_$new()
    test$ParseFileWrap(tbl.file[1])
    test$CategorizeConstraints()
    test$GaussianElimination()
    test$EnumerateVertices()
    test$OutputOptimum()
    test$Display()
    
    cat("VARIABLES\n", file = tbl.file[2])
    cat(obj$variables, file = tbl.file[2], append = TRUE, sep = "\n")
    cat("\nPARAMETERS\n", file = tbl.file[2], append = TRUE)
    cat(obj$parameters, file = tbl.file[2], append = TRUE, sep = "\n")
    cat("\nCONSTRAINTS\n", file = tbl.file[2], append = TRUE)
    cat(obj$constraints, file= tbl.file[2], append = TRUE, sep = "\n")
    cat("\nMINIMIZE\n", file = tbl.file[2], append = TRUE)
    cat("\nOBJECTIVE\n", file =tbl.file[2], append = TRUE)
    cat(obj$objective, file = tbl.file[2], append = TRUE)
    cat("\nEND\n", file = tbl.file[2], append = TRUE)
    
    
    test2 <- COptimization_$new()
    test2$ParseFileWrap(tbl.file[2])
    test2$CategorizeConstraints()
    test2$GaussianElimination()
    test2$EnumerateVertices()
    test2$OutputOptimum()
    test2$Display()
    
    
}


