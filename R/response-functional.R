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
    
    p.vals <- do.call(expand.grid, lapply(1:length(c(right.vars, cond.vars)), function(i) c(0, 1)))
    colnames(p.vals) <- c(names(right.vars), names(cond.vars))
    jd <- do.call(paste0, p.vals[, names(right.vars)])
    cond <- do.call(paste0, p.vals[, names(cond.vars), drop = FALSE])
    
    parameters <- paste0("p", paste(jd, cond, sep = "_"))
    parameters.key <- paste(paste(names(right.vars), collapse = ""), paste(names(cond.vars), collapse = ""), sep = "_")
    
    
    ## response variable for each observed variable
    
    obsvars <- c(right.vars, cond.vars)
    respvars <- vector(mode = "list", length = length(right.vars))
    names(respvars) <- paste0("R_", names(right.vars))
    for(ini in 1:length(right.vars)) {
        
        i <- right.vars[ini]
        intoi <- graph[from = obsvars, to = rep(i, length(obsvars))]
        nstart <- prod(c(2 , 2 ^ intoi))
        
        parents <- names(obsvars[as.logical(intoi)])
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
    
}

