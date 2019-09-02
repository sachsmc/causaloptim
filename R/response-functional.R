#' Analyze the causal graph to determine constraints and objective
#' 
#' The graph must contain edge attributes named "leftside" and "lrconnect"
#' that takes values 0 and 1. Only one edge may have a value 1 for lrconnect. 
#' The shiny app returns a graph in this format. 
#' 
#' @export

analyze_graph <- function(graph, constraints) {
    
    leftind <- vertex_attr(graph)$leftside
    
    if(sum(edge_attr(graph)$rlconnect) > 0) stop("No edges can go from right to left")
    
    cond.vars <- V(graph)[leftind == 1 & V(graph)$latent == 0]
    right.vars <- V(graph)[leftind == 0 & V(graph)$latent == 0] ## allow outcome to be latent? but how
    
    var.values <- lapply(names(c(right.vars, cond.vars)), function(i) c(0, 1))
    names(var.values) <- names(c(right.vars, cond.vars))
    
    p.vals <- do.call(expand.grid, var.values)  # p vals need to be based on observed variables only
    
    jd <- do.call(paste0, p.vals[, names(right.vars), drop = FALSE])
    cond <- do.call(paste0, p.vals[, names(cond.vars), drop = FALSE])
    
    parameters <- paste0("p", paste(jd, cond, sep = "_"))
    parameters.key <- paste(paste(names(right.vars), collapse = ""), paste(names(cond.vars), collapse = ""), sep = "_")
    
    
    ## response variable for each variable observed or unobserved
    
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
            values <- list(function(){ 0 }, function() { 1 })
        } else {
            
            
            ## da matrix
            
            poss.ins <- expand.grid(lapply(parents, function(x) c(0, 1)))
            colnames(poss.ins) <- parents
            ini.outs <- expand.grid(lapply(1:nrow(poss.ins), function(x) c(0, 1)))
            
            args <- vector(mode = "list", length = ncol(poss.ins))
            names(args) <- parents
            
            values <- vector(mode = "list", length = nrow(ini.outs))
            for(j in 1:nrow(ini.outs)) {
                f.tmp <- function() {}
                formals(f.tmp) <- as.pairlist(args)
                
                ## build body
                a1 <- paste("paste0(", paste(parents, collapse = ", "), ")")
                switchnames <- paste0("`", do.call(paste0, poss.ins), "`")
                switchbod <- as.list(c(ini.outs[j, ]))
                fbod <- paste0("switch(", a1, ", ", paste(paste(switchnames, switchbod, sep = " = "), collapse = ", "), ")")
                body(f.tmp) <- parse(text = fbod)
                environment(f.tmp) <- parent.frame()
                
                values[[j]] <- f.tmp
                
            }
            
        }
        
        respvars[[ini]] <- list(index = 0:(length(values) - 1), values = values)
        
    }
    
    
    ## check for any monotonicity assumptions
    if(any(E(graph)$edge.monotone == 1)) {
      which.monotone <- which(E(graph)$edge.monotone == 1)
      for(j in which.monotone) {
        
        head.mono <- names(head_of(graph, j))
        tail.mono <- names(tail_of(graph, j))
        
        tmpenv.1 <- list(1)
        tmpenv.0 <- list(0)
        names(tmpenv.1) <- names(tmpenv.0) <- tail.mono
        
        resp.out.0 <- unlist(lapply(respvars[[head.mono]]$values, function(f) do.call(f, tmpenv.0)))
        resp.out.1 <- unlist(lapply(respvars[[head.mono]]$values, function(f) do.call(f, tmpenv.1)))
        
        settozeroindex <- respvars[[head.mono]]$index[resp.out.0 > resp.out.1]
        removedex <- respvars[[head.mono]]$index == settozeroindex
      
        respvars[[head.mono]]$index <- respvars[[head.mono]]$index[!removedex] 
        respvars[[head.mono]]$values <- respvars[[head.mono]]$values[!removedex] 
          
      }
    }
    
    ## additional constraints
    
    if(!is.null(constraints)) {
      
      for(j in 1:length(constraints)) {
        
        p0 <- strsplit(constraints[[j]], "\\) ")[[1]]
        pl1 <- strsplit(p0[1], "\\(")[[1]]
        
        leftout <- pl1[1]
        leftcond <- strsplit(pl1[-1], ", ")[[1]]
        
        operator <- substr(p0[-1], 1, 1)
        if(operator == "\u2264") operator <- "<="
        if(operator == "\u2265") operator <- ">="
        
        pr1 <- strsplit(substr(p0[-1], 3, nchar(p0[-1])), "\\(")[[1]]
        rightout <- pr1[1]
        rightcond <- strsplit(gsub("\\)", "", pr1[-1]), ", ")[[1]]
        
        stopifnot(leftout == rightout)
        ## end parse, now apply
        
        tmpenv.left <- tmpenv.right <- list()
        tmpenv.left <- within(tmpenv.left, eval(parse(text = leftcond)))
        tmpenv.right <- within(tmpenv.right, eval(parse(text = rightcond)))
        
        resp.out.left <- unlist(lapply(respvars[[leftout]]$values, function(f) do.call(f, tmpenv.left)))
        resp.out.right <- unlist(lapply(respvars[[rightout]]$values, function(f) do.call(f, tmpenv.right)))
        
        stopifnot(length(resp.out.left) == length(resp.out.right))
        
        settozeroindex <- respvars[[head.mono]]$index[!do.call(operator, list(resp.out.left, resp.out.right))]
        
        if(length(settozeroindex) > 0) {
          removedex <- respvars[[leftout]]$index == settozeroindex
        
          respvars[[leftout]]$index <- respvars[[leftout]]$index[!removedex] 
          respvars[[leftout]]$values <- respvars[[leftout]]$values[!removedex] 
          
        }
        
      }
      
      
    }
    
    
    q.vals.all <- do.call(expand.grid, lapply(respvars, "[[", 1))
    q.vals <- do.call(expand.grid, lapply(respvars, "[[", 1)[which(obsvars %in% right.vars)])
    
    variables <- paste0("q", do.call(paste0, q.vals))
    
    q.vals.tmp <- cbind(q.vals, vars = variables)
    q.vals.all.lookup <- merge(q.vals.all, q.vals.tmp, by = names(right.vars), sort = TRUE)
    ## constraints 
    
    removeprows <- rep(0, nrow(p.vals))
    p.constraints <- rep(NA, nrow(p.vals) + 1)
    p.constraints[1] <- paste(paste(variables, collapse= " + "), " = 1")
    for(j in 1:nrow(p.vals)) {
        tmpenvir <- p.vals[j, ]
        
        tmp.match <- vector("list", length = length(right.vars))
        names(tmp.match) <- names(right.vars)
        for(i in 1:length(right.vars)) {
            
            cf.vals <- unlist(lapply(respvars[[i]]$values, function(x) {
                do.call(x, tmpenvir[names(tmpenvir) %in% names(formals(x))])
            }))
            
            tmp.match[[i]] <- respvars[[i]]$index[cf.vals == tmpenvir[, i]]
            
        }
        
        obsdex <- expand.grid(tmp.match)
        
        if(nrow(obsdex) == 0) {
          
          removeprows[j] <- 1
          next
          
        } else {
          
          q.match <- paste0("q", do.call(paste0, obsdex))
          p.constraints[j + 1] <- paste(parameters[j], "=", paste(q.match, collapse = " + "))
          
        }
    }
    
    p.vals <- p.vals[removeprows == 0,]
    parameters <- parameters[removeprows == 0]
    p.constraints <- p.constraints[!is.na(p.constraints)]
    
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
                as.numeric(intervene[[names(obsvars[i])]])
            } else if (length(parents) == 0){
                x <- respvars[[names(obsvars[[i]])]]$values[[which(respvars[[names(obsvars[[i]])]]$index == r[i])]]
                do.call(x, list())
            } else {
                
                lookin <- lapply(names(parents), function(gu) {
                    
                    as.numeric(gee_r(r, which(names(obsvars) == gu)))
                    
                })
                names(lookin) <- names(parents)
                inres <- respvars[[names(obsvars[[i]])]]$values[[which(respvars[[names(obsvars[[i]])]]$index == r[i])]]
                do.call(inres, lookin)
                
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
    
    ## reduce terms
    
    red.sets <- const.to.sets(p.constraints, objterm1, objterm2)
    
    
    objective <- paste(paste(red.sets$objective.terms[[1]], collapse = " + "), " - ", 
                       paste(red.sets$objective.terms[[2]], collapse = " - "))
    
    attr(parameters, "key") <- parameters.key
    attr(parameters, "rightvars") <- names(right.vars)
    attr(parameters, "condvars") <- names(cond.vars)
    
    list(variables = red.sets$variables, parameters = parameters, constraints = red.sets$constr, 
         objective = objective, p.vals = p.vals, q.vals = q.vals)
    
    
}

pastestar <- function(...) paste(..., sep = "*")


const.to.sets <- function(constr, objterm1, objterm2) {
    
    sets <- lapply(strsplit(constr, " = "), function(x) {
        
      tmp1 <- grep("q", x, value = TRUE)  
      trimws(unlist(strsplit(tmp1, "( \\+ | - )")))
      
    })
    
    pnames <- lapply(strsplit(constr, " = "), function(x) {
        
        tmp1 <- grep("q", x, value = TRUE, invert = TRUE) 
        trimws(tmp1)
        
    })
    
    K <- length(sets)
    sets[[length(sets) + 1]] <- objterm1
    sets[[length(sets) + 1]] <- objterm2
    
    reduced.sets <- reduce.sets(sets)
    
    constr.new <- reduced.sets[1:K]
    obj.new <- reduced.sets[(K+1):(K+2)]
    var.new <- reduced.sets[[1]]
    
    list(constr = paste(unlist(lapply(constr.new, paste, collapse = " + ")), " = ", pnames), 
         objective.terms = obj.new, 
         variables = var.new, 
         raw.sets = constr.new, 
         pnames = pnames)
    
    
}




reduce.sets <- function(sets){
    #find commonalities
    K <- length(sets)
    fullset <- sets[[1]]
    common <- list()
    left <- fullset
    r <- 1
    while(length(left)>1){
        tmp <- left[1]
        for(i in 2:length(left)){
            together <- vector(length=K)
            for(k in 1:K){
                if((left[1]%in%sets[[k]] & left[i]%in%sets[[k]]) |
                   (!left[1]%in%sets[[k]] & !left[i]%in%sets[[k]]))
                    together[k] <- TRUE  
            }
            if(sum(together)==K)
                tmp <- c(tmp, left[i])     
        }
        left <- setdiff(left, tmp)
        if(length(tmp)>1){
            common[[r]] <- tmp
            r <- r+1
        }
    }
    
    if(length(common) == 0) return(sets)
    #make reduction
    R <- length(common)
    for(r in 1:R){
        tmp <- common[[r]][-1]
        for(k in 1:K){
            ind <- match(tmp, sets[[k]])
            if(sum(is.na(ind))==0)
                sets[[k]] <- sets[[k]][-ind]
        }  
    }
    #simplify names
    fullset <- sets[[1]]
    for(i in 1:length(sets)){
        tmp <- sets[[i]]
        for(j in 1:length(tmp)){
            tmp[j] <- paste0("q", match(tmp[j], fullset))
        }
        sets[[i]] <- tmp
    }
    return(sets)  
}

#' Plot the analyzed graph object
#' 
#' 
#' @export

plot.graphres <- function(graphres) {
  
  mylayout <- cbind(V(graphres)$x, V(graphres)$y)
  plot(graphres, vertex.color = ifelse(V(graphres)$latent == 1, "grey70",
                                       ifelse(V(graphres)$exposure == 1, "green", "white")), 
       vertex.shape = ifelse(V(graphres)$outcome == 1, "rectangle", "circle"),
       edge.color = ifelse(E(graphres)$edge.monotone == 1, "blue", "black"), 
       layout = mylayout, main = "Graph to be analyzed, inspect carefully")
  legend("topleft", legend = c("latent", "outcome", "exposure", "monotone edge"), pt.cex = c(3, 3, 3, 1), 
         pch = c(20, 22, 20, NA), col = c("grey70", "black", "green", "blue"), lty = c(NA, NA, NA, 1))
  
}