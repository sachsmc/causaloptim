#' Translate regular DAG to response functions
#'
#' @param graph An \link[igraph]{aaa-igraph-package} object that represents a
#'   directed acyclic graph that contains certain edge attributes. 
#'    The shiny app returns a graph in this format and see examples.
#' @param right.vars Vertices of graph on the right side
#' @param cond.vars Vertices of graph on the left side
#' 
#' @return A list of functions representing the response functions
#' @export
#' @examples 
#' ### confounded exposure and outcome

#' b <- igraph::graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(2,2,2)
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
#' cond.vars <- V(b)[V(b)$leftside == 1 & names(V(b)) != "Ul"]
#' right.vars <- V(b)[V(b)$leftside == 0 & names(V(b)) != "Ur"] 
#' create_response_function(b, right.vars, cond.vars)

create_response_function <- function(graph, right.vars, cond.vars) {
    
    obsvars <- c(right.vars, cond.vars) 
    respvars <- vector(mode = "list", length = length(obsvars))
    names(respvars) <- names(obsvars)
    for (ini in 1:length(obsvars)) {
        i <- obsvars[ini]
        intoi <- graph[from = obsvars, to = rep(i, length(obsvars))]
        nstart <- prod(c(2 , 2 ^ intoi))
        
        parents <- names(obsvars[as.logical(intoi)])
        arglist <- vector(mode = "list", length = length(parents))
        names(arglist) <- parents
        
        if (length(parents) == 0) {
            values <- lapply(1:numberOfValues(graph, names(i)), function(j) {
                bin <- function() {
                }
                body(bin) <- eval(parse(text = paste0(j, "-1")))
                environment(bin) <- baseenv()
                bin
            })
        } else {
            ## da matrix
            
            poss.ins <-
                expand.grid(lapply(parents, function(varname)
                    seq(from = 0, to = numberOfValues(graph = graph, varname = varname) - 1)))
            colnames(poss.ins) <- parents
            ini.outs <-
                expand.grid(lapply(1:nrow(poss.ins), function(x)
                    seq(from = 0, to = numberOfValues(graph = graph, varname = names(i)) - 1)))
            
            args <- vector(mode = "list", length = ncol(poss.ins))
            names(args) <- parents
            
            values <- vector(mode = "list", length = nrow(ini.outs))
            for (j in 1:nrow(ini.outs)) {
                f.tmp <- function() {
                }
                formals(f.tmp) <- as.pairlist(args)
                
                ## build body
                a1 <-
                    paste("paste0(", paste(parents, collapse = ", "), ")")
                switchnames <-
                    paste0("`", do.call(paste0, poss.ins), "`")
                switchbod <- as.list(c(ini.outs[j,]))
                fbod <-
                    paste0("switch(",
                           a1,
                           ", ",
                           paste(
                               paste(switchnames, switchbod, sep = " = "),
                               collapse = ", "
                           ),
                           ")")
                body(f.tmp) <- parse(text = fbod)
                environment(f.tmp) <- baseenv()
                
                values[[j]] <- f.tmp
                
            }
            
        }
        
        respvars[[ini]] <-
            list(index = 0:(length(values) - 1), values = values)
        
    }
    
    
    ## check for any monotonicity assumptions
    if (any(E(graph)$edge.monotone == 1)) {
        which.monotone <- which(E(graph)$edge.monotone == 1)
        for (j in which.monotone) {
            head.mono <- names(head_of(graph, j))
            tail.mono <- names(tail_of(graph, j))
            
            tmpenv.1 <- list(1)
            tmpenv.0 <- list(0)
            names(tmpenv.1) <- names(tmpenv.0) <- tail.mono
            
            resp.out.0 <-
                unlist(lapply(respvars[[head.mono]]$values, function(f)
                    do.call(f, tmpenv.0)))
            resp.out.1 <-
                unlist(lapply(respvars[[head.mono]]$values, function(f)
                    do.call(f, tmpenv.1)))
            
            settozeroindex <-
                respvars[[head.mono]]$index[resp.out.0 > resp.out.1]
            removedex <- respvars[[head.mono]]$index == settozeroindex
            
            respvars[[head.mono]]$index <-
                respvars[[head.mono]]$index[!removedex]
            respvars[[head.mono]]$values <-
                respvars[[head.mono]]$values[!removedex]
            
        }
    }
    
    respvars
    
}


#' Translate response functions into matrix of counterfactuals
#'
#' @param respvars A list of functions as returned by \link{create_response_function}
#' @param right.vars Vertices of graph on the right side
#' @param cond.vars Vertices of graph on the left side
#' @param constraints A vector of character strings that represent the constraints
#' 
#' @return A list of 3 data frames of counterfactuals and their associated labels
#' @export
#' @examples
#' graphres <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y)
#' V(graphres)$leftside <- c(1, 0, 0, 1, 0)
#' V(graphres)$latent <- c(0, 0, 0, 1, 1)
#' V(graphres)$nvals <- c(3, 2, 2, 2, 2)
#' V(graphres)$exposure <- c(0, 1, 0, 0, 0)
#' V(graphres)$outcome <- c(0, 0, 1, 0, 0)
#' E(graphres)$rlconnect <- c(0, 0, 0, 0, 0)
#' E(graphres)$edge.monotone <- c(0, 0, 0, 0, 0)
#' constraints <- "X(Z = 1) >= X(Z = 0)"
#' cond.vars <- V(graphres)[V(graphres)$leftside == 1 & names(V(graphres)) != "Ul"]
#' right.vars <- V(graphres)[V(graphres)$leftside == 0 & names(V(graphres)) != "Ur"] 
#' respvars <- create_response_function(graphres, right.vars, cond.vars)
#' create_q_matrix(respvars, right.vars, cond.vars, constraints)

create_q_matrix <- function(respvars, right.vars, cond.vars, constraints) {
    
    
    obsvars <- c(right.vars, cond.vars)
    notsatlist <- NULL
    if(!is.null(constraints)) {
        
        
        parsed.constraints <- parse_constraints(constraints, names(obsvars)) 
        
        ### apply parsed constraints
        
        for(j in 1:nrow(parsed.constraints)) {
            
            iin <- parsed.constraints[j, ]
            tmpenv.left <- tmpenv.right <- list()
            tmpenv.left <- within(tmpenv.left, eval(parse(text = iin$leftcond)))
            tmpenv.right <- within(tmpenv.right, eval(parse(text = iin$rightcond)))
            
            resp.out.left <- unlist(lapply(respvars[[iin$leftout]]$values, function(f) do.call(f, tmpenv.left)))
            resp.out.right <- unlist(lapply(respvars[[iin$rightout]]$values, function(f) do.call(f, tmpenv.right)))
            
            if(!is.na(suppressWarnings(as.numeric(iin$rightout)))) {
                resp.out.right <- rep(iin$rightout, length(resp.out.left))
            }
            
            if(iin$leftout == iin$rightout | !is.na(suppressWarnings(as.numeric(iin$rightout)))) {  ## constraints are for the same counterfactual, these lead to removals of qs
                settozeroindex <- respvars[[iin$leftout]]$index[!do.call(iin$operator, list(resp.out.left, resp.out.right))]
                
                if(length(settozeroindex) > 0) {
                    removedex <- respvars[[iin$leftout]]$index %in% settozeroindex
                    
                    respvars[[iin$leftout]]$index <- respvars[[iin$leftout]]$index[!removedex] 
                    respvars[[iin$leftout]]$values <- respvars[[iin$leftout]]$values[!removedex] 
                    
                }
                
            } else {  ## otherwise these lead to added constraints
                
                lnotsat <- length(notsatlist)
                finddex <- !do.call(iin$operator, expand.grid(resp.out.left, resp.out.right))
                notsat <- expand.grid(respvars[[iin$leftout]]$index, respvars[[iin$rightout]]$index)[finddex, ]
                colnames(notsat) <- c(iin$leftout, iin$rightout)  
                ## these sets of response variables do not satisfy the constraints and should be removed from the q.vals table
                notsatlist[[lnotsat + 1]] <- notsat
                
            }
            
        }
        
    } #endif
    
    
    q.vals.all <- do.call(expand.grid, lapply(respvars, "[[", 1))
    ## remove rows not in notsatlist
    if(length(notsatlist) > 0) {
        
        for(j in 1:length(notsatlist)) {
            
            q.vals.tmp <- merge(q.vals.all, cbind(notsatlist[[j]], remove=1), all.x = TRUE)
            q.vals.all <- q.vals.tmp[is.na(q.vals.tmp$remove), -ncol(q.vals.tmp)]
            
        }
        
    }
    
    q.vals <- do.call(expand.grid, lapply(respvars, "[[", 1)[which(obsvars %in% right.vars)])
    
    variables <- paste0("q", do.call(paste, c(q.vals, sep = "_")))
    
    q.vals.tmp2 <- cbind(q.vals, vars = variables, stringsAsFactors = FALSE)
    q.vals.all.lookup <- merge(q.vals.all, q.vals.tmp2, by = names(right.vars), sort = TRUE)
    
    
    list(q.vals = q.vals, q.vals.all = q.vals.all, q.vals.all.lookup = q.vals.all.lookup)
    
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
    obsvarnames <- names(obsvars)
    
    parent_lookup <- lapply(1:length(obsvars), function(i) {
        tmpar <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
        tmpar[!names(tmpar) %in% c("Ul", "Ur")]
    })
    
     gee_r <- function(r, i) {
        
         parents <- parent_lookup[[i]]
         
        #parents <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
        #parents <- parents[!names(parents) %in% c("Ul", "Ur")]
        
        
        if (length(parents) == 0){
            
            x <- respvars[[obsvarnames[i]]]$values[[which(respvars[[obsvarnames[i]]]$index == r[i])]]
            do.call(x, list())
            
        } else {
            
            lookin <- lapply(names(parents), function(gu) {
                
                as.numeric(gee_r(r, which(obsvarnames == gu)))
                
            })
            names(lookin) <- names(parents)
            inres <- respvars[[obsvarnames[i]]]$values[[which(respvars[[obsvarnames[i]]]$index == r[i])]]
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
    colnames(res.mat) <- obsvarnames
    
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


#' Translate target effect to vector of response variables
#' 
#' @param effect Effect list, as returned by \link{parse_effect}
#' @param graph The graph
#' @param obsvars Vector of observed variable vertices from the graph 
#' @param respvars Response function, as returned by \link{create_response_function}
#' @param q.list List with q matrices, as returned by \link{create_q_matrix}
#' @param variables Vector of qs names
#' 
#' @export
#' @return A list with the target effect in terms of qs
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
#' parameters <- linconstr.list$newparams
#' p.vals <- linconstr.list$newpvals
#' effect <- parse_effect(effectt)
#' var.eff <- create_effect_vector(effect, graph, obsvars, respvars, q.list, variables)

create_effect_vector <- function(effect, graph, obsvars, respvars, q.list, variables) {
    
    q.vals <- q.list$q.vals
    q.vals.all <- q.list$q.vals.all
    q.vals.all.lookup <- q.list$q.vals.all.lookup
    obsvarnames <- names(obsvars)
    
    parent_lookup <- lapply(1:length(obsvars), function(i) {
        tmpar <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
        tmpar[!names(tmpar) %in% c("Ul", "Ur")]
    })
    
    var.eff <- NULL
    for(v in 1:length(effect$vars)) {
        
        #nest 
        thisterm <- effect$vars[[v]]
        
        res.mat.list <- vector(mode = "list", length = length(thisterm))
        
        for(v2 in 1:length(thisterm)){
            
            thisvar <- thisterm[[v2]]
            outcome <- V(graph)[names(V(graph)) == names(thisterm)[v2]]
            #intervene <- vector(mode = "list")
            
            if(effect$pcheck[[v]][v2] == FALSE) { ## observation
                
                
                gee_r <- function(r, i) {
                    
                    parents <- parent_lookup[[i]]
                    #parents <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
                    #parents <- parents[!names(parents) %in% c("Ul", "Ur")]
                    
                    
                    if (length(parents) == 0){
                        
                        x <- respvars[[obsvarnames[i]]]$values[[which(respvars[[obsvarnames[i]]]$index == r[i])]]
                        do.call(x, list())
                        
                    } else {
                        
                        lookin <- lapply(names(parents), function(gu) {
                            
                            as.numeric(gee_r(r, which(obsvarnames == gu)))
                            
                        })
                        names(lookin) <- names(parents)
                        inres <- respvars[[obsvarnames[i]]]$values[[which(respvars[[obsvarnames[i]]]$index == r[i])]]
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
                colnames(res.mat) <- obsvarnames
                
                res.mat.list[[v2]] <- res.mat
                
                
            } else { ## intervention
                
                thisintervene <- unlist(list_to_path(thisvar, names(outcome)))
                basevars <- sapply(strsplit(names(thisintervene), " -> "), "[", 1)
                ## check for missing paths from intervention sets to outcome
                ## only do this if any of the top level intervention sets doesn't contain all
                ## parents of the outcome
                ## the logic being that if the user wrote that as an effect, then 
                ## the intention was to propagate that intervention set forward through
                ## all paths in the graph to the outcome
                
                parents <- adjacent_vertices(graph, v = outcome, mode = "in")[[1]]
                if(length(setdiff(names(parents[which(names(parents) != "Ur")]), 
                                  names(thisvar))) > 0) {
                isets <- unique(btm_var(thisvar))
                missingpaths <- lapply(isets, function(cc) {
                    allpaths <- all_simple_paths(graph, from = cc, to = names(outcome), mode = "out")
                    paths2 <- unlist(lapply(allpaths, function(x) paste(names(x), collapse = " -> ")))
                    setdiff(paths2, names(thisintervene))
                })
                for(pp in 1:length(missingpaths)) {

                    if(length(missingpaths[[pp]]) == 0) {
                        next
                    }
                    addval <- thisintervene[which(isets[pp] == basevars)[1]]
                    addval2 <- rep(addval, length(missingpaths[[pp]]))
                    names(addval2) <- missingpaths[[pp]]
                    thisintervene <- c(thisintervene, addval2)

                }
                }
                
                gee_rA <- function(r, i, path = NULL) {
                    
                    parents <- parent_lookup[[i]]
                    #parents <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
                    #parents <- parents[!names(parents) %in% c("Ul", "Ur")]
                    
                    if(!is.null(path)){
                        #thisintervene <- intervene[[childcall]]
                    }
                    if(!is.null(path) && path %in% names(thisintervene)) {
                        
                        as.numeric(thisintervene[[path]])
                        
                    } else if (length(parents) == 0){
                        
                        x <- respvars[[obsvarnames[i]]]$values[[which(respvars[[obsvarnames[i]]]$index == r[i])]]
                        do.call(x, list())
                        
                    } else {
                        
                        lookin <- lapply(names(parents), function(gu) {
                            
                            as.numeric(gee_rA(r, which(obsvarnames == gu), path = paste(gu, "->", path)))
                            
                        })
                        names(lookin) <- names(parents)
                        inres <- respvars[[obsvarnames[i]]]$values[[which(respvars[[obsvarnames[i]]]$index == r[i])]]
                        do.call(inres, lookin)
                        
                    }
                }
                
                
                res.mat <- matrix(NA, ncol = ncol(q.vals.all), nrow = nrow(q.vals.all))
                for(k in 1:nrow(q.vals.all)) {
                    for(j in 1:ncol(q.vals.all)) {
                        res.mat[k, j] <- gee_rA(r = unlist(q.vals.all.lookup[k, -ncol(q.vals.all.lookup)]), i = j, 
                                                path = obsvarnames[j])
                        
                    }
                }
                colnames(res.mat) <- obsvarnames
                
                res.mat.list[[v2]] <- res.mat
                
            }
            
        }
        
        
        
        
        var.dex <- rep(TRUE, nrow(q.vals.all))
        for(i in 1:length(effect$values[[v]])) {
            
            var.dex <- var.dex & res.mat.list[[i]][, names(effect$values[[v]])[i]] == effect$values[[v]][[i]]
            
        }
        var.eff[[v]] <- unique(as.character(q.vals.all.lookup[var.dex, "vars"]))
        
    }
    
    var.eff
    
}
