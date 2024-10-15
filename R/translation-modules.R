#' Translate regular DAG to response functions
#'
#' @param graph An \link[igraph]{aaa-igraph-package} object that represents a
#'   directed acyclic graph that contains certain edge attributes. 
#'    The shiny app returns a graph in this format and \link{initialize_graph} 
#'    will add them to a regular igraph object with sensible defaults.
#' 
#' @return A list of functions representing the response functions
#' @export
#' @examples 
#' ### confounded exposure and outcome

#' b <- initialize_graph(igraph::graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y))
#' create_response_function(b)

create_response_function <- function(graph) {
    
    cond.vars <- V(graph)[V(graph)$leftside == 1 & names(V(graph)) != "Ul"]
    right.vars <- V(graph)[V(graph)$leftside == 0 & names(V(graph)) != "Ur"] 
    
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
            matrices <- lapply(1:numberOfValues(graph, names(i)), function(j) {
                mm <- matrix(j - 1)
                colnames(mm) <- names(i)
                mm
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
            
            values <- matrices <- vector(mode = "list", length = nrow(ini.outs))
            for (j in 1:nrow(ini.outs)) {
                f.tmp <- function() {
                }
                formals(f.tmp) <- as.pairlist(args)
                ## matrix
                
                matrices[[j]] <- cbind(poss.ins, unlist(ini.outs[j,]))
                rownames(matrices[[j]]) <- NULL
                colnames(matrices[[j]])[ncol(matrices[[j]])] <- names(i)
                
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
            list(index = 0:(length(values) - 1), values = values, matrices = matrices)
        
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
#' graphres <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y))
#' constraints <- "X(Z = 1) >= X(Z = 0)"
#' cond.vars <- V(graphres)[V(graphres)$leftside == 1 & names(V(graphres)) != "Ul"]
#' right.vars <- V(graphres)[V(graphres)$leftside == 0 & names(V(graphres)) != "Ur"] 
#' respvars <- create_response_function(graphres)
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
    q.vals.all.lookup <- merge(q.vals.all, q.vals.tmp2, by = right.vars, sort = TRUE)
    
    
    list(q.vals = q.vals, q.vals.all = q.vals.all, q.vals.all.lookup = q.vals.all.lookup)
    
}


#' Translate target effect to vector of response variables
#' 
#' @param causal_model An object of class "causalmodel" as produced by \link{create_causalmodel}
#' @param effect Effect list, as returned by \link{parse_effect}
#' 
#' @export
#' @return A list with the target effect in terms of qs
#' @examples
#' graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y))
#' constraints <- "X(Z = 1) >= X(Z = 0)"
#' effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
#' p.vals <- expand.grid(Z = 0:1, X = 0:1, Y = 0:1)
#' prob.form <- list(out = c("X", "Y"), cond = "Z")
#' effect <- parse_effect(effectt)
#' ivmod <- create_causalmodel(graph, respvars = NULL, p.vals = p.vals, prob.form = prob.form, 
#'          constraints = constraints)
#' var.eff <- create_effect_vector(ivmod, effect)

create_effect_vector <- function(causal_model, effect) {
    
    q.vals.all.lookup <- causal_model$data$q.vals[, -ncol(causal_model$data$q.vals)]
    q.vals.all <- q.vals.all.lookup[, -ncol(q.vals.all.lookup)]
    q.vals <- unique(q.vals.all[, unlist(causal_model$data$prob.form), drop = FALSE])
    
    respvars <- causal_model$data$response_functions
    
    parent_lookup <- lapply(respvars, \(var) {
        
        unlist(lapply(var$values, \(fun) {
            names(formals(fun))
        })) |> unique()
        
    })
    
    obsvars <- as.list(names(q.vals.all.lookup)[-c(ncol(q.vals.all.lookup))])
    names(obsvars) <- unlist(obsvars)
    
    obsvarnames <- names(obsvars)
    
    
    var.eff <- NULL
    for(v in 1:length(effect$vars)) {
        
        #nest 
        thisterm <- effect$vars[[v]]
        
        res.mat.list <- vector(mode = "list", length = length(thisterm))
        
        for(v2 in 1:length(thisterm)){
            
            thisvar <- thisterm[[v2]]
            #outcome <- V(graph)[names(V(graph)) == names(thisterm)[v2]]
            #intervene <- vector(mode = "list")
            outcome <- names(thisterm)[v2]
            
            if(effect$pcheck[[v]][v2] == FALSE) { ## observation
                
                
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
                colnames(res.mat) <- obsvarnames
                
                res.mat.list[[v2]] <- res.mat
                
                
            } else { ## intervention
                
                thisintervene <- unlist(list_to_path(thisvar, outcome))
                basevars <- sapply(strsplit(names(thisintervene), " -> "), "[", 1)
                ## check for missing paths from intervention sets to outcome
                ## only do this if any of the top level intervention sets doesn't contain all
                ## parents of the outcome
                ## the logic being that if the user wrote that as an effect, then 
                ## the intention was to propagate that intervention set forward through
                ## all paths in the graph to the outcome
                
                parents <- parent_lookup[[outcome]]
                #adjacent_vertices(graph, v = outcome, mode = "in")[[1]]
                
                if(length(setdiff(parents, 
                                  names(thisvar))) > 0) {
                isets <- unique(btm_var(thisvar))
                missingpaths <- lapply(isets, function(cc) {
                    #allpaths <- all_simple_paths(graph, from = cc, to = outcome, mode = "out")
                    paths2 <- find_all_paths(respvars, from = cc, to = outcome)
                    
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
                    
                    parents <- parent_lookup[[obsvars[[i]]]]
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
                        
                        lookin <- lapply(parents, function(gu) {
                            
                            as.numeric(gee_rA(r, which(obsvarnames == gu), path = paste(gu, "->", path)))
                            
                        })
                        names(lookin) <- parents
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
