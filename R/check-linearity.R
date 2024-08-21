#' Check linearity of objective function implied by a set of response functions, graph, and effect
#' 
#' @param respvars List of response functions as produced by \link{create_response_function}
#' @param graph igraph object
#' @param effectt A character string that represents the causal effect of interest
#' @param prob.form A list with two named elements "out", "cond" where each element is a character vector
#' 
#' @details
#' It is assumed that probabilities of the form p(out | cond) are observed, for each combination of values in p.vals. This is limited currently to DAGs that still fit into out left-side right-side framework
#'
#' @returns A logical value that is TRUE if the objective function is linear
#' @export
#' @examples
#'  ## regular IV case
#' 
#' graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y))
#' 
#' observed.variables <- V(graph)[V(graph)$latent == 0]
#' var.values <- lapply(names(observed.variables), 
#'                      function(varname) seq(from = 0, to = causaloptim:::numberOfValues(graph, varname) - 1))
#' names(var.values) <- names(observed.variables)
#' p.vals <- do.call(expand.grid, var.values)
#' 
#' respvars <- create_response_function(graph)
#' prob.form <- list(out = c("Y", "X"), cond = "Z")
#' 
#' effectt <- "p{Y(X = 1) = 1}"
#' check_linear_objective(respvars, graph, effectt, prob.form)
#' 
#' #'  ## contaminated IV case
#' 
#' graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Z-+ Y, Ur -+ X, Ur -+ Y))
#' V(b)$leftside <- c(1,0,0,0)
#' V(b)$latent <- c(0, 0,0,1)
#' V(b)$nvals <- c(2,2,2,2)
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0, 0, 0)
#' 
#' graph <- b
#' 
#' observed.variables <- V(graph)[V(graph)$latent == 0]
#' var.values <- lapply(names(observed.variables), 
#'                      function(varname) seq(from = 0, to = causaloptim:::numberOfValues(graph, varname) - 1))
#' names(var.values) <- names(observed.variables)
#' p.vals <- do.call(expand.grid, var.values)
#' 
#' respvars <- create_response_function(graph)
#' prob.form <- list(out = c("Y", "X"), cond = "Z")
#' 
#' effectt <- "p{Y(X = 1) = 1}"
#' check_linear_objective(respvars, effectt, prob.form)
#' 

check_linear_objective <- function(respvars, effectt, prob.form) {
    
    prob.form <- lapply(prob.form, \(x) {
        names(x) <- x
        x
    })
    q.list <- create_q_matrix(respvars, right.vars = prob.form$out, cond.vars= prob.form$cond, 
                              constraints = NULL)
    
    q.vals <- q.list$q.vals
    q.vals.all <- q.list$q.vals.all
    q.vals.all.lookup <- q.list$q.vals.all.lookup
    q.vals.all.lookup <- cbind(q.vals.all.lookup, 
                               vars2 = paste0("k", 
                                              do.call(paste, 
                                                      c(lapply(prob.form$cond, \(var) q.vals.all.lookup[, var]), sep = "_"))))
    
   
    variables <- list(unique(q.vals.all.lookup$vars), 
                      unique(q.vals.all.lookup$vars2))
    
    
    parent_lookup <- lapply(respvars, \(var) {
        
        unlist(lapply(var$values, \(fun) {
            names(formals(fun))
        })) |> unique()
        
    })
    
    obsvars <- as.list(names(q.vals.all.lookup)[-c(ncol(q.vals.all.lookup) - 1, ncol(q.vals.all.lookup))])
    names(obsvars) <- unlist(obsvars)
    
    effect <- parse_effect(effectt)
    chk0 <- lapply(effect$vars, causaloptim:::btm_var)
    interven.vars <- unique(unlist(chk0))
    allnmes <- unique(unlist(lapply(effect$vars, names)))
    realnms <- unlist(prob.form)
    
    obsvarnames <- names(obsvars)
    
    var.eff <- check_linear <-  NULL
    for(v in 1:length(effect$vars)) {
        
        #nest 
        thisterm <- effect$vars[[v]]
        
        res.mat.list <- vector(mode = "list", length = length(thisterm))
        
        for(v2 in 1:length(thisterm)){
            
            thisvar <- thisterm[[v2]]
            outcome <- names(thisterm)[v2]
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
                        
                        lookin <- lapply(parents, function(gu) {
                            
                            as.numeric(gee_r(r, which(obsvarnames == gu)))
                            
                        })
                        names(lookin) <- parents
                        inres <- respvars[[obsvarnames[i]]]$values[[which(respvars[[obsvarnames[i]]]$index == r[i])]]
                        do.call(inres, lookin)
                        
                    }
                }
                
                
                res.mat <- matrix(NA, ncol = ncol(q.vals.all), nrow = nrow(q.vals.all))
                q.vals.mat <- as.matrix(q.vals.all.lookup[, -c(ncol(q.vals.all.lookup), ncol(q.vals.all.lookup)-1)])
                for(k in 1:nrow(q.vals.all)) {
                    for(j in 1:ncol(q.vals.all)) {
                        res.mat[k, j] <- gee_r(r = q.vals.mat[k, ], i = j)
                        
                    }
                }
                colnames(res.mat) <- obsvarnames
                
                res.mat.list[[v2]] <- res.mat
                
                
            } else { ## intervention
                
                thisintervene <- unlist(causaloptim:::list_to_path(thisvar, outcome))
                basevars <- sapply(strsplit(names(thisintervene), " -> "), "[", 1)
                
                parents <- parent_lookup[[outcome]]
                if(length(setdiff(parents, 
                                  names(thisvar))) > 0) {
                    isets <- unique(btm_var(thisvar))
                    missingpaths <- lapply(isets, function(cc) {
                        
                        #allpaths <- all_simple_paths(graph, from = cc, to = outcome, mode = "out")
                       
                        allpaths <- check_parents(parent_lookup, cc, outcome)
                        if(!is.list(allpaths)) {
                            allpaths <- list(allpaths)
                        } 
                        ap2 <- NULL
                        for(i in 1:length(allpaths)) {
                            la <- unlist(allpaths[[i]])
                            if(la[1] == cc & la[length(la)] == outcome) {
                                ap2[[length(ap2) + 1]] <- la
                            }
                        }
                        
                        paths2 <- unlist(lapply(ap2, function(x) paste(x, collapse = " -> ")))
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
                    
                    parents <- parent_lookup[[obsvarnames[i]]]
                    names(parents) <- parents
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
                        res.mat[k, j] <- gee_rA(r = unlist(q.vals.all.lookup[k, -c(ncol(q.vals.all.lookup), 
                                                                                   ncol(q.vals.all.lookup)-1)]), i = j, 
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
        
        matp <- q.vals.all.lookup[var.dex, ]
        tswag <- table(matp[, ncol(matp) - 1], matp[, ncol(matp)]) # if linear, this should all be 1s
        
        var.eff[[v]] <- unique(as.character(q.vals.all.lookup[var.dex, "vars"]))
        check_linear[[v]] <- all(tswag == 1)
        
    }
    
    
    
    all(unlist(check_linear))
    
}
