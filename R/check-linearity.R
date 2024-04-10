#' Check linearity of constraint space implied by a set of response functions
#' 
#' @param respvars List of response functions as produced by \link{create_response_function}
#' @param p.vals Matrix defining which probabilities are observable
#' @param prob.form A list with two named elements "out", "cond" where each element is a character vector
#' 
#' @details
#' It is assumed that probabilities of the form p(out | cond) are observed, for each combination of values in p.vals
#'
#' @returns Something useful
#' 
check_linear_constraints <- function(respvars, p.vals, prob.form) {
    
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
    
    jd <- do.call(paste0, p.vals[, prob.form$out, drop = FALSE])
    cond <- do.call(paste0, p.vals[, prob.form$cond, drop = FALSE])
    
    parameters <- paste0("p", paste(jd, cond, sep = "_"))
    
    variables <- list(unique(q.vals.all.lookup$vars), 
                      unique(q.vals.all.lookup$vars2))
    
    
    parent_lookup <- lapply(respvars, \(var) {
        
        unlist(lapply(var$values, \(fun) {
         names(formals(fun))
        })) |> unique()
        
    })
    
    obsvars <- as.list(names(q.vals.all.lookup)[-c(ncol(q.vals.all.lookup) - 1, ncol(q.vals.all.lookup))])
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
    for (k in 1:nrow(q.vals.all)) {
        for (j in 1:ncol(q.vals.all)) {
            res.mat[k, j] <- gee_r(r = unlist(q.vals.all.lookup[k, 
                                                                -c(ncol(q.vals.all.lookup), ncol(q.vals.all.lookup) - 1)]), i = j)
        }
    }
    colnames(res.mat) <- obsvars
    removeprows <- rep(0, nrow(p.vals))
    p.constraints <- rep(NA, nrow(p.vals) + 1)
    p.constraints[1] <- paste(paste(outer(variables[[1]], variables[[2]], "paste"), collapse = " + "), 
                              " = 1")
    checkcond <- rep(NA, nrow(p.vals))
    for (pj in 1:nrow(p.vals)) {
        p.chk <- do.call(rbind, lapply(1:nrow(res.mat), function(i) p.vals[pj, 
                                                                           , drop = FALSE]))
        inp <- apply(res.mat[, colnames(p.chk), drop = FALSE] == 
                         p.chk, 1, all)
        if (!any(inp)) {
            removeprows[pj] <- 1
            next
        }
        else {
            pr.match <- q.vals.all.lookup[inp, ]
            
            obs.condvar <- q.vals.all.lookup[apply(res.mat[, prob.form$cond, drop = FALSE] == 
                                                       p.chk[, prob.form$cond, drop = FALSE], 
                                 1, all), ncol(q.vals.all.lookup)] |> unique()
            
            q.match <- q.vals.all.lookup[inp, ncol(q.vals.all.lookup) - 1]
            k.match <- q.vals.all.lookup[inp, ncol(q.vals.all.lookup)]
            
            checkcond[pj] <- all(k.match == obs.condvar)
            if (length(q.match) == 0) {
                q.match <- "0"
            }
            p.constraints[pj + 1] <- paste(parameters[pj], "=", 
                                           paste(unique(q.match), collapse = " + "))
        }
    }
    p.vals <- p.vals[removeprows == 0, , drop = FALSE]
    parameters <- parameters[removeprows == 0]
    p.constraints <- p.constraints[!is.na(p.constraints)]
    
    
    ## return something
    
    all(checkcond) ## true if linear
    
}