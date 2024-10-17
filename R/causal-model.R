#' Create a structural causal model from a graph or a set of response functions
#' 
#' Given either a graph or a set of response functions (i.e., either \code{graph} or \code{respvars} may be provided), and a specification of what conditional probabilities are observed, produce a causal model. 
#' 
#' @param graph A graph with special edge and vertex attributes, as produced by \link{initialize_graph}
#' @param respvars List of response functions as produced by \link{create_response_function}
#' @param prob.form A list with two named elements "out", "cond" where each element is a character vector of variable names that appear in p.vals
#' @param p.vals Data frame defining which probabilities are observable. The variable names of p.vals must all appear in prob.form. If missing, will assume that all combinations of the variables values are observed.
#' @param constraints A vector of character strings that represent the constraints on counterfactual quantities
#' @param right.vars A vector of character strings indicating which variables are on the right side of the graph. Only required when graph is NULL. See examples.
#' 
#' @details
#' It is assumed that probabilities of the form p(out | cond) are observed, for each combination of values in p.vals. cond may be NULL in which case nothing is conditioned on.
#' 
#' The constraints are specified in terms of potential outcomes to constrain by writing the potential outcomes, values of their parents, and operators that determine the constraint (equalities or inequalities). For example,
#' \code{X(Z = 1) >= X(Z = 0)}
#'
#' @returns An object of class "causalmodel"
#' @export
#' @examples
#'  ## regular IV case
#' 
#' graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y))
#' 
#' iv_model <- create_causalmodel(graph, prob.form = list(out = c("X", "Y"), cond = "Z"))
#' # with monotonicity
#' iv_model_mono <- create_causalmodel(graph, prob.form = list(out = c("X", "Y"), cond = "Z"),
#'                  constraints = list("X(Z = 1) >= X(Z = 0)"))
#'                  
#' #showing the use of right.vars
#' b <- initialize_graph(graph_from_literal(Ul -+ X -+ Y -+ Y2, Ur -+ Y, Ur -+ Y2))
#' V(b)$latent <- c(1, 0, 1, 0, 1)
#' respvars <- create_response_function(b)
#' create_causalmodel(graph = b, constraints = "Y2(Y = 1) >= Y2(Y = 0)",
#'                    p.vals = expand.grid(X = 0:1, Y2 = 0:1), 
#'                    prob.form = list(out = "Y2", cond = "X"))
#'  ## need to specify right.vars because it cannot be inferred from the response functions alone
#' create_causalmodel(graph = NULL, respvars = respvars, 
#'                    constraints = "Y2(Y = 1) >= Y2(Y = 0)",
#'                    p.vals = expand.grid(X = 0:1, Y2 = 0:1), 
#'                    prob.form = list(out = "Y2", cond = "X"), 
#'                    right.vars = c("Y", "Y2"))
#' 
create_causalmodel <- function(graph = NULL, respvars = NULL, prob.form,
                               p.vals, constraints = NULL, 
                               right.vars = NULL) { 
    
   
    
    
    if(is.null(graph) & is.null(respvars)) {
        stop("Either graph or respvars must be provided")
    }
    
    if(!is.null(respvars) & !is.null(graph)) {
        message("Both graph and respvars provided, using respvars")
        right.vars <- names(V(graph)[V(graph)$leftside == 0 & names(V(graph)) != "Ur"])
    }
    
    if(is.null(respvars) & !is.null(graph)) {
        
        respvars <- create_response_function(graph)
        right.vars <- names(V(graph)[V(graph)$leftside == 0 & names(V(graph)) != "Ur"])
        
    }
    
    if(missing(p.vals)) {
        
        expand.res <- lapply(names(respvars), \(vn) {
            unique(unlist(lapply(respvars[[vn]]$matrices, \(vm) {
                vm[, vn]
            })))
        })
        
        names(expand.res) <- names(respvars)
        p.vals <- do.call(expand.grid, expand.res)
        
    }
    
    
    varnames1 <- colnames(p.vals)
    varnames2 <- unlist(prob.form)
    
    if(length(setdiff(varnames1, varnames2)) > 0) {
        stop("The following variables are present in p.vals but not in prob.form: ", 
             paste(setdiff(varnames1, varnames2), collapse = ", "))
    }
    
    if(length(setdiff(varnames2, varnames1)) > 0) {
        stop("The following variables are present in prob.form but not in p.vals: ", 
             paste(setdiff(varnames2, varnames1), collapse = ", "))
    }
    
    if(is.null(right.vars)) {
        needright <- setdiff(names(respvars), varnames1)
        if(length(needright) > 0) {
            stop("May need to specify right.vars for variable: ", paste(needright, collapse = ", "))
        }
        
        right.vars <- prob.form$out
    }
    
    prob.form <- lapply(prob.form, \(x) {
        names(x) <- x
        x
    })
    q.list <- create_q_matrix(respvars, right.vars = right.vars, cond.vars= prob.form$cond, 
                              constraints = constraints)
    
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
    parameters.key <- paste(paste(prob.form$out, collapse = ""), 
                            paste(prob.form$cond, collapse = ""), sep = "_")
    
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
            res.mat[k, j] <- gee_r(r = unlist(q.vals.all.lookup[k, -c(ncol(q.vals.all.lookup), ncol(q.vals.all.lookup) - 1)]), i = j)
        }
    }
    colnames(res.mat) <- obsvars
    
    R <- matrix(0, nrow = nrow(p.vals) + 1, ncol = nrow(q.vals))
    R[1, ] <- 1
    
    removeprows <- rep(0, nrow(p.vals))
    p.constraints <- rep(NA, nrow(p.vals) + 1)
    p.constraints[1] <- paste(paste(variables[[1]], collapse = " + "), 
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
        } else {
            pr.match <- q.vals.all.lookup[inp, ]
            
            obs.condvar <- q.vals.all.lookup[apply(res.mat[, prob.form$cond, drop = FALSE] == 
                                                       p.chk[, prob.form$cond, drop = FALSE], 
                                                   1, all), ncol(q.vals.all.lookup)] |> unique()
            
            q.match <- q.vals.all.lookup[inp, ncol(q.vals.all.lookup) - 1]
            k.match <- q.vals.all.lookup[inp, ncol(q.vals.all.lookup)]
            
            R[pj + 1, match(unique(q.match), variables[[1]])] <- 1
            tabkq <- table(q.match, k.match)
            
            checkcond[pj] <- all(tabkq == 1)#all(k.match == obs.condvar)
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
    
    linear.if.true <- all(checkcond)
    
    ## inequalities
    
    vrep <- rcdd::makeV(points = t(R[-1,]))
    hrep2 <- rcdd::scdd(vrep, representation = "V")
    foo <- hrep2$output
    l <- foo[ , 1]
    b <- foo[ , 2]
    v <- foo[ , - c(1, 2)]
    a <- -v
    # axb <- a %*% x - b
    # all(axb <= 0)
    # all(l * axb == 0)
    
    
    if(!is.null(prob.form$cond)){
        cond.vals <- p.vals[, prob.form$cond, drop = FALSE]
        cond.str <- apply(cond.vals, MARGIN = 1, \(x) paste(x, collapse = "_"))
        parmsets.that.sum.to.one <- lapply(unique(cond.str), \(x) which(cond.str == x))
    } else {
        parmsets.that.sum.to.one <- list(1:length(parameters))
    }
    
    inequalities.text <- rep(NA_character_, nrow(a))
    for(i in 1:nrow(a)) {
        
        ## checking conditions for standard probabilistic constraints that we omit
        if(l[i] == 0) {  ## inequalities
            if(sum(a[i,] == -1) == 1 & b[i] == 0) {
                next
            }
            nonzeros <- which(a[i,] != 0)
            nomatchparmsets <- sapply(parmsets.that.sum.to.one, \(x) length(setdiff(nonzeros, x)) == 0)
            if(sum(nomatchparmsets == TRUE) == 1 & all(a[i,][nonzeros] == b[i]) & abs(b[i]) == 1) {
                next
            }
            
        } else if(l[i] == 1) { ## equalities
            nonzeros <- which(a[i,] != 0)
            matchparmsets <- sapply(parmsets.that.sum.to.one, \(x) length(setdiff(nonzeros, x)) == 0 & length(setdiff(x, nonzeros)) == 0)
            if(sum(matchparmsets == TRUE) == 1 & all(a[i,][nonzeros] == b[i]) & abs(b[i]) == 1){
                next
            }
        }
        
        
        thisp <- c(parameters)[as.logical(abs(a[i,]))]
        
        inp <- paste(paste(a[i,a[i,]!=0], "*", thisp), collapse = " + ") 
        
        inequalities.text[i] <- if(l[i] == 0) paste(inp, "<=", b[i]) else{
            paste(l[i], "*(", inp, " - ", b[i], ") = 0") 
        }
    }
    
    observable_constraints <- list(numeric = list(
        a = a, b = b, l = l
    ), 
    character = inequalities.text[!is.na(inequalities.text)])
    
    counterfactual_constraints <- list(numeric = list(R = R), 
                                       character = p.constraints, 
                                       linear.if.true = linear.if.true)
    
    attr(parameters, "key") <- parameters.key
    attr(parameters, "rightvars") <- right.vars
    attr(parameters, "condvars") <- prob.form$cond
    
    data <- list(response_functions = respvars, graph = graph, 
                 variables = variables[[1]], parameters = parameters, 
                 prob.form = prob.form,
                 p.vals = p.vals, q.vals = q.vals.all.lookup, 
                 user_constraints = constraints
                 )
    
    obj <- list(data = data, observable_constraints = observable_constraints, 
                counterfactual_constraints = counterfactual_constraints)
   
    class(obj) <- "causalmodel"
    obj
    
}

#' Sample a distribution of observable probabilities that satisfy the causal model
#' 
#' @param obj An object of class "causalmodel"
#' @param simplex_sampler A function to generate a random sample from the simplex in k dimensions, where k is the number of variables (q parameters, obj$data$variables). By default this is uniform (symmetric dirichlet with parameter 1).
#' @returns A vector of observable probabilities that satisfy the causal model
#' @export
#' @examples
#' 
#' graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y))
#' prob.form <- list(out = c("X", "Y"), cond = "Z")
#' 
#' iv_model <- create_causalmodel(graph, prob.form = prob.form)
#' sample_distribution(iv_model)
#' 

sample_distribution <- function(obj, 
                                simplex_sampler = function(k) { rdirichlet(k, alpha = 1) }) {
    
    stopifnot(inherits(obj, "causalmodel"))
    sim.qs <- simplex_sampler(length(obj$data$variables))
    
    stopifnot(length(sim.qs) == length(obj$data$variables)) 
    stopifnot(abs(sum(sim.qs) - 1) < 1e-12)
    
    res <- c(obj$counterfactual_constraints$numeric$R[-1, ] %*% sim.qs)
    names(res) <- obj$data$parameters
    res
    
}

#' Check whether any of the observable constraints implied by the causal model are violated for a given distribution of observables
#' 
#' @param obj An object of class "causalmodel"
#' @param probs A named vector of observable probabilities, in the same order as obj$data$parameters
#' @param tol Tolerance for checking (in)equalities
#' @export
#' @returns Either TRUE (violated) or FALSE (not violated) with messages indicating what constraints are violated if any.
#' @examples
#' graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y))
#' 
#' iv_model <- create_causalmodel(graph, prob.form = list(out = c("X", "Y"), cond = "Z"))
#' check_constraints_violated(iv_model, probs = sample_distribution(iv_model))
#' 

check_constraints_violated <- function(obj, probs, tol = 1e-12) {
    
    stopifnot(inherits(obj, "causalmodel"))
    if(!identical(names(probs), c(obj$data$parameters))) {
        
        warning("Vector of probabilities must be given in this form/order: ", 
                paste(obj$data$parameters, collapse = " "))
        
    }
    
    axb <- c(obj$observable_constraints$numeric$a %*% probs - obj$observable_constraints$numeric$b)
    chk1 <- abs(axb * obj$observable_constraints$numeric$l) < tol
    chk2 <- axb <= tol
    
    if(any(!chk1)) {
        
        message("Equality constraints violated: ", 
                paste(obj$observable_constraints$character[which(!chk1)], collapse = ", "), 
                " checked with tolerance ", tol)
        
    }
    
    if(any(!chk2)) {
        
        message("Inequality constraints violated: ", 
                paste(obj$observable_constraints$character[which(!chk2)], collapse = ", "), 
                " checked with tolerance ", tol)
        
        
    }
    
    res <- any(!chk1) | any(!chk2)
    message("Constraints ", c("not violated", "violated")[res + 1])
    res
    
}



#' Print relevant information about the causal model
#' 
#' @param x object of class "causalmodel"
#' @param omit_cf_constraints Do not print the counterfactual constraints
#' @param omit_obs_constraints Do not print the observable constraints
#' @param ... Not used
#' @return x, invisibly
#' @export

print.causalmodel <- function(x, omit_cf_constraints = FALSE, omit_obs_constraints = FALSE, ...) {
    
    
    lkey <- letters[1:length(x$data$prob.form$out)]
    rkey <- letters[(length(x$data$prob.form$out) + 1):(length(x$data$prob.form$out) + 
                                                            length(x$data$prob.form$cond))]
    
    if(length(x$data$prob.form$cond) == 0) rkey <- NULL
    
    sampparm <- paste0("p", paste(lkey, collapse = ""), "_", 
                       paste(rkey, collapse = ""))
    
    probstate <- paste0("P(", paste(paste0(x$data$prob.form$out, " = ", lkey), collapse = ", "), " | ", 
                        paste0(x$data$prob.form$cond, " = ", rkey, collapse = ", "), ")")
    
    if(length(x$data$prob.form$cond) == 0) {
        probstate <- paste0("P(", paste(paste0(x$data$prob.form$out, " = ", lkey), collapse = ", "), ")")
    }
    
    variabletext <- sprintf("This is a causal model represented by the set of response functions in the %s$data$response_functions element, where it is assumed that we observe parameters (observables) of the form %s, which represents the probability %s. To sample a distribution from this model, use sample_distribution(%s).", deparse1(substitute(x)),
                            sampparm, probstate, deparse1(substitute(x)))
    
    constrainttext <- sprintf("The observables relate to %s unobservable variables which represent probabilities of particular response functions. The constraints are in the following equations and are available in numeric matrix form in the %s$counterfactual_constraints$numeric$R element.", length(x$data$variables), deparse1(substitute(x)))
    
    cat(variabletext, constrainttext, sep = "\n")
    if(!omit_cf_constraints) {
        cat(x$counterfactual_constraints$character, sep = "\n")
    }
    
    observabletext <- sprintf("The causal model implies the following observable constraints in addition to the standard probabilistic constraints, which are available in numeric form in the %s$observable_constraints$numeric element. To test whether these constraints are violated, use check_constraints_violated(%s)", deparse1(substitute(x)), deparse1(substitute(x)))
    
    cat(observabletext, sep = "\n")
    if(!omit_obs_constraints) {
        if(length(x$observable_constraints$character) == 0) {
            cat("No observable constraints other than the probabilistic constraints. ")
        } else {
            cat(x$observable_constraints$character, sep = "\n")
        }
    }
    
    if(!x$counterfactual_constraints$linear.if.true) {
        warning("Causal model does not imply linear constraints on observables. Proceed with caution")
    }
    
    cat("To compute bounds, specify an effect of interest, and see the compute_bounds function.")
    invisible(x)
    
}