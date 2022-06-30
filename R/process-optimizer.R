#' Run the Balke optimizer
#' 
#' Given a object with the linear programming problem set up, compute the bounds
#' using the c++ code developed by Alex Balke. Bounds are returned as text but can
#' be converted to R functions using \link{interpret_bounds}, or latex code using
#' \link{latex_bounds}.
#' 
#' @param obj Object as returned by \link{analyze_graph}
#' 
#' @return An object of class "balkebound" that contains the bounds and logs as character strings
#' 
#' @export
#' @examples 
#' b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(2,2,2)
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
#' obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#' optimize_effect(obj)


optimize_effect <- function(obj) {
    
    special.terms <- grepl("p(.*) = 0", obj$constraints)
    
    red.sets <- const.to.sets(obj$constraints[!special.terms], obj$objective.nonreduced)
    objective.fin <- paste(red.sets$objective.terms[[1]], collapse = " + ")
    
    if(!is.null(obj$parsed.query$oper) & length(obj$parsed.query$oper) > 0 & 
       length(red.sets$objective.terms) > 1) {
        
        for(opp in 1:length(obj$parsed.query$oper)) {
            
            thiscol <- ifelse(obj$parsed.query$oper[[opp]] == "-", " - ", " + ")
            objective.fin <- paste(objective.fin, obj$parsed.query$oper[[opp]], 
                                   paste(red.sets$objective.terms[[opp + 1]], collapse = thiscol))
            
        }
        
    }
    
    obj$variables <- red.sets$variables
    obj$constraints <- c(red.sets$constr, obj$constraints[special.terms])
    obj$objective <- objective.fin
    
    
    tbl.file <- tempfile(pattern = c("max", "min"))
    cat("VARIABLES\n", file = tbl.file[1])
    cat(obj$variables, file = tbl.file[1], append = TRUE, sep = "\n")
    cat("\nPARAMETERS\n", file = tbl.file[1], append = TRUE)
    cat(obj$parameters, file = tbl.file[1], append = TRUE, sep = "\n")
    cat("\nCONSTRAINTS\n", file = tbl.file[1], append = TRUE)
    cat(sapply(obj$constraints, shortentxt), file= tbl.file[1], append = TRUE, sep = "\n")
    cat("\nMAXIMIZE\n", file = tbl.file[1], append = TRUE)
    cat("\nOBJECTIVE\n", file =tbl.file[1], append = TRUE)
    cat(shortentxt(obj$objective), file = tbl.file[1], append = TRUE)
    cat("\nEND\n", file = tbl.file[1], append = TRUE)
    
    
    test <- COptimization_$new()
    fileParseSuccess <- test$ParseFileWrap(tbl.file[1])
    stopifnot(fileParseSuccess == 1)
    
    test$CategorizeConstraints()
    test$GaussianElimination()
    
    log1.min <- test$EnumerateVertices()
    
    res.upperbound <- test$OutputOptimum()
    disp.min <- test$Display()
    
    cat("VARIABLES\n", file = tbl.file[2])
    cat(obj$variables, file = tbl.file[2], append = TRUE, sep = "\n")
    cat("\nPARAMETERS\n", file = tbl.file[2], append = TRUE)
    cat(obj$parameters, file = tbl.file[2], append = TRUE, sep = "\n")
    cat("\nCONSTRAINTS\n", file = tbl.file[2], append = TRUE)
    cat(sapply(obj$constraints, shortentxt), file= tbl.file[2], append = TRUE, sep = "\n")
    cat("\nMINIMIZE\n", file = tbl.file[2], append = TRUE)
    cat("\nOBJECTIVE\n", file =tbl.file[2], append = TRUE)
    cat(shortentxt(obj$objective), file = tbl.file[2], append = TRUE)
    cat("\nEND\n", file = tbl.file[2], append = TRUE)
    
    
    test2 <- COptimization_$new()
    file2ParseSuccess <- test2$ParseFileWrap(tbl.file[2])
    stopifnot(file2ParseSuccess == 1)
    
    test2$CategorizeConstraints()
    test2$GaussianElimination()
    
    
    log1.max <- test2$EnumerateVertices()
    res.lowerbound <- test2$OutputOptimum()
    disp.max <- test2$Display()
    
    
    bounds <- c(lower = res.lowerbound, upper = res.upperbound)
    logs <- list(lower = c(log = log1.min, display = disp.min), upper = c(log = log1.max, display = disp.max))
    
    structure(list(bounds = bounds, logs = logs), class = "balkebound")
    
    
}

#' @export

print.balkebound <- function(x, ...){
    
    cat("lower bound = ", x$bounds["lower"], ...)
    cat("----------------------------------------\n")
    cat("upper bound = ", x$bounds["upper"], ...)
    
    
}

#' Run the optimizer
#' 
#' Given an object with the linear programming problem set up, compute the bounds
#' using rcdd. Bounds are returned as text but can
#' be converted to R functions using \link{interpret_bounds}, or latex code using
#' \link{latex_bounds}.
#' 
#' @param obj Object as returned by \link{analyze_graph}
#' 
#' @return An object of class "balkebound" that contains the bounds and logs as character strings
#' 
#' @export
#' @examples 
#' b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(2,2,2)
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
#' obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#' optimize_effect_2(obj)
optimize_effect_2 <- function(obj) {
    lower_bound <- opt_effect(opt = "min", obj = obj)
    upper_bound <- opt_effect(opt = "max", obj = obj)
    bounds <- c(lower = lower_bound$expr, upper = upper_bound$expr)
    vreps_of_duals <- list(lower = lower_bound$dual_vrep, upper = upper_bound$dual_vrep)
    structure(list(bounds = bounds, logs = vreps_of_duals), class = "balkebound")
}

#' Compute a bound on the average causal effect
#' 
#' This helper function does the heavy lifting for \code{\link{optimize_effect_2}}.
#' For a given casual query, it computes either a lower or an upper bound on the corresponding causal effect.
#' @param opt A string. Either \code{"min"} or \code{"max"} for a lower or an upper bound, respectively.
#' @param obj An object as returned by the function \code{\link{analyze_graph}}. Contains the casual query to be estimated.
#' @return An object of class \code{optbound}; a list with the following named components: 
#' \itemize{
#'   \item \code{expr} is the \emph{main} output; an expression of the bound as a print-friendly string,
#'   \item \code{type} is either \code{"lower"} or \code{"upper"} according to the type of the bound,
#'   \item \code{dual_vertices} is a numeric matrix whose rows are the vertices of the convex polytope of the dual LP,
#'   \item \code{dual_vrep} is a V-representation of the dual convex polytope, including some extra data.
#' }
opt_effect <- function(opt, obj) {
    # The Primal LP
    c0 <- obj$c0
    n <- nrow(c0)
    A_e <- obj$R
    p <- obj$parameters
    m_e <- nrow(A_e)
    A_l <- obj$iqR
    if (is.null(A_l)) A_l <- matrix(data = 0, nrow = 0, ncol = n)
    b_l <- obj$iqb
    if (is.null(b_l)) b_l <- matrix(data = 0, nrow = 0, ncol = 1)
    if (!is.numeric(A_l) || !is.numeric(b_l)) stop("Inequality entries must be numeric.")
    if (ncol(A_l) != n) stop("Dimension mismatch of inequality constaints.")
    m_l <- nrow(A_l)
    if (nrow(b_l) != m_l) stop("Dimension mismatch of inequality constaints.")
    m <- m_l + m_e
    # The Dual LP
    a1 <- rbind(cbind(t(A_l), t(A_e)),
                cbind(diag(x = 1, nrow = m_l, ncol = m_l), matrix(data = 0, nrow = m_l, ncol = m_e)))
    b1 <- rbind(c0,
                matrix(data = 0, nrow = m_l, ncol = 1))
    if (opt == "max") {
        a1 <- -a1
        b1 <- -b1
    }
    hrep <- makeH(a1 = a1, b1 = b1)
    vrep <- scdd(input = hrep, adjacency = TRUE, inputadjacency = TRUE, incidence = TRUE, inputincidence = TRUE)
    matrix_of_vrep <- vrep$output
    indices_of_vertices <- matrix_of_vrep[ , 1] == 0 & matrix_of_vrep[ , 2] == 1
    vertices <- matrix_of_vrep[indices_of_vertices, -c(1, 2), drop = FALSE] # the rows of this matrix are the vertices of the convex polytope
    # The Bound
    c1_num <- rbind(b_l, 1)
    expressions <- apply(vertices, 1, function(y) evaluate_objective(c1_num = c1_num, p = p, y = y))
    elements <- paste(expressions, sep = ",", collapse = ",\n")
    opt_bound <- paste0(if (opt == "min") "\nMAX {\n" else "\nMIN {\n", elements, "\n}\n")
    opt_bound <- structure(list(expr = opt_bound,
                                type = if (opt == "min") "lower" else "upper",
                                dual_vertices = vertices,
                                dual_vrep = vrep),
                           class = "optbound")
    return(opt_bound)
}


#' Convert bounds string to a function
#' 
#' @param bounds The bounds element as returned by \link{optimize_effect}
#' @param parameters Character vector defining parameters, as returned by \link{analyze_graph}
#' 
#' @return A function that takes arguments for the parameters, i.e., the observed probabilities and returns a vector of length 2: the lower bound and the upper bound. 
#' 
#' @export
#' @examples 
#' b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(2,2,2)
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
#' obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#' bounds <- optimize_effect(obj)
#' bounds_func <- interpret_bounds(bounds$bounds, obj$parameters)
#' bounds_func(.1, .1, .4, .3)
#' # vectorized
#' do.call(bounds_func, lapply(1:4, function(i) runif(5)))

interpret_bounds <- function(bounds, parameters) {
    
    bcalls <- c("", "")
    for(i in 1:2) {
        bound <- bounds[i]
        ilines <- strsplit(bound, "(\n|,\n)")[[1]]
        ilines <- ilines[ilines != ""]
        
        if(grepl("MAX", ilines[1])) {
            type <- "pmax"
        } else type <- "pmin"
        
        elems <- ilines[c(-1, -length(ilines))]
        intotype <- gsub("([0-9])(| )(p)", "\\1 * p", elems, fixed = FALSE)
        
        bcalls[i] <- paste0(type, "(", paste(intotype, collapse = ", \n"), ")")
    }
    
    args <- vector(mode = "list", length = length(parameters))
    names(args) <- parameters
    
    bod <- parse(text = paste0("lb <- ", bcalls[1], "\n", 
                        "ub <- ", bcalls[2], "\n", 
                        "if(any(ub < lb)) {\n warning('Invalid bounds! Data probably does not satisfy the assumptions in the DAG!')\n } \n",
                        "data.frame(lower = lb, upper = ub) \n"))
    
    f <- function() {}
    formals(f) <- as.pairlist(args)
    body(f) <- as.call(c(as.name("{"), bod))
    environment(f) <- parent.frame()
    
    f
    
}

#' Shorten strings to 80 characters wide
#' 
#' @param x String
#' @return A string with line breaks to keep the width less than 80 characters

shortentxt <- function(x) {
    
    vex <- strsplit(x, " ")[[1]]
    ccount <- 0
    res <- ""
    vin <- vex
    while(length(vin) > 0) {
        
        res <- paste(res, vin[1])
        ccount <- ccount + nchar(vin[1])
        
        if(ccount > 80 & vin[1] %in% c("+", "-")){ 
            res <- paste(res, " \\ \n")
            ccount <- 0
        }
        vin <- vin[-1]
    }
    
    res
    
    
}


#' Simulate bounds
#' 
#' Run a simple simulation based on the bounds. For each simulation, sample the set of counterfactual probabilities 
#' from a uniform distribution, translate into a multinomial distribution, and then compute the objective and the 
#' bounds in terms of the observable variables. 
#' 
#' @param obj Object as returned by \link{analyze_graph}
#' @param bounds Object as returned by \link{optimize_effect}
#' @param nsim Number of simulation replicates
#' 
#' @return A data frame with columns: objective, bound.lower, bound.upper
#' 
#' @export
#' @examples
#' b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(2,2,2)
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
#' obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#' bounds <- optimize_effect(obj)
#' simulate_bounds(obj, bounds, nsim = 5)

simulate_bounds <- function(obj, bounds, nsim = 1e3) {
    
    
    f.bounds <- interpret_bounds(bounds$bounds, obj$parameters)
    result <- matrix(NA, ncol = 3, nrow = nsim)
    
    for(i in 1:nsim) {
        
        sim.qs <- runif(length(obj$variables))
        sim.qs <- sim.qs / sum(sim.qs)
        
        names(sim.qs) <- obj$variables
        objective <- eval(parse(text = obj$objective), envir = as.list(sim.qs))
        
        inenv <- new.env()
        for(j in 1:length(sim.qs)) {
            
            assign(names(sim.qs)[j], sim.qs[j], inenv)
            
        }
        res <- lapply(as.list(obj$constraints[-1]), function(x){
            x1 <- strsplit(x, " = ")[[1]]
            x0 <- paste(x1[1], " = ", x1[2])
            eval(parse(text = x0), envir = inenv)
        })
        
        params <- lapply(obj$parameters, function(x) get(x, envir = inenv))
        names(params) <- obj$parameters
        
        bees <- c(unlist(do.call(f.bounds, params)))
        result[i, ] <- c(objective, bees)
        
        if(objective < bees[1] | objective > bees[2]) {
            stop("Objective outside bounds, optimizer failed!")
            break
        }
        
    }
    colnames(result) <- c("objective", "bound.lower", "bound.upper")
    as.data.frame(result)
    
    
}


#' @useDynLib causaloptim
#' @import Rcpp methods
#' @importFrom rcdd makeH scdd
NULL