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
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
#' obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#' optimize_effect(obj)


optimize_effect <- function(obj) {
    
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
    
    cat(x$bounds, ...)
    
}

optimize_effect_2 <- function(obj) {
    lower_bound <- opt_effect(opt = "min", obj = obj)
    upper_bound <- opt_effect(opt = "max", obj = obj)
    bounds <- c(lower = lower_bound$expr, upper = upper_bound$expr)
    vreps_of_duals <- list(lower = lower_bound$dual_vrep, upper = upper_bound$dual_vrep)
    structure(list(bounds = bounds, logs = vreps_of_duals), class = "balkebound")
}

opt_effect <- function(opt, obj) {
    
    # The Primal LP
    # opt t(c0) %*% q
    # st  all(A_e %*% q == b_e)
    # &   all(q >= 0)
    # &   all(A_l %*% q <= b_l)
    # where
    # ncol(c0) == nrow(b_e) == ncol(q) == 1
    # nrow(c0) == ncol(A_e) == nrow(q) == n == ncol(A_l)
    # nrow(A_e) == nrow(b_e) == m_e
    # nrow(A_l) == nrow(b_l) == m_l
    c0 <- obj$c0 # the gradient column vector (an n\times1 matrix) of the linear objective function
    n <- nrow(c0) # the dimension/length of the variable q to optimize the objective function over
    A_e <- obj$R # the linear equality constraint coefficient matrix (m_e\times n)
    if (ncol(A_e) != n) stop("Dimension mismatch of equality constraints.") # not really needed
    p <- obj$parameters # a vector of the p-parameters as character strings (a "plain" vector; _not_ a column matrix)
    # b_e <- matrix(data=c("1", # actually b_e is not used anywhere
    #                      p), ncol = 1) # the matrix equality right hand side column vector (an m_e\times1 matrix)
    m_e <- nrow(A_e) # the number of equality constraints
    # if (nrow(b_e) != m_e) stop("Dimension mismatch of equality constaints.") # not really needed
    # User-provided inequality constraint(s)
    A_l <- obj$iqR # a user-provided inequality constraint coefficient matrix
    # A_l <- NULL # only temporary!
    if (is.null(A_l)) A_l <- matrix(data = 0, nrow = 0, ncol = n) # needed?
    b_l <- obj$iqb # a user-provided inequality constraint right hand side vector (as a column matrix?)
    # b_l <- NULL # only temporary!
    if (is.null(b_l)) b_l <- matrix(data = 0, nrow = 0, ncol = 1) # needed?
    if (!is.numeric(A_l) || !is.numeric(b_l)) stop("Inequality entries must be numeric.")
    if (ncol(A_l) != n) stop("Dimension mismatch of inequality constaints.")
    m_l <- nrow(A_l) # the number of inequality constraints
    if (nrow(b_l) != m_l) stop("Dimension mismatch of inequality constaints.")
    m <- m_l + m_e # the total number of linear constraints
    
    # The Dual LP
    # opt' t(c1) %*% y
    # s.t. all(a1 %*% y <= b1)
    # where c1, a1 and b1 are given below and 
    # ncol(c1) == nrow(b1) == ncol(y) == 1
    # nrow(c1) == ncol(a1) == nrow(y) == m
    # nrow(a1) == nrow(b1) == n
    # c1 <- rbind(b_l, # actually c1 is not used anywhere
    #             b_e) # the gradient column vector (an m\times1 matrix) c1 := b of the linear objective function $y \mapsto b^t y$
    # if (nrow(c1) != m) stop("Dimension mismatch in dual objective.") # not really needed
    a1 <- rbind(cbind(t(A_l), t(A_e)),
                cbind(diag(x = 1, nrow = m_l, ncol = m_l), matrix(data = 0, nrow = m_l, ncol = m_e))) # the linear inequality constraint coefficient matrix
    b1 <- rbind(c0,
                matrix(data = 0, nrow = m_l, ncol = 1)) # the matrix inequality constraint right hand side column vector
    # N.b. If we have no user-provided (A_l, b_l) then (c1, a1, b1) reduces to c1 = b_e, a1 = t(A_e) and b1 = c0.
    if (opt == "max") {
        a1 <- -a1
        b1 <- -b1
    }
    hrep <- makeH(a1 = a1, b1 = b1) # the H-representation (half-space description) of the constraint space
    # Enumerate the vertices of the convex polytope (given as the H-representation 'hrep'). The vertices of the convex polytope are its extreme points.
    vrep <- scdd(input = hrep, adjacency = TRUE, inputadjacency = TRUE, incidence = TRUE, inputincidence = TRUE) # the corresponding V-representation of the same m-dimensional convex polytope
    matrix_of_vrep <- vrep$output
    indices_of_vertices <- matrix_of_vrep[ , 1] == 0 & matrix_of_vrep[ , 2] == 1
    vertices <- matrix_of_vrep[indices_of_vertices, -c(1, 2), drop = FALSE] # the rows of this matrix are the vertices of the convex polytope
    if (ncol(vertices) != m) stop("Dimension mismatch in dual constraint space.") # not really needed
    
    # The Bounds
    # K <- nrow(vertices) # the number of vertices of the convex polytope
    # We let {ybar_1,...,ybar_K} be the set of rows of the matrix 'vertices'.
    # The length of each vertex ybar_k is m = m_l + m_e.
    # Extract each vertex ybar as a row vector from the matrix 'vertices' 
    # and plug it into the linear objective function t(c1) %*% t(y) = t(b) %*% t(y), 
    # to get its value as an affine expression in the p's of the column vector c1 = b.
    # indices_of_numbers_in_c1 <- 1:(m_l + 1)
    # indices_of_parameters_in_c1 <- (m_l + 2):m
    c1_num <- rbind(b_l, 1) # the _numeric_ column vector (an (m_l + 1)\times1 matrix) consisting of 
    # the first m_l + 1 entries of the (m\times1) column vector c1, so we have c1 == c(c1_num, p)
    if (nrow(c1_num) != m_l + 1) stop("Wrong dimension of sub-vector.") # not really needed
    if (ncol(c1_num) != 1) stop("Wrong dimension of sub-vector.") # not really needed
    if (!is.numeric(c1_num)) stop("Wrong entry-type of sub-vector.") # not really needed
    expressions <- apply(vertices, 1, function(y) evaluate_objective(c1_num = c1_num, p = p, y = y))
    elements <- paste(expressions, sep = ",", collapse = ",\n")
    opt_bound <- paste0(if (opt == "min") "min-bound: MAX {\n" else "max-bound: MIN {\n", elements, "\n}\n")
    opt_bound <- structure(list(expr = opt_bound, # an expression of the bound as a print-friendly string
                                type = if (opt == "min") "lower" else "upper", # the kind of extremum/optimum of the bound
                                dual_vertices = vertices, # a matrix whose rows are the vertices (in lexicographic order) of the dual constraint set
                                dual_vrep = vrep), # a V-description (including some extra information) of the dual constraint set
                           class = "optbound")
    return(opt_bound)
}

#' @export

print.optbound <- function(x, ...){
    
    cat(x$expr, ...)
    
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
        ilines <- strsplit(bound, "\n")[[1]]
        ilines <- ilines[ilines != ""]
        
        if(ilines[1] == "MAX {") {
            type <- "pmax"
        } else type <- "pmin"
        
        elems <- ilines[c(-1, -length(ilines))]
        intotype <- gsub("([0-9])( )(p)", "\\1 * p", elems, fixed = FALSE)
        
        bcalls[i] <- paste0(type, "(", paste(intotype, collapse = ", \n"), ")")
    }
    
    args <- vector(mode = "list", length = length(parameters))
    names(args) <- parameters
    
    f <- function() {}
    formals(f) <- as.pairlist(args)
    body(f) <- parse(text = paste0("data.frame(lower = ", bcalls[1], ", upper = ", bcalls[2], ") \n"))
    environment(f) <- parent.frame()
    
    f
    
}

#' Shorten strings to 80 characters wide
#' 
#' @param x String

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