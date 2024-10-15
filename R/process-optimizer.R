#' @export

print.balkebound <- function(x, ...){
    
    cat("lower bound = ", x$bounds["lower"], ...)
    cat("----------------------------------------\n")
    cat("upper bound = ", x$bounds["upper"], ...)
    cat("\nA function to compute the bounds for a given set of probabilities is available in the x$bounds_function element.\n")
    
    
}

#' Run the optimizer to obtain symbolic bounds
#' 
#' Given an object with the linear programming problem set up, compute the bounds
#' using rcdd. Bounds are returned as text but can
#' be converted to R functions using \link{interpret_bounds}, or latex code using
#' \link{latex_bounds}.
#' 
#' @param obj Object as returned by \link{analyze_graph} or \link{create_linearcausalproblem}
#' 
#' @return An object of class "balkebound" that is a list that contains the bounds and logs as character strings, and a function to compute the bounds
#' @export
#' @aliases optimize_effect
#' @examples 
#' b <- initialize_graph(graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y))
#' obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#' optimize_effect_2(obj)
optimize_effect_2 <- optimize_effect <- function(obj) {
    lower_bound <- opt_effect(opt = "min", obj = obj)
    upper_bound <- opt_effect(opt = "max", obj = obj)
    bounds <- c(lower = lower_bound$expr, upper = upper_bound$expr)
    vreps_of_duals <- list(lower = lower_bound$dual_vrep, upper = upper_bound$dual_vrep)
    structure(list(bounds = bounds, logs = vreps_of_duals, 
                   bounds_function = interpret_bounds(bounds, obj$parameters)), class = "balkebound")
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
#' bounds <- optimize_effect_2(obj)
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
#' @noRd

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


