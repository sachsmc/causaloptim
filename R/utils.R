#' Recursive function to get the last name in a list
#' 
#' @param x a list
#' @param name name of the top element of the list
#' @return The name of the deepest nested list element

btm_var <- function(x, name = NULL) {
    
    if(!is.list(x)) {
        return(name)
    } else {
        
        unlist(lapply(1:length(x), function(i) {
            btm_var(x[[i]], names(x)[[i]])
        }))
    }
    
}

#' Recursive function to translate an effect list to a path sequence
#' 
#' @param x A list of vars as returned by parse_effect
#' @param name The name of the outcome variable
#' @return a list of characters describing the path sequence

list_to_path <- function(x, name = NULL) {
    
    if(!is.list(x)) {
        names(x) <- name
        return(x)
    } else {
        
        lapply(1:length(x), function(i) {
            unlist(list_to_path(x[[i]], paste(names(x)[[i]], "->", name)))
        })
    }
    
}

constant_term <- function(numbers1, numbers2) {
    if (is.null(numbers1) || is.null(numbers2)) stop("A null argument was given to the constant_term function.") # not really needed
    if (!is.numeric(numbers1) || !is.numeric(numbers2)) stop("Incorrect type in argument. Numeric vector required.") # not really needed
    if (length(numbers1) != length(numbers2)) stop("Dimension mismatch of arguments.") # not really needed
    as.character(sum(numbers1*numbers2))
}

linear_term <- function(number, string) {
    if (is.null(number) || is.null(string)) stop("A null argument was given to the linear_term function.") # not really needed
    if (!is.numeric(number) || !is.character(string)) stop("Incorrect type in argument to linear_term.") # not really needed
    if (number == 0) return("")
    if (number == 1) return(paste0(" + ", string))
    if (number == -1) return(paste0(" - ", string))
    if (number > 0) return(paste0(" + ", number, string))
    return(paste0(" - ", abs(number), string))
}

linear_expression <- function(numbers, strings) {
    if (is.null(numbers) || is.null(strings)) stop("A null argument was given to the linear_expression function.") # not really needed
    if (length(numbers) != length(strings)) stop("Dimension mismatch.") # not really needed
    if (!is.numeric(numbers) || !is.character(strings)) stop("Incorrect type in argument to linear_expression.") # not really needed
    paste0(mapply(linear_term, numbers, strings), collapse = "")
}

evaluate_objective <- function(c1_num, p, y) {
    if (is.null(c1_num) || is.null(p) || is.null(y)) stop("A null argument was given to the evaluate_objective function.") # not really needed
    if (ncol(c1_num) != 1) stop("Argument has incorrect dimension.") # not really needed
    m1 <- nrow(c1_num)
    m2 <- length(p)
    m <- length(y)
    if (m1 + m2 != m) stop("Dimension mismatch in dual affine objective function.") # not really needed
    if (!is.numeric(c1_num) || !is.character(p) || !is.numeric(y)) stop("Incorrect type in argument to evaluate_objective.") # not really needed
    number_indices <- 1:m1 # the indices of the numeric entries of the gradient vector c1
    parameter_indices <- (m1+1):m # the indices of the string entries of the gradient vector c1
    y1 <- y[number_indices] # , drop = FALSE maybe?
    y2 <- y[parameter_indices] # , drop = FALSE maybe?
    if (length(y1) != m1 || length(y2) != m2 || !all(c(y1,y2) == y)) stop("Index mismatch in dual affine objective function.") # not really needed
    const_term <- constant_term(numbers1 = c1_num, numbers2 = y1)
    lin_expr <- linear_expression(numbers = y2, strings = p)
    aff_expr <- paste0(as.character(const_term), lin_expr)
    # trim aff_expr if needed
    if (const_term == "0" && lin_expr != "") {
        sign_of_first_lin_term <- substr(lin_expr, start = 2, stop = 2)
        aff_expr <- substr(aff_expr, start = 5, stop = nchar(aff_expr)) # discard prefix "0 + " or "0 - "
        if (sign_of_first_lin_term == "-") aff_expr <- paste0("-", aff_expr)
    }
    aff_expr <- paste0("  ", aff_expr)
    return(aff_expr)
}
