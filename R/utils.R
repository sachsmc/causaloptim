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

#' Compute the scalar product of two numeric vectors of the same length
#' 
#' A helper function for \code{\link{opt_effect}}, where it is used to compute a part of a bound.
#' @param numbers1 A numeric vector of finite length.
#' @param numbers2 A numeric vector of the same length as \code{numbers1}.
#' @return A string consisting of the numeric scalar (in decimal form) product of the two numeric vectors \code{numbers1} and \code{numbers2}.
# @export
# @examples
# constant_term(c(1,2,3),c(4,5,6)) # returns the string "32"
constant_term <- function(numbers1, numbers2) {
    if (is.null(numbers1) || is.null(numbers2)) stop("A null argument was given to the constant_term function.") # not really needed
    if (!is.numeric(numbers1) || !is.numeric(numbers2)) stop("Incorrect type in argument. Numeric vector required.") # not really needed
    if (length(numbers1) != length(numbers2)) stop("Dimension mismatch of arguments.") # not really needed
    as.character(sum(numbers1*numbers2))
}

#' Compute the product of a single numeric scalar and a single string
#' 
#' A helper function for \code{\link{opt_effect}}, where it is used to compute a part of a bound.
#' @param number A numeric vector of length 1.
#' @param string A character vector of length 1.
#' @return A string consisting of the concatenation of the decimal representation of the scalar \code{number} and the variable-name \code{string}, with its sign explicitly prefixed surrounded by single white-space characters.
# @export
# @examples
# linear_term(3.14,"x") # returns the string " + 3.14x"
linear_term <- function(number, string) {
    if (is.null(number) || is.null(string)) stop("A null argument was given to the linear_term function.") # not really needed
    if (!is.numeric(number) || !is.character(string)) stop("Incorrect type in argument to linear_term.") # not really needed
    if (number == 0) return("")
    if (number == 1) return(paste0(" + ", string))
    if (number == -1) return(paste0(" - ", string))
    if (number > 0) return(paste0(" + ", number, string))
    return(paste0(" - ", abs(number), string))
}

#' Compute the scalar product of a vector of numbers and a vector of strings
#' 
#' A helper function for \code{\link{opt_effect}}, where it is used to compute a part of a bound.
#' @param numbers A numeric vector of finite length.
#' @param strings A character vector of the same length as \code{numbers}.
#' @return A string consisting of the corresponding linear combination as an expression with an explicit sign even for the first term even if it is positive.
# @export
# @examples
# linear_expression(c(1,2,3),c("x","y","z")) # returns the string " + x + 2y + 3z"
linear_expression <- function(numbers, strings) {
    if (is.null(numbers) || is.null(strings)) stop("A null argument was given to the linear_expression function.") # not really needed
    if (length(numbers) != length(strings)) stop("Dimension mismatch.") # not really needed
    if (!is.numeric(numbers) || !is.character(strings)) stop("Incorrect type in argument to linear_expression.") # not really needed
    paste0(mapply(linear_term, numbers, strings), collapse = "")
}

#' Compute the scalar product of a vector of numbers and a vector of both numbers and strings
#' 
#' A helper function for \code{\link{opt_effect}}, where it is used to compute a part of a bound.
#' Evaluate the linear combination t(c1) %*% y, where 
#' c1 == rbind(c1_num, p) (an m\eqn{\times}1 column matrix), is the gradient vector where 
#' c1_num = rbind(b_l, 1) (an (m_e + 1)\eqn{\times}1 column vector of type "numeric"), and 
#' p is the vector of parameters (an (m_e + 1)\eqn{\times}1 column vector of type "character"), and 
#' y is a variable (an m\eqn{\times}1 column vector) of of length m.
#' Returns the result as an affine expression in/of the parameters of/in the vector \code{p}.
#' @param c1_num a numeric column matrix
#' @param p a character vector
#' @param y a numeric vector whose length is the sum of the lengths of the vectors c1_num and p
#' @return a string consisting of an affine expression in p corresponding to the scalar multiplication of the vectors c(c1_num,p) and y
# @export
# @examples
# evaluate_objective(matrix(c(1,2)),c("a","b","c"),c(3,4,5,6,7)) # returns the string "  11 + 5a + 6b + 7c"
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
