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
#' A helper function for \code{\link{evaluate_objective}}.
#' @param numbers1,numbers2 Two numeric vectors of the same length.
#' @return A string consisting of the value of the scalar product of \code{numbers1} and \code{numbers2}.
constant_term <- function(numbers1, numbers2) {
    as.character(sum(numbers1*numbers2))
}

#' Compute the product of a single numeric scalar and a single string
#' 
#' A helper function for \code{\link{linear_expression}}.
#' @param number A numeric vector of length 1.
#' @param string A character vector of length 1.
#' @return A string consisting of the concatenation of \code{number} and \code{string}, including its sign.
linear_term <- function(number, string) {
    if (number == 0) return("")
    if (number == 1) return(paste0(" + ", string))
    if (number == -1) return(paste0(" - ", string))
    if (number > 0) return(paste0(" + ", number, string))
    return(paste0(" - ", abs(number), string))
}

#' Compute the scalar product of a vector of numbers and a vector of strings
#' 
#' A helper function for \code{\link{evaluate_objective}}.
#' @param numbers A numeric vector.
#' @param strings A character vector of the same length as \code{numbers}.
#' @return A string consisting of the corresponding linear combination, including the sign of its first term.
linear_expression <- function(numbers, strings) {
    paste0(mapply(linear_term, numbers, strings), collapse = "")
}

#' Compute the scalar product of a vector of numbers and a vector of both numbers and strings
#' 
#' A helper function for \code{\link{opt_effect}}.
#' @param c1_num A numeric column matrix.
#' @param p A character vector.
#' @param y A numeric vector whose length is the sum of the lengths of \code{c1_num} and \code{p}.
#' @return A string consisting of an affine expression in \code{p} corresponding to the scalar product of \code{c(c1_num, p)} with \code{y}.
evaluate_objective <- function(c1_num, p, y) {
    m1 <- nrow(c1_num)
    m <- length(y)
    number_indices <- 1:m1 # the indices of the numeric entries of c1
    parameter_indices <- (m1+1):m # the indices of the character entries of c1
    y1 <- y[number_indices]
    y2 <- y[parameter_indices]
    const_term <- constant_term(numbers1 = c1_num, numbers2 = y1)
    lin_expr <- linear_expression(numbers = y2, strings = p)
    aff_expr <- paste0(as.character(const_term), lin_expr)
    # trim the initial part of aff_expr if needed
    if (const_term == "0" && lin_expr != "") {
        sign_of_first_lin_term <- substr(lin_expr, start = 2, stop = 2)
        aff_expr <- substr(aff_expr, start = 5, stop = nchar(aff_expr)) # discard prefix "0 + " or "0 - "
        if (sign_of_first_lin_term == "-") aff_expr <- paste0("-", aff_expr)
    }
    aff_expr <- paste0("  ", aff_expr) # indent each line/element slightly
    return(aff_expr)
}
