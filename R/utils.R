#' Recursive function to get the last name in a list
#' 
#' @param x a list
#' @param name name of the top element of the list
#' @return The name of the deepest nested list element
#' @examples
#' btm_var(list(W3 = list(W2 = list(W1 = 1))), "Y")

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
#' @examples 
#' 
#' list_to_path(list(W3 = list(W2 = list(W1 = 1))), "Y")

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
