#' Convert an igraph object to the response functional
#' 
#' The graph must contain edge attributes named "leftside" and "lrconnect"
#' that takes values 0 and 1. Only one edge may have a value 1 for lrconnect. 
#' The shiny app returns a graph in this format. 
#' 
#' @export

igraph_to_response_function <- function(graph) {
    
    leftind <- edge_attr(graph)$leftside
    lrconnect <- edge_attr(graph)$lrconnect
    if(is.null(lrconnect)) stop("Graph object must have an edge attribute named 'leftside'")
    if(sum(lrconnect) > 1) stop("Only one edge must originate from the leftside")
    
    cond.vars <- V(graph)[leftind]
    right.vars <- V(graph)[!leftind]
    
    do.call(expand.grid, lapply(1:length(cond.vars), function(i) c("0", "1")))
    
}