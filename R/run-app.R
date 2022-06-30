#' Shiny interface to specify network structure and compute bounds
#' 
#' This launches the Shiny interface in the system's default web browser. The 
#' results of the computation will be displayed in the browser, but they can 
#' also be returned to the R session by assigning the result of the function call
#' to an object. See below for information on what is returned. 
#' 
#' @return If the button "Exit and return graph object" is clicked, then only the 
#'     graph is returned as an \link[igraph]{aaa-igraph-package} object. 
#'     
#'     If the bounds are computed and the button "Exit and return objects to R" is clicked, 
#'     then a list is returned with the following elements: 
#'     
#'     \describe{
#'        \item{graphres}{The graph as drawn and interpreted, an \link[igraph]{aaa-igraph-package} object.}
#'        \item{obj}{The objective and all necessary supporting information. This object is 
#'        documented in \link{analyze_graph}. This can be passed directly to \link{optimize_effect_2}.}
#'        \item{bounds.obs}{Object of class 'balkebound' as returned by \link{optimize_effect_2}.}
#'        \item{constraints}{Character vector of the specified constraints. NULL if no constraints.}
#'        \item{effect}{Text describing the causal effect of interest.}
#'        \item{boundsFunction}{Function that takes parameters (observed probabilities) as arguments, and returns a vector of length 2 for the lower and upper bounds.}
#'     
#'     }
#' 
#' @export
#' 
specify_graph <- function() {
    
    appDir <- system.file("shiny", "interface", package = "causaloptim")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `causaloptim`.", call. = FALSE)
    }
    
    message("Objects analyzed in the Shiny app can be returned to the R environment, but you must assign them to an object. If you would like to save the results, quit and make sure to assign the result of this function to an object: results <- specify_graph()")
    
    shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
    
}