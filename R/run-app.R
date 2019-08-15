#' Shiny interface to specify network structure
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