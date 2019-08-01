#' Shiny interface to specify network structure
#' 
#' @export
#' 
specify_graph <- function() {
    
    appDir <- system.file("shiny", "interface", package = "causaloptim")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `causaloptim`.", call. = FALSE)
    }
    
    shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
    
}