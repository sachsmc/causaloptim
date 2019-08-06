#' Run the Balke optimizer and return bounds as a function
#' 
#' @export

optimize_effect <- function(obj) {
    
    tbl.file <- tempfile(pattern = c("max", "min"))
    cat("VARIABLES\n", file = tbl.file[1])
    cat(obj$variables, file = tbl.file[1], append = TRUE, sep = "\n")
    cat("\nPARAMETERS\n", file = tbl.file[1], append = TRUE)
    cat(obj$parameters, file = tbl.file[1], append = TRUE, sep = "\n")
    cat("\nCONSTRAINTS\n", file = tbl.file[1], append = TRUE)
    cat(obj$constraints, file= tbl.file[1], append = TRUE, sep = "\n")
    cat("\nMAXIMIZE\n", file = tbl.file[1], append = TRUE)
    cat("\nOBJECTIVE\n", file =tbl.file[1], append = TRUE)
    cat(obj$objective, file = tbl.file[1], append = TRUE)
    cat("\nEND\n", file = tbl.file[1], append = TRUE)
    
    
    test <- COptimization_$new()
    fileParseSuccess <- test$ParseFileWrap(tbl.file[1])
    stopifnot(fileParseSuccess == 0)
    
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
    cat(obj$constraints, file= tbl.file[2], append = TRUE, sep = "\n")
    cat("\nMINIMIZE\n", file = tbl.file[2], append = TRUE)
    cat("\nOBJECTIVE\n", file =tbl.file[2], append = TRUE)
    cat(obj$objective, file = tbl.file[2], append = TRUE)
    cat("\nEND\n", file = tbl.file[2], append = TRUE)
    
    
    test2 <- COptimization_$new()
    file2ParseSuccess <- test2$ParseFileWrap(tbl.file[2])
    stopifnot(file2ParseSuccess == 0)
    
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


#' Convert bounds string to a function
#' 
#' @export

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
    body(f) <- parse(text = paste0("c(lower = ", bcalls[2], ", upper = ", bcalls[1], ") \n"))
    environment(f) <- parent.frame()
    
    f
    
}


