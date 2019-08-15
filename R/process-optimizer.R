#' Run the Balke optimizer
#' 
#' @param obj Object as returned by \link{analyze_graph}
#' 
#' @return An object of class "balkebound" that contains the bounds and logs as character strings
#' 


optimize_effect <- function(obj) {
    
    tbl.file <- tempfile(pattern = c("max", "min"))
    cat("VARIABLES\n", file = tbl.file[1])
    cat(obj$variables, file = tbl.file[1], append = TRUE, sep = "\n")
    cat("\nPARAMETERS\n", file = tbl.file[1], append = TRUE)
    cat(obj$parameters, file = tbl.file[1], append = TRUE, sep = "\n")
    cat("\nCONSTRAINTS\n", file = tbl.file[1], append = TRUE)
    cat(sapply(obj$constraints, shortentxt), file= tbl.file[1], append = TRUE, sep = "\n")
    cat("\nMAXIMIZE\n", file = tbl.file[1], append = TRUE)
    cat("\nOBJECTIVE\n", file =tbl.file[1], append = TRUE)
    cat(shortentxt(obj$objective), file = tbl.file[1], append = TRUE)
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
    cat(sapply(obj$constraints, shortentxt), file= tbl.file[2], append = TRUE, sep = "\n")
    cat("\nMINIMIZE\n", file = tbl.file[2], append = TRUE)
    cat("\nOBJECTIVE\n", file =tbl.file[2], append = TRUE)
    cat(shortentxt(obj$objective), file = tbl.file[2], append = TRUE)
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
#' @param bounds The bounds element as returned by \link{optimize_effect}
#' @param parameters Character vector defining parameters, as returned by \link{analyze_graph}
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

#' Shorten strings to 80 characters wide

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


#' Simulate bounds
#' 
#' Run a simple simulation based on the bounds. For each simulation, sample the set of counterfactual probabilities 
#' from a uniform distribution, translate into a multinomial distribution, and then compute the objective and the 
#' bounds in terms of the observable variables. 
#' 
#' @param obj Object as returned by \link{analyze_graph}
#' @param bounds Object as returned by \link{optimize_effect}
#' @param nsim Number of simulation replicates
#' 
#' @return A data frame with columns: objective, bound.lower, bound.upper

simulate_bounds <- function(obj, bounds, nsim = 1e3) {
    
    
    f.bounds <- interpret_bounds(bounds$bounds, obj$parameters)
    result <- matrix(NA, ncol = 3, nrow = nsim)
    
    for(i in 1:nsim) {
        
        sim.qs <- runif(length(obj$variables))
        sim.qs <- sim.qs / sum(sim.qs)
        
        names(sim.qs) <- obj$variables
        objective <- eval(parse(text = obj$objective), envir = as.list(sim.qs))
        
        inenv <- new.env()
        for(j in 1:length(sim.qs)) {
            
            assign(names(sim.qs)[j], sim.qs[j], inenv)
            
        }
        res <- lapply(as.list(obj$constraints[-1]), function(x){
            x1 <- strsplit(x, " = ")[[1]]
            x0 <- paste(x1[2], " = ", x1[1])
            eval(parse(text = x0), envir = inenv)
        })
        
        params <- lapply(obj$parameters, function(x) get(x, envir = inenv))
        names(params) <- obj$parameters
        
        bees <- sort(do.call(f.bounds, params))
        result[i, ] <- c(objective, bees)
        
        if(objective < bees[1] | objective > bees[2]) {
            stop("Objective outside bounds, optimizer failed!")
            break
        }
        
    }
    colnames(result) <- c("objective", "bound.lower", "bound.upper")
    as.data.frame(result)
    
    
}
