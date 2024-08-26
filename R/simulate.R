
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
#' 
#' @export
#' @examples
#' b <- initialize_graph(graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y))
#' obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#' bounds <- optimize_effect_2(obj)
#' simulate_bounds(obj, bounds, nsim = 5)

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
            x0 <- paste(x1[1], " = ", x1[2])
            eval(parse(text = x0), envir = inenv)
        })
        
        params <- lapply(obj$parameters, function(x) get(x, envir = inenv))
        names(params) <- obj$parameters
        
        bees <- c(unlist(do.call(f.bounds, params)))
        result[i, ] <- c(objective, bees)
        
        if(objective < bees[1] | objective > bees[2]) {
            stop("Objective outside bounds, optimizer failed!")
            break
        }
        
    }
    colnames(result) <- c("objective", "bound.lower", "bound.upper")
    as.data.frame(result)
    
    
}
