library(causaloptim)


graph <- readRDS("tests/test-graphs/instrument.RData")
obj <- analyze_graph(graph)

bounds.obs <- optimize_effect(obj)

f.bounds <- interpret_bounds(bounds.obs$bounds, obj$parameters)

length(formals(f.bounds))

nsim <- 1e4
objs <- rep(NA, nsim)
for(i in 1:nsim){
    
    sim.qs <- runif(length(obj$variables))
    sim.qs <- sim.qs / sum(sim.qs)
    
    names(sim.qs) <- obj$variables
    objective <- eval(parse(text = obj$objective), envir = as.list(sim.qs))
    
    inenv <- new.env()
    for(j in 1:length(sim.qs)) {
        
        assign(names(sim.qs)[j], sim.qs[j], inenv)
        
    }
    res <- lapply(as.list(obj$constraints[1:nrow(obj$p.vals)]), function(x) eval(parse(text = x), envir = inenv))
    
    params <- lapply(obj$parameters, function(x) get(x, envir = inenv))
    names(params) <- obj$parameters
    
    bees <- sort(do.call(f.bounds, params))
    objs[i] <- objective
    
    if(objective < bees[1] | objective > bees[2]) {
        print("error")
        break
    }
    
}
