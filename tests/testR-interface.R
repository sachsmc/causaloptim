library(causaloptim)

#graph <- specify_graph()

graph <- readRDS("tests/test-graphs/direct-and-indirect.RData")

plot(graph, vertex.color = ifelse(V(graph)$latent == 1, "grey70",
                                     ifelse(V(graph)$exposure == 1, "green", "white")), 
     vertex.shape = ifelse(V(graph)$outcome == 1, "rectangle", "circle"),
     edge.color = ifelse(E(graph)$edge.monotone == 1, "blue", "black"), 
     layout = layout_nicely, main = "Graph to be analyzed, inspect carefully")
legend("topleft", legend = c("latent", "outcome", "exposure", "monotone edge"), pt.cex = c(3, 3, 3, 1), 
       pch = c(20, 22, 20, NA), col = c("grey70", "black", "green", "blue"), lty = c(NA, NA, NA, 1))


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
        stop("error")
        break
    }
    
}
