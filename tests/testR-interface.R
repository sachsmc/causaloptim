library(causaloptim)

#graph <- specify_graph()

graph <- readRDS("tests/test-graphs/simple.RData")

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
simulation <- simulate_bounds(obj, bounds.obs, nsim = 100)




