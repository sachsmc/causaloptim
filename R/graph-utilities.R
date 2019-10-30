
#' Plot the analyzed graph object
#' 
#' Special plotting method for igraphs of this type
#' 
#' @param graphres an igraph object
#' 
#' @export

plot_graphres <- function(graphres) {
    
    mylayout <- cbind(V(graphres)$x, V(graphres)$y)
    plot(graphres, vertex.color = ifelse(V(graphres)$latent == 1, "grey70",
                                         ifelse(V(graphres)$exposure == 1, "green", "white")), 
         vertex.shape = ifelse(V(graphres)$outcome == 1, "rectangle", "circle"),
         edge.color = ifelse(E(graphres)$edge.monotone == 1, "blue", "black"), 
         layout = mylayout, main = "Graph to be analyzed, inspect carefully")
    legend("topleft", legend = c("latent", "outcome", "exposure", "monotone edge"), pt.cex = c(3, 3, 3, 1), 
           pch = c(20, 22, 20, NA), col = c("grey70", "black", "green", "blue"), lty = c(NA, NA, NA, 1))
    
}


#' Find cycles in a graph
#' 
#' @param g an igraph object
#' @export
find_cycles = function(g) {
    Cycles = NULL
    for(v1 in V(g)) {
        if(degree(g, v1, mode="in") == 0) { next }
        GoodNeighbors = neighbors(g, v1, mode="out")
        GoodNeighbors = GoodNeighbors[GoodNeighbors > v1]
        for(v2 in GoodNeighbors) {
            TempCyc = lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p))
            TempCyc = TempCyc[which(sapply(TempCyc, length) > 3)]
            TempCyc = TempCyc[sapply(TempCyc, min) == sapply(TempCyc, `[`, 1)]
            Cycles  = c(Cycles, TempCyc)
        }
    }
    Cycles
}