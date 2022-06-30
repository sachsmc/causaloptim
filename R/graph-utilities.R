
#' Plot the analyzed graph object
#' 
#' Special plotting method for igraphs of this type
#' 
#' @param graphres an igraph object
#' @return None
#' @export

plot_graphres <- function(graphres) {
    
    nvals_info <- ifelse(any(vertex_attr(graphres)$nvals > 2), 
                         paste("Number of possible values of each variable:\n", print_nvals(graphres)), 
                         "All measured variables are assumed binary.")
    if (any(vertex_attr(graphres)$nvals > 3)) {
        nvals_info <- paste(nvals_info,"\nWarning: Multilevel categorical variables may have a large impact on computation time!")
    }
    mylayout <- cbind(V(graphres)$x, V(graphres)$y)
    plot(graphres, vertex.color = ifelse(V(graphres)$latent == 1, "grey70",
                                         ifelse(V(graphres)$exposure == 1, "green", "white")), 
         vertex.shape = ifelse(V(graphres)$outcome == 1, "rectangle", "circle"),
         edge.color = ifelse(E(graphres)$edge.monotone == 1, "blue", "black"), 
         layout = mylayout, main = "Graph to be analyzed, inspect carefully", sub = nvals_info)
    legend("topleft", legend = c("latent", "outcome", "exposure", "monotone edge"), pt.cex = c(3, 3, 3, 1), 
           pch = c(20, 22, 20, NA), col = c("grey70", "black", "green", "blue"), lty = c(NA, NA, NA, 1))
    
}

#' Print the number of values of each variable/vertex of the analyzed graph object
#' 
#' @param graphres an igraph object
#' @return None
#' @export
print_nvals <- function(graphres) {
    df <- data.frame(name_of_variable = vertex_attr(graph = graphres)$name, 
                     number_of_possible_values = vertex_attr(graph = graphres)$nvals)
    unknown <- df$name_of_variable %in% c("Ur", "Ul")
    df <- df[!unknown, ]
    paste(df$name_of_variable, df$number_of_possible_values, sep = ": ", collapse = ", ")
}

#' Get the number of values of a given variable in the graph
#' 
#' @param graph An igraph object.
#' @param varname A string. The name of a vertex in 'graph'.
#' @return An integer greater than 1. The number of values of 'varname'.
#' @export
numberOfValues <- function(graph, varname) {
    df <- data.frame(name_of_variable = vertex_attr(graph = graph)$name, 
                     number_of_possible_values = vertex_attr(graph = graph)$nvals)
    df[df$name_of_variable == varname, ]$number_of_possible_values
}

#' Find cycles in a graph
#' 
#' @param g an igraph object
#' @return A list of vectors of integers, indicating the vertex sequences for the cycles found in the graph
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

#' Define default effect for a given graph
#' 
#' @param graphres The graph object, should have vertex attributes "outcome" and "exposure"
#' 
#' @return A string that can be passed to \link{parse_effect}
#' @export
get_default_effect <- function(graphres) {
    
    rightvars <- V(graphres)[V(graphres)$leftside == 0 & names(V(graphres)) != "Ur"]
    
    expo <- V(graphres)[V(graphres)$exposure == 1]
    outc <- V(graphres)[V(graphres)$outcome == 1]
    effectpath <- all_simple_paths(graphres, from = expo, to = outc)
    
    if(length(outc) == 0 | length(expo) == 0) {
        default.effect <- ""
    } else {
        ## default total effect
        def.eff <- paste0(names(outc), "(")
        stack <- c(")")
        len.arg <- 0
        for(j in 1:length(effectpath)) {
            res <- ""
            nvs <- length(effectpath[[j]])
            for(k in max(1, nvs - 1):1) {
                thisvar <- effectpath[[j]][k]
                res <- paste0(res, names(thisvar), 
                              ifelse(names(thisvar) == names(expo), 
                                     " = %s", "("))
                if(names(thisvar) == names(expo)) {
                    len.arg <- len.arg + 1
                } else {
                    stack <- c(stack, ")")
                }
                
            }
            def.eff <- paste0(def.eff, res, 
                              ifelse(j < length(effectpath), ", ", ""))
            
            #if((nvs - 1) > 1) {
            #    stack <- c(stack, ")")
            #}
        }
        def.eff <- paste(def.eff, paste(stack, collapse = ""), sep = "")
        
        def.eff <- paste0("p{", def.eff, "=1}")
        
        
        default.effect <- paste(sapply(c(1, 0), function(x){
            
            arg2 <- lapply(1:len.arg, function(i) x)
            arg1 <- def.eff
            do.call(sprintf, c(arg1, arg2))
            
            
            }), collapse = " - ")
        
    }
        default.effect
} 
