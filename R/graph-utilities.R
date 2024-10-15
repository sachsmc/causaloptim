
#' Plot the analyzed graph object
#' 
#' Special plotting method for igraphs of this type
#' 
#' @param graphres an igraph object
#' @return None
#' @seealso \link{plot.linearcausalproblem} which plots a graph with attributes
#' @export
#' @examples
#' b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(2,2,2)
#' V(b)$exposure <- c(1,0,0)
#' V(b)$outcome <- c(0,1,0)
#' E(b)$rlconnect <- c(0,0,0)
#' E(b)$edge.monotone <- c(0,0,0)
#' plot(b)

plot_graphres <- function(graphres) {
    
    if (is.null(V(graphres)$exposure)) {
        V(graphres)$exposure <- 0
    }
    if (is.null(V(graphres)$outcome)) {
        V(graphres)$outcome <- 0
    }
    
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
#' @noRd
#' @examples
#' b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(3,4,2)
#' V(b)$exposure <- c(1,0,0)
#' V(b)$outcome <- c(0,1,0)
#' E(b)$rlconnect <- c(0,0,0)
#' E(b)$edge.monotone <- c(0,0,0)
#' print_nvals(graphres = b)
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
#' @noRd
#' @examples
#' b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(3,4,2)
#' V(b)$exposure <- c(1,0,0)
#' V(b)$outcome <- c(0,1,0)
#' E(b)$rlconnect <- c(0,0,0)
#' E(b)$edge.monotone <- c(0,0,0)
#' numberOfValues(graph = b, varname = "X")
numberOfValues <- function(graph, varname) {
    df <- data.frame(name_of_variable = vertex_attr(graph = graph)$name, 
                     number_of_possible_values = vertex_attr(graph = graph)$nvals)
    df[df$name_of_variable == varname, ]$number_of_possible_values
}

#' Find cycles in a graph
#' 
#' @param g an igraph object
#' @return A list of vectors of integers, indicating the vertex sequences for the cycles found in the graph
#' @noRd
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
#' @examples
#' graphres <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y)
#' V(graphres)$leftside <- c(1, 0, 0, 1, 0)
#' V(graphres)$latent <- c(0, 0, 0, 1, 1)
#' V(graphres)$nvals <- c(3, 2, 2, 2, 2)
#' V(graphres)$exposure <- c(0, 1, 0, 0, 0)
#' V(graphres)$outcome <- c(0, 0, 1, 0, 0)
#' E(graphres)$rlconnect <- c(0, 0, 0, 0, 0)
#' E(graphres)$edge.monotone <- c(0, 0, 0, 0, 0)
#' get_default_effect(graphres = graphres) == "p{Y(X = 1)=1} - p{Y(X = 0)=1}" # TRUE
get_default_effect <- function(graphres) {
    if (length(E(graphres)) == 0) {
        return("")
    }
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

# Check for right to left edges.
# edges: A 'data.frame' as output by 'edges_from_input'.
#' Check that a data frame containing the edges of a digraph with certain attributes 
#' satisfies the condition of no edges going from the 'right side' to the 'left side'.
#' @param edges A data.frame representing a digraph.
#' @return \code{TRUE} if the condition is satisfied; else \code{FALSE}.
#' @noRd
rlcheck0 <- function(edges) {
    if (sum(edges$rlconnect) > 0) {
        error_message <- "No connections from right to left are allowed!"
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            print(error_message)
        }
        return(FALSE)
    }
    TRUE
}

# Check for right side to left side edges in a digraph.
# graphres: An 'igraph' object as e.g. output by 'graphres_from_edges'.
#' Check that no edges of a given digraph go from the 'right side' to the 'left side'.
#' @param graphres An \code{igraph} object representing a digraph.
#' This digraph should have at least the binary edge attribute \code{rlconnect}.
#' @return \code{TRUE} if the condition is satisfied; else \code{FALSE}.
#' @noRd
#' @examples
#' graphres <- graph_from_literal(X -+ Y, X -+ M, M -+ Y, Ul -+ X, Ur -+ M, Ur -+ Y)
#' E(graphres)$rlconnect <- c(0, 0, 0, 0, 0, 0)
#' rlcheck(graphres = graphres) # TRUE
rlcheck <- function(graphres) {
    edges <- E(graph = graphres)
    rlcheck0(edges = edges)
}

# Check that vertices are named appropriately.
#' Check that the names given to the vertices of a digraph are all valid.
#' @param graphres An \code{igraph} object representing a digraph.
#' @return \code{TRUE} if all the variable names are valid; else \code{FALSE}.
#' @noRd
#' @examples
#' graphres <- graph_from_literal(X -+ Y, X -+ M, M -+ Y, Ul -+ X, Ur -+ M, Ur -+ Y)
#' vertexnamecheck(graphres = graphres) # TRUE
vertexnamecheck <- function(graphres) {
    vnames <- names(V(graphres))
    badnames <- grep(
        pattern = "(^[^[:alpha:]])|([[:punct:]])|(^p)",
        x = vnames,
        value = TRUE
    )
    if (length(badnames) > 0) {
        error_message <- sprintf(
            "Invalid names: %s, found in graph vertices!",
            paste(badnames,
                  collapse = ","
            )
        )
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            message(error_message)
        }
        return(FALSE)
    }
    TRUE
}

# Check that the digraph is acyclic.
#' Check that a given digraph is a DAG, i.e., contains no cycles.
#' @param graphres An \code{igraph} object representing a digraph.
#' @return \code{TRUE} if \code{graphres} is a DAG; else \code{FALSE}.
#' @noRd
#' @examples
#' graphres <- graph_from_literal(X -+ Y, X -+ M, M -+ Y, Ul -+ X, Ur -+ M, Ur -+ Y)
#' cyclecheck(graphres = graphres) # TRUE
cyclecheck <- function(graphres) {
    if (is.dag(graph = graphres)) {
        return(TRUE)
    }
    error_message <- "No cycles in the graph are allowed!"
    if (isRunning()) {
        showNotification(
            ui = error_message,
            type = "error"
        )
    } else {
        message(error_message)
    }
    FALSE
}

# Check that each categorical variable is at least dichotomous.
#' Check that the number of categorical levels for each variable in a graph is at least 2.
#' @param graphres An \code{igraph} object representing a digraph.
#' @return \code{TRUE} if each variable is at least binary; else \code{FALSE}.
#' @noRd
#' @examples
#' graphres <- graph_from_literal(X -+ Y, X -+ M, M -+ Y, Ul -+ X, Ur -+ M, Ur -+ Y)
#' V(graphres)$nvals <- c(3, 2, 4, 2, 2)
#' nvalscheck(graphres = graphres) # TRUE
nvalscheck <- function(graphres) {
    if (any(vertex_attr(graph = graphres)$nvals < 2)) {
        error_message <-
            "Each variable needs to be able to take on at least two distinct possible values!"
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            message(error_message)
        }
        return(FALSE)
    }
    TRUE
}

# Check all conditions on the digraph.
# Set 'ret = TRUE' to also return 'graphres' if all checks are passed.
#' Check conditions on digraph
#' 
#' Check that a given digraph satisfies the conditions of 
#' 'no left to right edges', 'no cycles', 'valid number of categories' and 'valid variable names'.
#' Optionally returns the digraph if all checks are passed.
#' @param graphres An \code{igraph} object representing a digraph.
#' @param ret A logical value. Default is \code{FALSE}.
#' Set to \code{TRUE} to also return \code{graphres} if all checks are passed.
#' @return If \code{ret=FALSE} (default): \code{TRUE} if all checks pass; else \code{FALSE}.
#' If \code{ret=TRUE}: \code{graphres} if all checks pass; else \code{FALSE}.
#' @export
#' @examples
#' graphres <- graph_from_literal(X -+ Y, X -+ M, M -+ Y, Ul -+ X, Ur -+ M, Ur -+ Y)
#' V(graphres)$leftside <- c(1, 0, 0, 1, 0)
#' V(graphres)$latent <- c(0, 0, 0, 1, 1)
#' V(graphres)$nvals <- c(2, 2, 2, 2, 2)
#' V(graphres)$exposure <- c(0, 0, 0, 0, 0)
#' V(graphres)$outcome <- c(0, 0, 0, 0, 0)
#' E(graphres)$rlconnect <- c(0, 0, 0, 0, 0, 0)
#' E(graphres)$edge.monotone <- c(0, 0, 0, 0, 0, 0)
#' graphrescheck(graphres = graphres) # TRUE
graphrescheck <- function(graphres, ret = FALSE) {
    if (rlcheck(graphres = graphres)) {
        if (cyclecheck(graphres = graphres)) {
            if (vertexnamecheck(graphres = graphres)) {
                if (nvalscheck(graphres = graphres)) {
                    if (ret) {
                        if (isRunning()) {
                            stopApp(returnValue = graphres)
                        }
                        return(graphres)
                    }
                    return(TRUE)
                }
            }
        }
    }
    FALSE
}

#' Check for paths from from to to 
#' 
#' 
#' @param parent_lookup A list of vectors
#' @param from character
#' @param to character
#' @param prev Should always be null when first called
#' @returns A list of paths or null if no path is found
#' @export
#' @examples
#' parent_lookup <- list(M = "Am", Y = c("M", "Ay"), A = NULL, Am = "A", Ay = "A")
#' check_parents(parent_lookup, "A", "Y")

check_parents <- function(parent_lookup, from, to, prev = NULL) {
    
    this <- if(is.null(prev)){
        prev <- to
        to 
    } else prev[1]
    parents <- parent_lookup[[this]]
    nprev <- c(this, prev)
    
    if(is.null(parents)) {
        prev
    } else if(from %in% parents) {
        c(from, prev)
    } else {
        res <- list()
        for(pa in parents) {
            res[[pa]] <- check_parents(parent_lookup, from = from, to = to, 
                                       prev = c(pa, prev))
        }
        
        res
    }
    
    
    
}


#' Initialize an igraph object for use with causaloptim
#' 
#' Checks for required attributes and adds defaults if missing
#'
#' @param graph An object of class igraph
#' @returns An igraph with the vertex attributes leftside, latent, and nvals, and edge attributes rlconnect and edge.monotone
#' @export
#' @examples
#' b <- igraph::graph_from_literal(X -+ Y)
#' b2 <- initialize_graph(b)
#' V(b2)$nvals
#' 
initialize_graph <- function(graph) {
    
    nvars <- length(V(graph))
    
    if(is.null(V(graph)$latent)) {
        Unodes <- grep("^U", names(V(graph)))
        V(graph)$latent <- rep(0, nvars)
        V(graph)$latent[Unodes] <- 1
    }
    
    if(is.null(V(graph)$leftside)) {
        V(graph)$leftside <- rep(0, nvars)
        if("Ur" %in% names(V(graph))) {
            rightV <- neighbors(graph, "Ur", mode = "out")
            leftdex <- match(setdiff(names(V(graph)), c("Ur", names(rightV))), 
                             names(V(graph)))
            V(graph)$leftside[leftdex] <- 1
        }
    }
    
    if(is.null(V(graph)$nvals)) {
        V(graph)$nvals <- rep(2, nvars)
    }
    
    if(is.null(E(graph)$rlconnect)) {
        E(graph)$rlconnect <- rep(0, length(E(graph)))
        
        rlchk <- E(graph)[V(graph)[V(graph)$leftside == 1] %<-% V(graph)[V(graph)$leftside == 0]]
        edge_attr(graph, "rlconnect", index = rlchk) <- 1
        
    } 
    
    if(is.null(E(graph)$edge.monotone)) {
        E(graph)$edge.monotone <- rep(0, length(E(graph)))
    }
    
    graph
    
}

