function(input, output) {
    
    output$outcode <- renderPrint({
        
        myin <- input$edges
        if(length(myin) > 0) {
            print(edgeList())
            #print(igraphFromList())
        } else {
            
            print("Shift+click to add nodes, shift+drag to add edges")
            
        }
        
    })
    
    
    edgeList <- reactive({
        
        
        myin <- input$edges
        if(length(myin) > 0) {
            j <- seq(1, length(myin) - 12, by = 13)
            
            data.frame(id = paste0("e", myin[j]), source = myin[j+1], target = myin[j+2], 
                       source.leftside = ifelse(myin[j+3] == "FALSE", 0, 1), 
                       target.leftside = ifelse(myin[j + 4] == "FALSE", 0, 1), 
                       rlconnect = ifelse(myin[j+5] == "FALSE", 0, 1), 
                       source.latent = as.numeric(myin[j + 6]), target.latent = as.numeric(myin[j + 7]), 
                       source.outcome = as.numeric(myin[j + 8]), target.outcome = as.numeric(myin[j + 9]), 
                       source.exposure = as.numeric(myin[j + 10]), target.exposure = as.numeric(myin[j + 11]), 
                       edge.monotone = as.numeric(myin[j + 12]))
            
            
        } else {
            
            NULL
            
        }
        
    })
    
    
    igraphFromList <- reactive({
        
        myin <- input$edges
        if(length(myin) > 0) {
            
            edges <- edgeList()[, -c(1)]
            
            vertex.meta <- rbind(data.frame(vnames = edges$source, leftside = edges$source.leftside, 
                                      latent = edges$source.latent, outcome = edges$source.outcome, 
                                      exposure = edges$source.exposure),
                                 data.frame(vnames = edges$target, leftside = edges$target.leftside, 
                                            latent = edges$target.latent, outcome = edges$target.outcome, 
                                            exposure = edges$target.exposure))
            
            print(myin)
            graphres <- graph_from_data_frame(edges[, c(1, 2, 5, 12)], vertices = unique(vertex.meta))
            
            ogleft <- V(graphres)[V(graphres)$leftside == 1]
            ogright <- V(graphres)[V(graphres)$leftside == 0]
            graphres <- add_vertices(graphres, 2, name = c("VL0", "VR0"), latent = c(1, 1), 
                                     leftside = c(1, 0), outcome = c(0, 0), exposure = c(0, 0))
            
            graphres <- add_edges(graphres, unlist(lapply(names(ogleft), function(x) c("VL0", x))), 
                                  rlconnect = rep(0,length(ogleft)), edge.monotone= rep(0, length(ogleft)))
            graphres <- add_edges(graphres, unlist(lapply(names(ogright), function(x) c("VR0", x))), 
                                  rlconnect = rep(0,length(ogright)), edge.monotone= rep(0, length(ogright)))
            
            
            plot(graphres, vertex.color = ifelse(V(graphres)$latent == 1, "grey70",
                                                 ifelse(V(graphres)$exposure == 1, "green", "white")), 
                 vertex.shape = ifelse(V(graphres)$outcome == 1, "rectangle", "circle"),
                 edge.color = ifelse(E(graphres)$edge.monotone == 1, "blue", "black"), 
                 layout = layout_nicely, main = "Graph to be analyzed, inspect carefully")
            legend("topleft", legend = c("latent", "outcome", "exposure", "monotone edge"), pt.cex = c(3, 3, 3, 1), 
                   pch = c(20, 22, 20, NA), col = c("grey70", "black", "green", "blue"), lty = c(NA, NA, NA, 1))
            
            graphres
            
        } else {
            
            NULL
            
        }
        
    })
    
    observe({
        
        myin <- edgeList()
        if(sum(myin$rlconnect) > 0) {
            
             showNotification("No connections from right to left are allowed!", type = "error")
            
        }
        
    })
    
    observeEvent(input$endbtn, {
            
            myin <- edgeList()
            if(sum(myin$rlconnect) > 0) {
                
                showNotification("No connections from right to left are allowed!", type = "error")
                
            } else {
            
                stopApp(igraphFromList())
            }
        
    })
    
    
    
}
