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
            j <- seq(1, length(myin) - 8, by = 9)
            
            data.frame(id = paste0("e", myin[j]), source = myin[j+1], target = myin[j+2], 
                       source.leftside = ifelse(myin[j+3] == "FALSE", 0, 1), 
                       target.leftside = ifelse(myin[j + 4] == "FALSE", 0, 1), 
                       lrconnect = ifelse(myin[j+5] == "FALSE", 0, 1), 
                       source.latent = as.numeric(myin[j + 6]), target.latent = as.numeric(myin[j + 7]), 
                       target.outcome = as.numeric(myin[j + 8]))
            
            
        } else {
            
            NULL
            
        }
        
    })
    
    
    igraphFromList <- reactive({
        
        myin <- input$edges
        if(length(myin) > 0) {
            
            edges <- edgeList()[, -c(1)]
            
            vertex.meta <- rbind(data.frame(vnames = edges$source, leftside = edges$source.leftside, 
                                      latent = edges$source.latent, outcome = 0),
                                 data.frame(vnames = edges$target, leftside = edges$target.leftside, 
                                            latent = edges$target.latent, outcome = edges$target.outcome))
            
            print(unique(vertex.meta))
            graph_from_data_frame(edges[, c(1, 2, 5)], vertices = unique(vertex.meta))
            
        } else {
            
            NULL
            
        }
        
    })
    
    observe({
        
        myin <- edgeList()
        if(sum(myin$lrconnect) > 1) {
            
             showNotification("Only one connection is allowed from left side to right side!", type = "error")
            
        }
        
    })
    
    observeEvent(input$endbtn, {
            
            myin <- edgeList()
            if(sum(myin$lrconnect) > 1) {
                
                showNotification("Only one connection is allowed from left side to right side!", type = "error")
                
            } else {
            
                stopApp(igraphFromList())
            }
        
    })
    
    
    
}
