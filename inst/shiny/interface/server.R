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
            j <- seq(1, length(myin) - 7, by = 8)
            
            data.frame(id = paste0("e", myin[j]), source = myin[j+1], target = myin[j+2], 
                       leftside = ifelse(myin[j+3] == "FALSE", 0, 1), 
                       lrconnect = ifelse(myin[j+4] == "FALSE", 0, 1), 
                       source.latent = as.numeric(myin[j + 5]), target.latent = as.numeric(myin[j + 6]), 
                       target.outcome = as.numeric(myin[j + 7]))
            
            
        } else {
            
            NULL
            
        }
        
    })
    
    
    igraphFromList <- reactive({
        
        myin <- input$edges
        if(length(myin) > 0) {
            
            graph_from_data_frame(edgeList()[, -c(1)])
            
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
