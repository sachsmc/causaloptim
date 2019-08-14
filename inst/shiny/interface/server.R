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
    
    
    ## return graph to R
    
    observeEvent(input$endbtn, {
            
            myin <- edgeList()
            if(sum(myin$rlconnect) > 0) {
                
                showNotification("No connections from right to left are allowed!", type = "error")
                
            } else {
            
                stopApp(igraphFromList())
            }
        
    })
    
    
    ## analyze the graph in shiny
    
    observeEvent(input$analyze, {
      
        myin <- edgeList()
        if(sum(myin$rlconnect) > 0) {
            
            showNotification("No connections from right to left are allowed!", type = "error")
            
        } else {
            
            graphres <- igraphFromList()
            
            removeUI(selector = "#myplot")
            removeUI(selector = "#results")
            
            insertUI(selector = "#analyze", 
                     where = "afterEnd", 
                     ui = tags$div(id = "myplot", 
                                   plotOutput("myplot")
                         )
            )
            
            output$myplot <- renderPlot(plot.graphres(graphres))
            
            insertUI(selector = "#myplot", 
                     where = "afterEnd", 
                     ui = tags$div(id = "results", 
                                   actionButton("optimize", "Press to optimize the bounds")
                                   )
            )
            
            
        }
        
    })
    
    
    optimizeGraph <- reactive({
      
      graphres <- igraphFromList()
      obj <- analyze_graph(graphres)
      bounds.obs <- optimize_effect(obj)
      
      list(graphres = graphres, obj = obj, bounds.obs = bounds.obs)
      
    })
    
    
    observeEvent(input$optimize, {
        
        
        b <- optimizeGraph()
        
        
        
        removeUI(selector = "#resultsText")
        insertUI(selector = "#results", where = "beforeEnd", 
                 ui = div(h3("Results"), 
                              htmlOutput("resultsText")
                          )
                 )
        
        
        expo <- V(b$graphres)[V(b$graphres)$exposure == 1]
        outc <- V(b$graphres)[V(b$graphres)$outcome == 1]
        effectpath <- all_simple_paths(b$graphres, from = expo, to = outc)
        
        effecttext <- sprintf("Computed bounds for the total effect of '%s' on '%s', i.e., P(%s = 1 | do(%s = 1)) - P(%s = 1 | do(%s = 0)). The total effect includes the following paths: ", names(expo), names(outc), names(outc), names(expo), names(outc), names(expo))
        totalpaths <- sapply(effectpath, function(x) paste(names(x), collapse = " -> "))
        
        lkey <- letters[1:length(attr(b$obj$parameters, "rightvars"))]
        rkey <- letters[(length(attr(b$obj$parameters, "rightvars")) + 1):(length(attr(b$obj$parameters, "rightvars")) + 
                                                                             length(attr(b$obj$parameters, "condvars")))]
        
        sampparm <- paste0("p", paste(lkey, collapse = ""), "_", 
                           paste(rkey, collapse = ""))
        
        probstate <- paste0("P(", paste(paste0(attr(b$obj$parameters, "rightvars"), " = ", lkey), collapse = ", "), " | ", 
        paste0(attr(b$obj$parameters, "condvars"), " = ", rkey, collapse = ", "), ")")
        
        variabletext <- sprintf("The bounds are reported in terms of parameters of the form %s, which represents the probability %s.", sampparm, probstate)
        
        textres <- lapply(c(effecttext, totalpaths, variabletext, "Bounds: ",
                            b$bounds.obs$bounds[1], b$bounds.obs$bounds[2]), function(x) {
                   
                   x2 <- strsplit(x, "\n", fixed = TRUE)[[1]]
                   lapply(x2, function(x) tags$p(x))
                   
                 })
        
        
        output$resultsText <- renderUI(do.call(tagList, textres))
        
        insertUI(selector = "#results", where = "afterEnd", 
                 ui = actionButton("downloadf", "Press to return objects to R"))
        
        
        
        
    })
    
    
    
    observeEvent(input$downloadf, {
      
      b <- optimizeGraph()
      b$boundsFunction <- interpret_bounds(b$bounds.obs$bounds, b$obj$parameters)
      stopApp(b)
      
    })
    
    
}
