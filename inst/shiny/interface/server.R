function(input, output) {
    
    # output$outcode <- renderPrint({
    #     
    #     myin <- input$edges
    #     if(length(myin) > 0) {
    #         print(edgeList())
    #         #print(igraphFromList())
    #     } else {
    #         
    #         print("Shift+click to add nodes, shift+drag to add edges")
    #         
    #     }
    #     
    # })
    
    
    edgeList <- reactive({
        
        
        myin <- input$edges
        if(length(myin) > 0) {
            j <- seq(1, length(myin) - 16, by = 17)
            
            data.frame(id = paste0("e", myin[j]), source = myin[j+1], target = myin[j+2], 
                       source.leftside = ifelse(myin[j+3] == "FALSE", 0, 1), 
                       target.leftside = ifelse(myin[j + 4] == "FALSE", 0, 1), 
                       rlconnect = ifelse(myin[j+5] == "FALSE", 0, 1), 
                       source.latent = as.numeric(myin[j + 6]), target.latent = as.numeric(myin[j + 7]), 
                       source.outcome = as.numeric(myin[j + 8]), target.outcome = as.numeric(myin[j + 9]), 
                       source.exposure = as.numeric(myin[j + 10]), target.exposure = as.numeric(myin[j + 11]), 
                       edge.monotone = as.numeric(myin[j + 12]), 
                       source.x = as.numeric(myin[j + 13]), source.y = as.numeric(myin[j + 14]), 
                       target.x = as.numeric(myin[j + 15]), target.y = as.numeric(myin[j + 16]) )
            
            
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
                                      exposure = edges$source.exposure, x = edges$source.x, y = -edges$source.y),
                                 data.frame(vnames = edges$target, leftside = edges$target.leftside, 
                                            latent = edges$target.latent, outcome = edges$target.outcome, 
                                            exposure = edges$target.exposure, x = edges$target.x, y = -edges$target.y))
            
            #print(myin)
            graphres <- graph_from_data_frame(edges[, c(1, 2, 5, 12)], vertices = unique(vertex.meta))
            
            ogleft <- V(graphres)[V(graphres)$leftside == 1]
            ogright <- V(graphres)[V(graphres)$leftside == 0]
            
            if(length(ogleft) > 1) {
              graphres <- add_vertices(graphres, 1, name = "Ul", latent = 1, 
                                       leftside = 1, outcome = 0, exposure = 0, 
                                       x = min(V(graphres)$x) - 100, y = min(V(graphres)$y) +20)
              graphres <- add_edges(graphres, unlist(lapply(names(ogleft), function(x) c("Ul", x))), 
                                    rlconnect = rep(0,length(ogleft)), edge.monotone= rep(0, length(ogleft)))
            }
            
            if(length(ogright) > 1) {
              graphres <- add_vertices(graphres, 1, name = "Ur", latent = 1, 
                                       leftside = 0, outcome = 0, exposure = 0, 
                                       x = max(V(graphres)$x) + 100, y = min(V(graphres)$y) +20)
              
              graphres <- add_edges(graphres, unlist(lapply(names(ogright), function(x) c("Ur", x))), 
                                    rlconnect = rep(0,length(ogright)), edge.monotone= rep(0, length(ogright)))
            }
            
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
            
                graph <- igraphFromList()
                ## check for valid names
                vnames <- names(V(graph))
                badnames <- grep("(^[^[:alpha:]])|([[:punct:]])|(^p)", vnames, value = TRUE)
                ## check for cycles
                
                cychek <- find_cycles(graph)
                if(length(cychek) > 0) {
                  
                  showNotification("No cycles in the graph are allowed!", type = "error")
                } else if (length(badnames) > 0) {
                  
                  showNotification(sprintf("Invalid names: %s, found in graph vertices!", 
                                           paste(badnames, collapse = ",")), type = "error")
                  
                } else {
                  
                  
                  
                  stopApp(graph)
                  
                }
            }
        
    })
    
    ## analyze the graph in shiny
    
    observeEvent(input$analyze, {
      
        myin <- edgeList()
        
        
        if(sum(myin$rlconnect) > 0) {
            
            showNotification("No connections from right to left are allowed!", type = "error")
            
        } else {
            
            graphres <- igraphFromList()
            
            cychek <- find_cycles(graphres)
            ## check for valid names
            vnames <- names(V(graphres))
            badnames <- grep("(^[^[:alpha:]])|([[:punct:]])|(^p)", vnames, value = TRUE)
            
            if(length(cychek) > 0) {
              
              showNotification("No cycles in the graph are allowed!", type = "error")
              
            } else if(length(badnames) > 0) { 
              
              showNotification(sprintf("Invalid names: %s, found in graph vertices!", paste(badnames, collapse = ",")), type = "error")
              
              } else {
             
            
            
            removeUI(selector = "#myplot")
            removeUI(selector = "#results")
            
            insertUI(selector = "#analyze", 
                     where = "afterEnd", 
                     ui = tags$div(id = "myplot", 
                                   plotOutput("myplot")
                         )
            )
            
            output$myplot <- renderPlot(plot_graphres(graphres))
            
            rightvars <- V(graphres)[V(graphres)$leftside == 0 & names(V(graphres)) != "Ur"]
            
            expo <- V(graphres)[V(graphres)$exposure == 1]
            outc <- V(graphres)[V(graphres)$outcome == 1]
            effectpath <- all_simple_paths(graphres, from = expo, to = outc)
            
            if(length(outc) == 0 | length(expo) == 0) {
              default.effect <- ""
            } else {
            ## default total effect
            def.eff <- paste0(names(outc), "(")
            for(j in 1:length(effectpath)) {
              res <- ""
              nvs <- length(effectpath[[j]])
              for(k in max(1, nvs - 1):1) {
                thisvar <- effectpath[[j]][k]
                res <- paste0(res, names(thisvar), 
                              ifelse(names(thisvar) == names(expo), 
                                     " = %s", "("))
                
              }
              def.eff <- paste0(def.eff, res, ")", ifelse(j < length(effectpath), ", ", ""))
              
            }
           
            def.eff <- paste0("p{", def.eff, "=1}")
            default.effect <- paste(sapply(c(1, 0), function(x) sprintf(def.eff, x, x)), collapse = " - ")
            }
            ##
            
            effectUI <- div(id = "effect", 
                                 h3("Specify causal effect of interest (required)"), 
                            fluidRow(id = "effecttext",
                                 column(8, textAreaInput("effect", NULL, default.effect)), 
                                 column(1, actionButton("parseeffect", "Parse", style="background-color: #69fb82"))
                                 ))
            
            insertUI(selector = "#myplot", 
                     where = "afterEnd", 
                     ui = list(effectUI,
                               div(id = "constraintsdiv", h3("Constraints (optional)"),
                                   fluidRow(
                                        column(1, actionButton("constraints", "Specify constraints"))
                                        )),
                               div(id = "results", h3("Bounds"), 
                                   fluidRow(
                                        column(1, actionButton("optimize", "Compute the bounds", style="background-color: #69fb82"))
                               ))
                     )
            )
            
            
        }}
            
            })
    
    
    effectFixed <- reactiveValues()
    
    observeEvent(input$parseeffect, {
      
      
        effecttext <- input$effect
        error <- NULL
        
        parsed.test <- tryCatch(parse_effect(effecttext), error = function(e) "fail")
        
        
        if(!is.list(parsed.test)) {
          
          error <- "Unable to parse effect!"
          
        } else {
          
          chk0 <- unlist(parsed.test)
          
          allnmes <- unique(unlist(strsplit(gsub("(vars\\.|values\\.)", "", 
                                                 names(chk0[-which(names(chk0) == "oper")])), 
                                            "\\.")))
          
          graph <- igraphFromList()
          
          ## check that children of intervention sets are on the right
          find.iset <- function(l) {
            
            lapply(1:length(l), function(i) {
              if(is.numeric(l[[i]])){ 
                return(names(l)[i]) 
              } else {
                find.iset(l[[i]])
              }
            })
          }
          
          interven.vars <- unique(unlist(lapply(parsed.test$vars, find.iset)))
          any.children.onleft <- sapply(interven.vars, function(v) {
            
            children <- neighbors(graph, V(graph)[v], mode = "out")
            any(children$leftside == 1)
            
          })
          
          if(any(any.children.onleft) == TRUE) {
            error <- "No children of variables in the intervention set allowed on the leftside!"
          }
          
          if("oper" %in% names(chk0) & !chk0["oper"] %in% c("+", "-")) {
            error <- sprintf("Operator '%s' not allowed!", chk0["oper"])
          }
          
          realnms <- names(V(graph))
          if(any(!allnmes %in% realnms)) {
            
            error <- sprintf("Names %s in effect not specified in graph!", 
                             paste(allnmes[which(!allnmes %in% realnms)], collapse = ", "))
            
          }
          
          
        }
        
        if(is.null(error)) {
        
        removeUI("#effecttext", immediate = TRUE)
        insertUI("#effect", "beforeEnd", 
                 ui = fluidRow(column(8, pre(effecttext))))
        
        effectFixed$effectt <- effecttext
        
        } else {
          
          showNotification(error, type = "error")
          
        }
        
      
    })
            
               
    
    optimizeGraph <- reactive({
      
      
      graphres <- igraphFromList()
      ## parse causal effect
      effecttext <- effectFixed$effectt
      
      constraints <- fixedConstraints$constraints
      
      
    
      obj <- analyze_graph(graphres, constraints, effectt = effecttext)
      bounds.obs <- optimize_effect(obj)
      
      list(graphres = graphres, obj = obj, bounds.obs = bounds.obs, 
           constraints = constraints, effect = effecttext)
      
    })
 
    
    ### constraints
    
    fixedConstraints <- reactiveValues()
    
    observeEvent(input$constraints, {
      
      
      insertUI(selector = "#constraintsdiv", where = "beforeEnd", 
               ui = div(h3("Constraints"), 
                        fluidRow(id = "constrainttext",
                                 column(8, textAreaInput("constraintfield", NULL)), 
                                 column(1, actionButton("parseconstraint", "Parse", style="background-color: #69fb82"))
                        )
               )
      )
      
      
    })
    
    
    observeEvent(input$parseconstraint, {
      
      constrainttext <- strsplit(input$constraintfield, "\n", fixed = TRUE)[[1]]
      
      error <- NULL
      
      parsed.ctest <- tryCatch(parse_constraints(constrainttext), error = function(e) "fail")
      if(!is.list(parsed.ctest)) {
        
        error <- "Unable to parse constraints!"
        
      } else {
        
        allnmes <- unique(c(parsed.ctest$leftout, parsed.ctest$rightout, 
                            gsub("=(0|1)", "", c(parsed.ctest$leftcond, parsed.ctest$rightcond))))
        
        graph <- igraphFromList()
        if(any(!parsed.ctest$operator %in% c("==", "<", ">", "<=", ">="))) {
          error <- "Operator not allowed!"
        }
        
        realnms <- c(names(V(graph)), "0", "1")
        if(any(!allnmes %in% realnms)) {
          
          error <- sprintf("Names %s in constraint not specified in graph!", 
                           paste(allnmes[which(!allnmes %in% realnms)], collapse = ", "))
          
        }
        
        
      }
      
      if(is.null(error)) {
      
      removeUI("#constrainttext", immediate = TRUE)
      insertUI("#constraintsdiv", "beforeEnd", 
               ui = fluidRow(column(8, pre(paste(constrainttext, collapse = "\n")))))
      
      fixedConstraints$constraints <- constrainttext
      
      } else {
        showNotification(error, type = "error")
        
      }
      
    })
    
    
    
    observeEvent(input$optimize, {
        
        
        b <- optimizeGraph()
        
        
        
        removeUI(selector = "#resultsText")
        insertUI(selector = "#results", where = "beforeEnd", 
                 ui = div(fluidRow(column(12, h3("Results")), 
                               column(12, pre(htmlOutput("resultsText")))
                          ))
                 )
        
        
        effecttext <- sprintf("Computed bounds for the effect %s", effectFixed$effectt)
        
        lkey <- letters[1:length(attr(b$obj$parameters, "rightvars"))]
        rkey <- letters[(length(attr(b$obj$parameters, "rightvars")) + 1):(length(attr(b$obj$parameters, "rightvars")) + 
                                                                             length(attr(b$obj$parameters, "condvars")))]
        
        if(length(attr(b$obj$parameters, "condvars")) == 0) rkey <- NULL
        
        sampparm <- paste0("p", paste(lkey, collapse = ""), "_", 
                           paste(rkey, collapse = ""))
        
        probstate <- paste0("P(", paste(paste0(attr(b$obj$parameters, "rightvars"), " = ", lkey), collapse = ", "), " | ", 
        paste0(attr(b$obj$parameters, "condvars"), " = ", rkey, collapse = ", "), ")")
        
        if(length(attr(b$obj$parameters, "condvars")) == 0) {
          probstate <- paste0("P(", paste(paste0(attr(b$obj$parameters, "rightvars"), " = ", lkey), collapse = ", "), ")")
        }
        
        variabletext <- sprintf("The bounds are reported in terms of parameters of the form %s, which represents the probability %s.", sampparm, probstate)
        
        if(!is.null(b$constraints)) {
          constrainttext <- sprintf("This following constraints have been specifed: \n %s", paste(b$constraints, collapse = "\n"))
        } else constrainttext <- "No constraints have been specified"
        
        textres <- lapply(c(effecttext, constrainttext, variabletext, "Bounds: ",
                            "Lower = ", b$bounds.obs$bounds[1], "Upper = ", b$bounds.obs$bounds[2]), function(x) {
                   
                   x2 <- strsplit(x, "\n", fixed = TRUE)[[1]]
                   lapply(x2, function(x) tags$p(x))
                   
                 })
        
        
        output$resultsText <- renderUI(do.call(tagList, textres))
        
        insertUI(selector = "#results", where = "beforeEnd", 
                 ui = actionButton("downloadf", "Exit and return objects to R", style="background-color: #fb6970"))
        
        
        
        
    })
    
    
    
    observeEvent(input$downloadf, {
      
      print("click")
      b <- optimizeGraph()
      b$boundsFunction <- interpret_bounds(b$bounds.obs$bounds, b$obj$parameters)
      stopApp(b)
      
    })
    
    
}
