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
            j <- seq(1, length(myin) - 18, by = 19)
            
            data.frame(id = paste0("e", myin[j]), source = myin[j+1], target = myin[j+2], 
                       source.leftside = ifelse(myin[j+3] == "FALSE", 0, 1), 
                       target.leftside = ifelse(myin[j + 4] == "FALSE", 0, 1), 
                       rlconnect = ifelse(myin[j+5] == "FALSE", 0, 1), 
                       source.latent = as.numeric(myin[j + 6]), target.latent = as.numeric(myin[j + 7]), 
                       source.outcome = as.numeric(myin[j + 8]), target.outcome = as.numeric(myin[j + 9]), 
                       source.exposure = as.numeric(myin[j + 10]), target.exposure = as.numeric(myin[j + 11]), 
                       source.nvals = as.numeric(myin[j + 12]), target.nvals = as.numeric(myin[j + 13]), 
                       edge.monotone = as.numeric(myin[j + 14]), 
                       source.x = as.numeric(myin[j + 15]), source.y = as.numeric(myin[j + 16]), 
                       target.x = as.numeric(myin[j + 17]), target.y = as.numeric(myin[j + 18]) )
            
            
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
                                      exposure = edges$source.exposure, nvals = edges$source.nvals, 
                                      x = edges$source.x, y = -edges$source.y),
                                 data.frame(vnames = edges$target, leftside = edges$target.leftside, 
                                            latent = edges$target.latent, outcome = edges$target.outcome, 
                                            exposure = edges$target.exposure, nvals = edges$target.nvals, 
                                            x = edges$target.x, y = -edges$target.y))
            
            #print(myin)
            graphres <- graph_from_data_frame(edges[, c(1, 2, 5, 14)], vertices = unique(vertex.meta))
            
            ogleft <- V(graphres)[V(graphres)$leftside == 1]
            ogright <- V(graphres)[V(graphres)$leftside == 0]
            
            if(length(ogleft) > 1) {
              graphres <- add_vertices(graphres, 1, name = "Ul", latent = 1, 
                                       leftside = 1, outcome = 0, exposure = 0, nvals = 2, 
                                       x = min(V(graphres)$x) - 100, y = min(V(graphres)$y) +20)
              graphres <- add_edges(graphres, unlist(lapply(names(ogleft), function(x) c("Ul", x))), 
                                    rlconnect = rep(0,length(ogleft)), edge.monotone= rep(0, length(ogleft)))
            }
            
            if(length(ogright) > 1) {
              graphres <- add_vertices(graphres, 1, name = "Ur", latent = 1, 
                                       leftside = 0, outcome = 0, exposure = 0, nvals = 2, 
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
                  
                } else if (any(vertex_attr(graph)$nvals < 2)) {
                  showNotification("Each variable needs to be able to take on at least two distinct possible values!", type = "error")
                } else {
                  
                  
                  
                  stopApp(graph)
                  
                }
            }
        
    })
    
    ## analyze the graph in shiny
    
    observeEvent(input$analyze, {
      
      removeUI(selector = "#effect")
      removeUI(selector = "#constraintsdiv")
      removeUI(selector = "#myplot")
      removeUI(selector = "#results")
      removeUI(selector = "#toast")
      removeUI(selector = "#nvalsModal")
      
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
             
            insertUI(selector = "#analyze", 
                     where = "afterEnd", 
                     ui = tags$div(id = "myplot", 
                                   plotOutput("myplot")
                         )
            )
            
            output$myplot <- renderPlot(plot_graphres(graphres))
            
            rightvars <- V(graphres)[V(graphres)$leftside == 0 & names(V(graphres)) != "Ur"]
            
            default.effect <- get_default_effect(graphres)
            
            ##
            
            effectUI <- div(id = "effect", 
                                 h3("Specify causal effect of interest (required)"), 
                            helpText("Use the text box to describe your causal effect of interest. The effects must be of the form p{V11(X=a)=a; V12(X=a)=b;...; W1=a; ...} op1 p{V21(X=b)=a; V22(X=c)=b;...; W1=b} op2 ... where Vij and Wk are names of variables in the graph, a, b are numeric values in the appropriate value sets, and op are either - or +. You can specify a single probability statement (i.e., no operator). Note that the probability statements begin with little p, and use curly braces, and items inside the probability statements are separated by ;. The variables may be potential outcomes which are denoted by parentheses, and if there is nothing on the left side, they may also be observed outcomes which do not have parentheses. Variables may also be nested inside potential outcomes."),
                            fluidRow(id = "effecttext",
                                 column(8, textAreaInput("effect", NULL, default.effect)), 
                                 column(1, actionButton("parseeffect", "Parse", style="background-color: #69fb82"))
                                 ))
            
            insertUI(selector = "#myplot", 
                     where = "afterEnd", 
                     ui = list(effectUI,
                               div(id = "constraintsdiv", h3("Constraints (optional)"),
                                   helpText("Here you can specify potential outcomes to constrain by writing the potential outcomes, values of their parents, and operators that determine the constraint (equalities or inequalities). For example, X(Z = 1) >= X(Z = 0)."),
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
          
          
          badnames <- FALSE
          graph <- igraphFromList()
          
          chk0 <- lapply(parsed.test$vars, causaloptim:::btm_var)
          
          interven.vars <- unique(unlist(chk0))
          allnmes <- unique(c(interven.vars, unlist(lapply(parsed.test$vars, names))))
          
          realnms <- names(V(graph))
          if(any(!allnmes %in% realnms)) {
              
              error <- sprintf("Names %s in effect not specified in graph!", 
                               paste(allnmes[which(!allnmes %in% realnms)], collapse = ", "))
              badnames <- TRUE
          }
          
          
          ## check that children of intervention sets are on the right
          if(!badnames) {
          any.children.onleft <- sapply(interven.vars, function(v) {
            
            children <- neighbors(graph, V(graph)[v], mode = "out")
            any(children$leftside == 1)
            
          })
          
          
          
          if(any(any.children.onleft) == TRUE) {
            error <- sprintf("Cannot intervene on %s because it has children on the leftside!", 
                             paste(interven.vars[which(any.children.onleft)], collapse = ", "))
          }
          
          
          if(any(V(graph)$leftside == 1 & names(V(graph)) != "Ul")) {
              cond.vars <- names(V(graph)[V(graph)$leftside == 1 & names(V(graph)) != "Ul"])
              chkpaths <- unlist(lapply(cond.vars, function(x){ 
                  pths <- all_simple_paths(graph, from = x, to = allnmes, mode = "out")
                  unlist(lapply(pths, function(pth) {
                      any(interven.vars %in% names(pth))
                      
                  }))
              }))
              
              if(any(!chkpaths)) {
                  error <- sprintf("Leftside variables %s not ancestors of intervention sets. Condition 6 violated.", 
                                   paste(names(chkpaths)[!chkpaths], collapse = ", "))
              }
              
          }
          }
          
          if("oper" %in% names(parsed.test) & any(!unlist(parsed.test$oper) %in% c("+", "-"))) {
            whoper <- unlist(parsed.test$oper)[!unlist(parsed.test$oper) %in% c("+", "-")]
            error <- sprintf("Operator '%s' not allowed!", whoper)
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
      
      withProgress(message = "Computing bounds", value = .1, expr = {
    
      obj <- analyze_graph(graphres, constraints, effectt = effecttext)
      
      
      incProgress(.4)
      if(obj$objective == "") {
        
        showNotification("Objective is NULL, nothing to optimize.", type = "error")
        "Error"
        
      } else {
      
      bounds.obs <- optimize_effect_2(obj)
      
      list(graphres = graphres, obj = obj, bounds.obs = bounds.obs, 
           constraints = constraints, effect = effecttext)
      
      }
      })
      
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
      graph <- igraphFromList()
      obsnames <- names(V(graph)[!names(V(graph)) %in% c("Ur", "Ul")])
      
      error <- NULL
      
      parsed.ctest <- tryCatch(parse_constraints(constrainttext, obsnames), error = function(e) "fail")
      if(!is.list(parsed.ctest)) {
        
        error <- "Unable to parse constraints!"
        
      } else {
        
        allnmes <- unique(c(parsed.ctest$leftout, parsed.ctest$rightout, 
                            gsub("=\\d+", "", c(parsed.ctest$leftcond, parsed.ctest$rightcond))))
        
        
        if(any(!parsed.ctest$operator %in% c("==", "<", ">", "<=", ">="))) {
          error <- "Operator not allowed!"
        }
        
        realnms <- names(V(graph))
        if(any(!allnmes %in% realnms & !is.na(suppressWarnings(as.numeric(allnmes))))) {
          
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
        
        if(is.list(b)) {
          
        
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
        
        variabletext <- sprintf("The bounds are reported in terms of parameters of the form %s, which represents the probability %s.", 
                                sampparm, probstate)
        
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
                 ui = fluidRow(
                   column(2, actionButton("downloadf", 
                                          "Exit and return objects to R", 
                                          style="background-color: #fb6970")), 
                   column(2, actionButton("latexme", 
                                          "Show latex code for bounds"))))
        
        
        }
        
    })
    
    
    
    observeEvent(input$downloadf, {
      
      print("click")
      b <- optimizeGraph()
      b$boundsFunction <- interpret_bounds(b$bounds.obs$bounds, b$obj$parameters)
      stopApp(b)
      
    })
    
    observeEvent(input$latexme, {
      
      b <- optimizeGraph()
      
      insertUI(selector = "#results", where = "afterEnd", 
               ui = div(fluidRow(column(12, h3("Latex code")), 
                                 column(12, pre(htmlOutput("latexCode")))
               ))
      )
      
      
      output$latexCode <- renderUI(p(latex_bounds(b$bounds.obs$bounds, b$obj$parameters)))
      
      
    })
    
    
}
