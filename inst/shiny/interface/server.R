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
            
            rightvars <- V(graphres)[V(graphres)$leftside == 0 & names(V(graphres)) != "Ur"]
            defaultOut <- names(V(graphres)[V(graphres)$outcome == 1])
            
            
            effectUI <- div(id = "effect", 
                                 h3("Specify causal effect of interest"), 
                            fluidRow(
                                 column(2, actionButton("addpo", "Add potential outcome")), 
                                 column(1, actionButton("addvar", "Add variable")), 
                                 column(2, actionButton("addpocond", "Add potential condition")),
                                 column(1, actionButton("addoper", "Add operator")), 
                                 column(1, actionButton("clear", "Reset"))
                                 ))
            
            varcounter <- reactiveValues(addvar = 0, addpocond = 0)
            
            insertUI(selector = "#myplot", 
                     where = "afterEnd", 
                     ui = list(effectUI,
                               fluidRow(id = "results", 
                                        h3("Constraints"),
                                        column(1, actionButton("constraints", "Specify constraints")),
                                        column(1, actionButton("optimize", "Compute the bounds"))
                               )
                     )
            )
            
            observeEvent(input$clear, {
              removeUI(".outrow", multiple = TRUE)
              varcounter$addvar <- 0
              varcounter$addpocond <- 0
              
            })
            
            observeEvent(input$addpo, {
              
              insertUI(selector = "#effect", "beforeEnd", 
                       ui = fluidRow(class="outrow", 
                         column(1, id = paste0("po.", input$addpo), 
                                selectInput(inputId=paste0("po.", input$addpo), 
                                            label = "", choices=names(rightvars), multiple = FALSE, selectize = FALSE, width = "80px")),
                         column(1, style= "padding-top: 20px", h4("(")), 
                         
                         column(1, id = paste0("end", input$addpo),  style= "padding-top: 20px", h4(")"))
                       ))
              
              varcounter$addvar <-  0
              varcounter$addpocond <- 0
              
            })
            
            observeEvent({
              input$addvar
            }, {
              
              varcounter$addvar <- varcounter$addvar + 1
              
              pout <- V(graphres)[names(V(graphres)) == input[[paste0("po.", input$addpo)]]]
              
              condsel <- names(adjacent_vertices(graphres, pout, mode = "in")[[1]])
              condsel <- condsel[!condsel %in% c("Ur", "Ul")]
              
              insertUI(selector = paste0("#end", input$addpo), "beforeBegin", 
                       ui = list(column(1, selectInput(inputId = paste0("po.", input$addpo, ".cond.", varcounter$addvar), 
                                                       label = "", choices = condsel, multiple = FALSE, selectize = FALSE, width = "80px")), 
                                 column(1, selectInput(inputId = paste0("po.", input$addpo, ".cond.", varcounter$addvar, ".val"), 
                                                       label = "=", choices = c("0", "1"), multiple = FALSE, selectize = FALSE, width = "80px")))
              )
              
            })
            
            observeEvent(input$addoper, {
              
              insertUI(selector = "#effect", "beforeEnd", 
                       ui = fluidRow(class = "outrow",
                         column(1, selectInput(inputId=paste0("oper.", input$addoper), 
                                            label = "", choices=c("-", "+"), multiple = FALSE, selectize = FALSE, width = "80px"))
                         
                       ))
              
            })
            
            
            observeEvent(input$addpocond, {
              
              varcounter$addpocond <- varcounter$addpocond + 1
              
              pout <- V(graphres)[names(V(graphres)) == input[[paste0("po.", input$addpo)]]]
              
              condsel <- names(adjacent_vertices(graphres, pout, mode = "in")[[1]])
              condsel <- condsel[!condsel %in% c("Ur", "Ul")]
              
              insertUI(selector = paste0("#end", input$addpo), "beforeBegin", 
                       ui = list(column(1, selectInput(inputId = paste0("po.", input$addpo, ".pocond.", varcounter$addpocond), 
                                                       label = "", choices = condsel, multiple = FALSE, selectize = FALSE, width = "80px")), 
                                 column(1, style= "padding-top: 20px", h4("(")), 
                                 column(1, id = paste0("po", input$addpo, "condend", varcounter$addpocond), style= "padding-top: 20px", h4(")")), 
                                 column(1, selectInput(inputId = paste0("po.", input$addpo, ".pocond.", varcounter$addpocond, ".val"), 
                                                       label = "=", choices = c("0", "1"), multiple = FALSE, selectize = FALSE, width = "80px"))
                                 ))
              

              
            })
            
            observeEvent(input[[paste0("po.", input$addpo, ".pocond.", varcounter$addpocond)]], {
              
              seldcond <- input[[paste0("po.", input$addpo, ".pocond.", varcounter$addpocond)]]
              pvout <- V(graphres)[names(V(graphres)) == seldcond]
              
              condsel2 <- names(adjacent_vertices(graphres, pvout, mode = "in")[[1]])
              condsel2 <- condsel2[!condsel2 %in% c("Ur", "Ul")]
              
              removeUI(selector = paste0("#po", input$addpo, "condcho", varcounter$addpocond), immediate = TRUE)
              removeUI(selector = paste0("#po", input$addpo, "condchov", varcounter$addpocond), immediate = TRUE)
              insertUI(selector = paste0("#po", input$addpo, "condend", varcounter$addpocond), where = "beforeBegin", 
                       ui = list(column(1, id = paste0("po", input$addpo, "condcho", varcounter$addpocond), 
                                        selectInput(paste0("po.", input$addpo, ".pocond.cond.", varcounter$addpocond), 
                                                    label = "", choices = condsel2, multiple = FALSE, selectize = FALSE, width = "80px")), 
                                 column(1, id = paste0("po", input$addpo, "condchov", varcounter$addpocond), 
                                        selectInput(inputId = paste0("po.", input$addpo, ".pocond.cond.", varcounter$addpocond, ".val"), 
                                                    label = "=", choices = c("0", "1"), multiple = FALSE, selectize = FALSE, width = "80px"))
                       ))
              
            }, ignoreNULL=TRUE, ignoreInit = FALSE)
            
    
            
            
            
        }
            
            })
            
            
                     
    
    optimizeGraph <- reactive({
      
      graphres <- igraphFromList()
      
      ## parse causal effect
      #stopApp(reactiveValuesToList(input))
      n.pos <- input$addpo
      n.oper <- input$addoper
      
      effectlist <- vector(mode = "list")
      for(i in 1:n.pos) {
        
        po.rele <- grep(paste0("^po\\.", i), names(input), value = TRUE)
        inlist <- vector(mode = "list")
        
        po.povars <- grep(paste0("^po\\.", i, "\\.pocond\\.[1-9]$"), names(input), value = TRUE)
        po.vars <- grep(paste0("^po\\.", i, "\\.cond\\.[1-9]$"), names(input), value = TRUE)
        
        jout <- 1
        if(length(po.vars) > 0) {
          
        for(j in 1:length(po.vars)) {
          
          inlist[[j]] <- input[[paste0(po.vars[j], ".val")]]
          names(inlist)[j] <- input[[po.vars[j]]]
          jout <- jout + 1
          
        }
          
        }
        
        if(length(po.povars) > 0) {
          
          for(j in 1:length(po.povars)) {
            
            inpolist <- vector(mode = "list")
            po.condpo.vars <- grep(paste0("^po\\.", i, "\\.pocond\\.cond\\.[1-9]$"), names(input), value = TRUE)
            for(k in 1:length(po.condpo.vars)) {
              
              inpolist[[k]] <- input[[paste0(po.condpo.vars[k], ".val")]]
              names(inpolist)[k] <- input[[po.condpo.vars[k]]]
              
            }
            inpolist[[k + 1]] <- input[[paste0("po.", i, ".pocond.", j, ".val")]]
            
            inlist[[jout]] <- inpolist
            names(inlist)[jout] <- input[[po.povars[j]]]
            jout <- jout + 1
            
          }
          
        }
        
        effectlist[[i]] <- inlist
        
        
      }
      
      names(effectlist) <- lapply(1:n.pos, function(i) input[[paste0("po.", i)]])
      
      if(n.oper > 0){
      operators <- lapply(1:n.oper, function(i) input[[paste0("oper.", i)]])
      } else operators <- NULL
      
      ## parse constraints
      
      if(input$constraints > 0) {
        
        constraints <- lapply(1:input$constraints, function(i) {
          
          thisconst.l <- paste0("varconstr.l.", i)
          thisconst.r <- paste0("varconstr.r.", i)
          
          thisin.l <- poutc$pout[[thisconst.l]]
          thisin.r <- poutc$pout[[thisconst.r]]
          
          lefton <- righton <- NULL
          for(j in 1:length(thisin.l$vary)) {
           
            lefton <- c(lefton, paste0(thisin.l$vary[j], " = ", input[[paste0(thisconst.l, "vcl.", thisin.l$vary[j])]]))
          
          }
          for(j in 1:length(thisin.r$vary)) {
            
            righton <- c(righton, paste0(thisin.r$vary[j], " = ", input[[paste0(thisconst.r, "vcr.", thisin.r$vary[j])]]))
            
          }
          oper <- input[[paste0("varconstr.", i, ".operator")]]
          
          paste0(thisin.l$fix, "(", paste(lefton, collapse = ", "), ") ", oper, " ", 
                 thisin.r$fix, "(", paste(righton, collapse = ", "), ")")
          
        })
        
        print(constraints)
        
      } else {
        
        constraints <- NULL
        
      }
    
      obj <- analyze_graph(graphres, constraints, effect = list(vars = effectlist, oper = operators))
      bounds.obs <- optimize_effect(obj)
      
      list(graphres = graphres, obj = obj, bounds.obs = bounds.obs, constraints = constraints, effect = list(vars = effectlist, oper = operators))
      
    })
 
    
    ### constraints
    
    nconstraints <- reactiveValues(nconst = NULL)
    
    observeEvent(input$constraints, {
      
      ## variables that have parents
      graphres <- igraphFromList()
      parentsof <- adjacent_vertices(graphres, V(graphres), mode = "in")
      tmpparent <- lapply(parentsof, function(x) {
        if(length(names(x)) == 0) return("")
        xn <- names(x)[!names(x) %in% c("Ul", "Ur")]
        paste0("(", paste(xn, collapse = ", "), ")")
        
      })
      
      
      potent.outs <- paste0(names(parentsof), tmpparent)
      potent.outs <- potent.outs[lapply(parentsof, length) > 0]
      
      insertUI(selector = "#results", where = "beforeBegin", 
               ui = div(h3(paste0("Constraint ", input$constraints)), 
                        selectInput(paste0("varconstr.l.", input$constraints), "Potential outcome (left) to constrain", 
                                    choices = potent.outs), 
                        selectInput(paste0("varconstr.r.", input$constraints), "Potential outcome (right) to constrain", 
                                    choices = potent.outs)
               )
      )
      nconstraints$nconst <- input$constraints
      
    })
    
    ## keep track of potential outcomes
    poutc <- reactiveValues(pout = NULL)
    
    observeEvent({ 
      
      dex <- nconstraints$nconst
      eval(parse(text = paste0("input$varconstr.l.", dex)))
      eval(parse(text = paste0("input$varconstr.r.", dex)))
      
      }, {
        
        constid.l <- paste0("varconstr.l.", nconstraints$nconst)
        thisconst.l <- input[[paste0("varconstr.l.", nconstraints$nconst)]]
        constid.r <- paste0("varconstr.r.", nconstraints$nconst)
        thisconst.r <- input[[paste0("varconstr.r.", nconstraints$nconst)]]
        
      
      proc.vars.l <- strsplit(gsub(")", "", thisconst.l, fixed = TRUE), "(", fixed = TRUE)[[1]]
      fix.l <- proc.vars.l[1]
      vary.l <- strsplit(proc.vars.l[-1], ", ", fixed = TRUE)[[1]]
      
      proc.vars.r <- strsplit(gsub(")", "", thisconst.r, fixed = TRUE), "(", fixed = TRUE)[[1]]
      fix.r <- proc.vars.r[1]
      vary.r <- strsplit(proc.vars.r[-1], ", ", fixed = TRUE)[[1]]
      
      
      poutc$pout[[constid.l]] <- list(fix = fix.l, vary = vary.l)
      poutc$pout[[constid.r]] <- list(fix = fix.r, vary = vary.r)
      
      ui0 <- lapply(vary.l, function(x){
            
            column(1, selectInput(paste0(constid.l, "vcl.", x), x, choices = c(x, "0", "1"), width = "80px"))
            
          })
      ui1 <- lapply(vary.r, function(x){
                   
                   column(1, selectInput(paste0(constid.r, "vcr.", x), x, choices = c(x, "0", "1"), width = "80px"))
                   
                 })
      
      removeUI(selector = paste0("#constrow", nconstraints$nconst), immediate = TRUE, multiple = TRUE)
      insertUI(selector = "#results", where = "beforeBegin", 
               ui = fluidRow(id = paste0("constrow", nconstraints$nconst), 
                        column(1, fix.l),
                        ui0, 
                        column(1, selectInput(paste0("varconstr.", nconstraints$nconst, ".operator"), "Operator", choices = c("=", "<", "\u2264", ">", "\u2265"), width = "80px")), 
                        column(1, fix.r), 
                        ui1
                        )
      )
      
    })
    
    
    observeEvent(input$optimize, {
        
        
      ## parse constraints
      
      
      ###
      
        b <- optimizeGraph()
        
        
        
        removeUI(selector = "#resultsText")
        insertUI(selector = "#results", where = "beforeEnd", 
                 ui = fluidRow(column(12, h3("Results")), 
                               column(1), 
                              column(11, htmlOutput("resultsText"))
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
        
        textres <- lapply(c(effecttext, totalpaths, constrainttext, variabletext, "Bounds: ",
                            b$bounds.obs$bounds[1], b$bounds.obs$bounds[2]), function(x) {
                   
                   x2 <- strsplit(x, "\n", fixed = TRUE)[[1]]
                   lapply(x2, function(x) tags$p(x))
                   
                 })
        
        
        output$resultsText <- renderUI(do.call(tagList, textres))
        
        insertUI(selector = "#results", where = "beforeEnd", 
                 ui = actionButton("downloadf", "Press to return objects to R"))
        
        
        
        
    })
    
    
    
    observeEvent(input$downloadf, {
      
      print("click")
      b <- optimizeGraph()
      b$boundsFunction <- interpret_bounds(b$bounds.obs$bounds, b$obj$parameters)
      stopApp(b)
      
    })
    
    
}
