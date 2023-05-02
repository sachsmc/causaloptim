function(input, output, session) {
    
    onStop(function(){ 
        rm(list = c("constraintschangeUI", "constraintUI", "dagUI", 
                    "edges_from_input", "effectUI", "graphres_from_edges", 
                    "helptextUI", "open_constraintinputUI", 
                    "open_finalUI", "open_latexUI", "open_newUI", 
                    "plotServer", "plotUI", 
                    "querychangeUI", "remove_oldUI", "resultschangeUI", "resultstext", 
                    "resultsUI"), 
           envir = .GlobalEnv
           )
        })
    
    ########################## Causal diagram input ##########################
    
    # Read directed graph input from GUI.
    # IN: input$edges
    # OUT: A data frame encoding a digraph with attributes.
    edgeList <- reactive({
        myin <- input$edges
        edges_from_input(myin)
    })
    
    # Construct igraph object from input.
    # IN: edgeList()
    # OUT: An igraph object encoding a digraph with attributes.
    igraphFromList <- reactive({
        edges <- edgeList()
        graphres_from_edges(edges = edges)
    })
    
    # Monitor restrictions for digraph.
    # IN: igraphFromList()
    observe({
        graphres <- igraphFromList()
        graphrescheck(graphres = graphres)
    })
    
    # Monitor "endbtn" action button ("Exit and return graph object").
    # IN: input$endbtn and igraphFromList()
    observeEvent(eventExpr = input$endbtn,
                 handlerExpr = {
                     graphres <- igraphFromList()
                     # Check conditions and return igraph object to R:
                     graphrescheck(graphres = graphres, ret = TRUE)
                 })
    
    # Monitor "analyze" action button ("Analyze the graph").
    # IN: input$analyze and igraphFromList()
    observeEvent(eventExpr = input$analyze,
                 handlerExpr = {
                     remove_oldUI()
                     graphres <- igraphFromList()
                     # Check conditions and plot igraph object in Shiny:
                     if (graphrescheck(graphres = graphres)) {
                         plotUI(id = "dagplot")
                         plotServer(id = "dagplot",
                                    graphres = graphres)
                         default.effect <-
                             get_default_effect(graphres = graphres)
                         open_newUI(default_effect = default.effect)
                     }
                 })
    
    ########################### Causal query input ###########################
    
    effectFixed <- reactiveValues()
    # Monitor "parseeffect" action button ("Parse").
    # IN: input$parseeffect and input$effect and igraphFromList() and effectFixed
    observeEvent(eventExpr = input$parseeffect,
                 handlerExpr = {
                     effecttext <- input$effect
                     graphres <- igraphFromList()
                     # Parse query and check conditions:
                     if (querycheck(effecttext = effecttext, graphres = graphres)) {
                         querychangeUI(effecttext = effecttext)
                         effectFixed$effectt <- effecttext
                     }
                 })
    
    #################### Optional extra constraints input ####################
    
    # Monitor "constraints" action button ("Specify constraints").
    # IN: input$constraints
    observeEvent(eventExpr = input$constraints,
                 handlerExpr = {
                     # Open constraints input field.
                     open_constraintinputUI()
                 })
    
    fixedConstraints <- reactiveValues()
    # Monitor "parseconstraint" action button ("Parse").
    # IN: input$parseconstraint and igraphFromList() and fixedConstraints
    observeEvent(eventExpr = input$parseconstraint,
                 handlerExpr = {
                     constrainttext <- strsplit(
                         x = input$constraintfield,
                         split = "\n",
                         fixed = TRUE
                     )[[1]]
                     graphres <- igraphFromList()
                     # Parse constraints:
                     if (constraintscheck(constrainttext = constrainttext, graphres = graphres)) {
                         constraintschangeUI(constrainttext = constrainttext)
                         fixedConstraints$constraintst <-
                             constrainttext
                     }
                 })
    
    ############################# Optimization #############################
    
    # Compute the bounds.
    # IN: igraphFromList() and effectFixed and fixedConstraints
    # OUT: A list of results including the causal problem and bounds.
    optimizeGraph <- reactive({
        graphres <- igraphFromList()
        effecttext <- effectFixed$effectt
        constraintstext <- fixedConstraints$constraintst
        withProgress(expr = {
            obj <- analyze_graph(
                graph = graphres,
                constraints = constraintstext,
                effectt = effecttext
            )
            incProgress(amount = .4)
            if (is.null(obj)) {
                showNotification(ui = "Objective is NULL, nothing to optimize.",
                                 type = "error")
                return("Error")
            }
            bounds.obs <- optimize_effect_2(obj = obj)
            # The following list is returned:
            list(
                graphres = graphres,
                obj = obj,
                bounds.obs = bounds.obs,
                constraints = constraintstext,
                effect = effecttext
            )
        },
        value = .1,
        message = "Computing bounds")
    })
    
    # Monitor "optimize" action button ("Compute the bounds").
    # IN: input$optimize and optimizeGraph() and effectFixed
    observeEvent(eventExpr = input$optimize,
                 handlerExpr = {
                     resultslist <- optimizeGraph()
                     if (is.list(resultslist)) {
                         # Display the bounds together with
                         # information about the causal problem
                         # and interpretation of the parameters:
                         resultschangeUI()
                         textres <-
                             resultstext(resultslist = resultslist,
                                         effecttext = effectFixed$effectt)
                         output$resultsText <-
                             renderUI(expr = do.call(tagList, textres))
                         open_finalUI()
                     }
                 })
    
    ########################### Return and LaTeX ###########################
    
    # Monitor "downloadf" action button ("Exit and return objects to R").
    # IN: input$downloadf and optimizeGraph()
    observeEvent(eventExpr = input$downloadf,
                 handlerExpr = {
                     print("click")
                     resultslist <- optimizeGraph()
                     resultslist$boundsFunction <- interpret_bounds(
                         bounds = resultslist$bounds.obs$bounds,
                         parameters = resultslist$obj$parameters
                     )
                     # Return results to R:
                     stopApp(resultslist)
                 })
    
    # Monitor "latexme" action button ("Show latex code for bounds").
    # IN: input$latexme and optimizeGraph()
    observeEvent(eventExpr = input$latexme,
                 handlerExpr = {
                     resultslist <- optimizeGraph()
                     # Display LaTeX code:
                     open_latexUI()
                     output$latexCode <- renderUI(expr = p(
                         latex_bounds(
                             bounds = resultslist$bounds.obs$bounds,
                             parameters = resultslist$obj$parameters
                         )
                     ))
                 })
}
