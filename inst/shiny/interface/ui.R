## multiple connections are allowed from left to right. But nothing from right to left.
## only nodes on the left, all connections are assumed. Only observables on the left.
## parameters are the joint distribution of the observables on the right 
## conditional on the observables on the right
## allow for monotonicity specification on the arrows
## allow for exposure marking on the graph. Then the default estimand is the total effect

shinyUI(fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/graph-creator.css")
    ),
    headerPanel("Causal Network Specification"), 
    fluidRow(id = "helptext", 
             column(12, helpText("The graph is divided into a left side and a right side. Connections between the left and right sides must originate from the left. I.e, no connections from the right to left are allowed. On the left side, arbitrary connections are allowed and the model assumes that all nodes are observed and connected. On the right side, unmeasured variables are allowed, and the procedure assumes unmeasured confounding between all nodes. ")), 
             column(12, helpText("Shift+click to add nodes. Shift+drag to connect nodes. Click to select nodes/edges and press delete to remove. Click a node to select and then press 'u' to mark it as unobserved/latent or press 'y' to mark it as the outcome of interest, or press 'e' to mark it as the exposure of interest. The default causal contrast is the total effect. Click and edge and press 'm' to enforce monotonicity. "))
             ),
    fluidRow(id = "graphrow",
        column(12, tagList(tags$div(id="graph", style="height:640px"), 
                          tags$br()))
    ), 
    verbatimTextOutput("outcode"),
    actionButton("endbtn", "Press when finished to return graph object"),
    tags$script(src = "js/d3.v3.min.js"),
    tags$script(src = "js/graph-creator.js"), 
    tags$script(src = "js/FileSaver.min.js") 
))