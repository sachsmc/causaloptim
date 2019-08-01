shinyUI(fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/graph-creator.css")
    ),
    headerPanel("Causal Network Specification"), 
    fluidRow(id = "helptext", 
             column(12, helpText("The graph is divided into a left side and a right side. Only one vertex is allowed to connect the two sides and it must come from
                                 a node on the left, into a node on the right. On the left side, arbitrary connections are allowed. On the right side, the subgraph
                                 must be ...")), 
             column(12, helpText("Shift+click to add nodes. Shift+drag to connect nodes. Click to select nodes/edges and press delete to remove. Click a node to select and then press 'u' to mark it as unobserved/latent or press 'y' to mark it as the outcome of interest."))
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