## multiple connections are allowed from left to right. But nothing from right to left.
## only nodes on the left, all connections are assumed. Only observables on the left.
## parameters are the joint distribution of the observables on the right 
## conditional on the observables on the right
## allow for monotonicity specification on the arrows
## allow for exposure marking on the graph. Then the default estimand is the total effect

shinyUI(fluidPage(tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/graph-creator.css")
    ),
    headerPanel("Causal Network Analysis and Optimization"), 
    fluidRow(id = "helptext", 
             column(12, helpText("The graph is divided into a left side and a right side. There is no confounding allowed between the left and right sides. All variables on the right side are confounded. Connections between the left and right sides must originate from the left. I.e, no connections from the right to left are allowed. On the left side, arbitrary connections are allowed and the model assumes that all nodes are observed and connected. On the right side, unmeasured variables are allowed, and the procedure assumes unmeasured confounding between all nodes. Once you press 'Analyze the graph', the algorithm will automatically add common causes to each side. ")), 
             column(12, helpText("Shift+click to add nodes. Shift+drag to connect nodes. Click to select nodes/edges and press 'd' to remove. Click a node to select and then press 'u' to mark it as unobserved/latent or press 'y' to mark it as the outcome of interest, or press 'e' to mark it as the exposure of interest. Select a node and press a digit to set that number of possible categorical values (all variables default to binary), or press 'c' and enter a number into the prompt. Click an edge and press 'm' to enforce monotonicity for that connection. Other constraints can be specified later. "))
             ),
    fluidRow(id = "graphrow",
        column(12, 
               tagList(tags$div(id="nvalsModal", class="modal fade", role="dialog", 
                                tags$div(class="modal-dialog modal-dialog-centered", role="document", 
                                         tags$div(class="modal-content shadow", 
                                                  tags$div(class="modal-header", 
                                                           tags$h5(class="modal-title", "Enter number of values: ")
                                                           ), 
                                                  tags$div(class="modal-body", 
                                                           tags$input(id="nvalsInput", type="number", class="form-control", value=2, min=2)
                                                           ), 
                                                  tags$div(class="modal-footer", 
                                                           tags$button(type="button", class="btn btn-default", `data-dismiss`="modal", "Cancel"), 
                                                           tags$button(id="nvalsSubmit", type="button", class="btn btn-primary", `data-toggle`="modal", `data-target`="#nvalsModal", "Ok")))))), 
               tagList(tags$div(id="toast")), 
               tagList(tags$div(id="graph", style="height:480px"), 
                          tags$br()))
    ), 
    #verbatimTextOutput("outcode"), # for debugging
    actionButton("endbtn", "Exit and return graph object", style="background-color: #fb6970"),
    actionButton("analyze", "Analyze the graph", style="background-color: #69fb82"), 
    
    headerPanel(a(href='shinyapp.html', target='_blank', 'Help and more information')),
    
    tags$script(src = "js/d3.v3.min.js"),
    tags$script(src = "js/graph-creator.js") 
))