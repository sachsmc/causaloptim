## multiple connections are allowed from left to right. But nothing from right to left.
## only nodes on the left, all connections are assumed. Only observables on the left.
## parameters are the joint distribution of the observables on the right
## conditional on the observables on the right
## allow for monotonicity specification on the arrows
## allow for exposure marking on the graph. Then the default estimand is the total effect

fluidPage(
        # Web interface parameters:
        tags$head(
            tags$link(rel = "stylesheet",
                      type = "text/css",
                      href = "css/graph-creator.css")
        ),
        # Title:
        headerPanel("Causal Network Analysis and Optimization"),
        # Instructional text:
        helptextUI("helptext"),
        # Interactive DAG input GUI:
        dagUI(),
        # Return to R the drawn DAG with attributes:
        actionButton(
            inputId = "endbtn",
            label = "Exit and return graph object",
            style = "background-color: #fb6970"
        ),
        # Plot the augmented DAG with attributes:
        actionButton(
            inputId = "analyze",
            label = "Analyze the graph and go to next step",
            style = "background-color: #69fb82"
        ),
        # A link to open a page with further usage instructions:
        headerPanel(
            a(href = 'shinyapp.html',  ## this is automatically built from one of the vignettes
              target = '_blank',
              'Help and more information')
        ),
        # Web interface JavaScript for drawing digraphs:
        tags$script(src = "js/d3.v3.min.js"),
        tags$script(src = "js/graph-creator.js")
)

