
# A UI that appears at the top of the Shiny app web interface, just beneath the title.
# Contains two paragraphs of instructional text.
#' Generate HTML for a UI displaying instructional text on method and usage.
#' @return HTML code for a UI with text.
#' @examples
#' helptextUI()
helptextUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    id = ns("helptext"),
    column(
      width = 10,
      h3("How does this work?"),
      helpText(
    "The first step is to draw a graph, but it cannot be just any graph, there are some important rules to follow: ",
        "The graph is divided into a left side and a right side. Within each side, all variables must be mutually confounded by unmeasured confounders. There must be at least 1 variable in the right side, but the left side can be empty.", 
        strong("But you do not have to draw the unmeasured confounders"), 
        ", once you press 'Analyze the graph', the algorithm will automatically add common causes to each side.",
        "Connections between the left and right sides must originate from the left. I.e, no connections from the right to left are allowed. The left side and right side variables are unconfounded. Generally speaking, you want to put outcome variables of interest on the right side, things like instruments or randomized treatments on the left side, and confounded exposures of interest on the right side. "
      )
    ),
    column(
      width = 10,
      h3("Getting started"),
      helpText(
        strong("To add new variables/nodes:"), "Hold shift and click where you want the node, then type a name for the node. Choose a short name, starting with a letter.", 
        br(), 
        strong("To draw a directed edge connecting two nodes."), "Hold shift, and use the mouse to click and drag from the first node to the second.", 
        br(),
        strong("Click on a node to select it, it will turn salmon colored."),
        br(),
        strong("If you made a mistake,"), "select a node and press 'd' to remove.",
        br(),
        strong("Select a node, then use the keyboard to mark it with special information (optional):"), 
        br(),
        "- press 'u' to mark it as unobserved/latent", 
        br(),
        "- press 'y' to mark it as the outcome of interest", 
        br(),
        "- press 'e' to mark it as the exposure of interest", 
        br(),
        "- select a node and press a digit to set that number of possible categorical values (all variables default to binary), or press 'c' and enter a number into the prompt.",
        br(),
        "Click an edge and press 'm' to enforce monotonicity for that connection. Other constraints can be specified later. "
      )
    )
  )
}

# A UI for drawing an appropriate DAG. Appears just beneath helptextUI.
# Divided into a left side and a right side.
#' Generate HTML for a UI for GUI digraph input.
#' @return HTML code for a UI for digraph input.
#' @examples
#' dagUI()
dagUI <- function() {
  fluidRow(
    id = "graphrow",
    column(
      width = 12,
      tagList(
        tags$div(
          id = "nvalsModal",
          class = "modal fade",
          role = "dialog",
          tags$div(
            class = "modal-dialog modal-dialog-centered",
            role = "document",
            tags$div(
              class = "modal-content shadow",
              tags$div(
                class = "modal-header",
                tags$h5(
                  class = "modal-title",
                  "Enter number of values: "
                )
              ),
              tags$div(
                class = "modal-body",
                tags$input(
                  id = "nvalsInput",
                  type = "number",
                  class = "form-control",
                  value = 2,
                  min = 2
                )
              ),
              tags$div(
                class = "modal-footer",
                tags$button(
                  type = "button",
                  class = "btn btn-default",
                  `data - dismiss` = "modal",
                  "Cancel"
                ),
                tags$button(
                  id = "nvalsSubmit",
                  type = "button",
                  class = "btn btn-primary",
                  `data - toggle` = "modal",
                  `data - target` = "#nvalsModal",
                  "Ok"
                )
              )
            )
          )
        )
      ),
      tagList(tags$div(id = "toast")),
      tagList(
        tags$div(
          id = "graph",
          style = "height:480px"
        ),
        tags$br()
      )
    )
  )
}

# A UI for textual input of causal query.
# Contains instructional text, a text input field for query and a button for parsing it.
# Appears beneath just the plotted DAG once the 'analyze' button is pressed.
# Takes as argument a string representing the default effect, as in
# effectUI(default_effect = "p{Y(X = 1)=1} - p{Y(X = 0)=1}")
# if exposure and outcome has been set inn GUI,
# else default_effect = "".
#' Generate HTML for a UI for textual input of causal query.
#' @param default_effect A string representing a causal query.
#' @return HTML code for a text field with default value \code{default_effect} 
#' and an action button for parsing it.
#' @examples
#' effectUI(default_effect = "p{Y(X = 1)=1} - p{Y(X = 0)=1}")
#' effectUI(default_effect = "")
effectUI <- function(default_effect) {
  div(
    id = "effect",
    h3("Specify causal effect of interest (required)"),
    helpText(
      "Use the text box to describe your causal effect of interest.", br(),
      "The effects must be described using sums or differences of probability statements involving counterfactuals, for example 'p{Y(X = 1) = 1} - p{Y(X = 0) = 1}'. ", 
      br(), 
      "You must describe them using only the names of variables in the graph, and numeric values in the appropriate range of the categorical variable (e.g., if Y has 4 levels, then the possibly values it takes on is 0, 1, 2, 3)." ,
      br(), 
      "You can specify complex queries, but note that the probability statements begin with little p, and use curly braces, and events inside the probability statements are separated by ';', e.g., 'p{Y(X = 1) = 1; X(Z = 1) = 1}'.",
      br(),
      "The variables may be potential outcomes which are denoted using smooth parentheses, and they can be arbitrarily nested.", 
      br(), 
      strong("There are 2 important rules to follow:"), "1) Only variables on the right side can be in the probability events, and if the left side is not empty: 2) none of the variables in the left side that are intervened upon can have any children in the left side, and all paths from the left to the right must be blocked by the intervention set. Here the intervention set is anything that is inside the smooth brackets (i.e., variable set to values)."
    ),
    fluidRow(
      id = "effecttext",
      column(
        width = 8,
        textAreaInput(
          inputId = "effect",
          label = NULL,
          value = default_effect
        )
      ),
      column(
        width = 1,
        actionButton(
          inputId = "parseeffect",
          label = "Parse",
          style = "background-color: #69fb82"
        )
      )
    )
  )
}

# A UI for optional input of extra constraints.
# Contains instructional text and a button to open a text input field for constraints.
# Appears just beneath effectUI once the 'analyze' button is pressed.
#' Generate HTML for a UI for textual input of optional extra constraints.
#' @return HTML code for a text input field and an action button for parsing.
#' @examples
#' constraintUI()
constraintUI <- function() {
  div(
    id = "constraintsdiv",
    h3("Constraints (optional)"),
    helpText(
      "Here you can specify potential outcomes to constrain by writing the potential outcomes, values of their parents, and operators that determine the constraint (equalities or inequalities). For example, X(Z = 1) >= X(Z = 0)."
    ),
    fluidRow(column(
      width = 1,
      actionButton(
        inputId = "constraints",
        label = "Specify constraints"
      )
    ))
  )
}

# Opens a UI for text input of optional additional user-provided constraints.
# Contains a text input field for constraints and a button for parsing them.
# Appears just beneath constraintUI if the 'constraints' button is pressed.
#' Dynamically open a new UI for textual input of optional additional constraints.
open_constraintinputUI <- function() {
  if (!isRunning()) {
    return(NULL)
  }
  insertUI(
    selector = "#constraintsdiv",
    where = "beforeEnd",
    ui = div(
      h3("Constraints"),
      fluidRow(
        id = "constrainttext",
        column(
          width = 8,
          textAreaInput(
            inputId = "constraintfield",
            label = NULL
          )
        ),
        column(
          width = 1,
          actionButton(
            inputId = "parseconstraint",
            label = "Parse",
            style = "background-color: #69fb82"
          )
        )
      )
    )
  )
}

# A UI for computing the bounds.
# Contains a message and a button to start the computation and display the bounds.
# Appears just beneath constraintUI once the 'analyze' button is pressed.
#' Generate HTML for a UI with an action button for computing bounds for a causal problem.
#' @return HTML code with a text message and an action button for computing bounds.
#' @examples
#' resultsUI()
resultsUI <- function() {
  div(
    id = "results",
    h3("Bounds"),
    helpText(
      "Note that the computation may take a significant amount of time."
    ),
    fluidRow(column(
      width = 1,
      actionButton(
        inputId = "optimize",
        label = "Compute the bounds",
        style = "background-color: #69fb82"
      )
    ))
  )
}

# Removes certain UI elements once the 'analyze' button is pressed.
#' Dynamically remove parts of the UI after clicking the 'analyze' action button. 
remove_oldUI <- function() {
  if (!isRunning()) {
    return(NULL)
  }
  removeUI(selector = "#effect")
  removeUI(selector = "#constraintsdiv")
  removeUI(selector = "#myplot")
  removeUI(selector = "#results")
  removeUI(selector = "#toast")
  removeUI(selector = "#nvalsModal")
}

# Opens a UI to display the plot of the augmented DAG once the 'analyze' button is pressed.
#' Dynamically open a new UI to display a plot of the DAG to be optimized over.
#' @param id A Shiny module identifier. Should match that of \code{plotServer}.
plotUI <- function(id) {
  if (!isRunning()) {
    return(NULL)
  }
  insertUI(
    selector = "#analyze",
    where = "afterEnd",
    ui = tags$div(
      id = "myplot",
      plotOutput(
        outputId = NS(
          namespace = id,
          id = "myplot"
        )
      )
    )
  )
}

# A server function that renders the plot of the augmented DAG in plotUI.
# Takes an 'igraph' object 'graphres' as argument, which may be an empty graph.
#' A Shiny module server to go with \code{plotUI}.
#' Renders to \code{plotUI} a plot of the DAG of the causal problem.
#' @param id A Shiny module identifier. Should match that of \code{plotUI}.
#' @param graphres An \code{igraph} object representing the DAG.
plotServer <- function(id, graphres) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      output$myplot <- renderPlot(plot_graphres(graphres))
    }
  )
}

# Opens the UIs effectUI, constraintUI and resultsUI once the 'analyze' button is pressed.
# Takes a string argument to pre-populate the causal query text input field.
#' Dynamically open a new UI for causal query entry and parsing, 
#' as well as UIs for optional extra constraints 
#' and subsequent display of results.
#' @param default_effect A string representing a causal query.
#' This parameter is expected to be returned by \code{get_default_effect(graphres)}, 
#' where \code{graphres} is an \code{igraph} representation of the DAG of interest.
#' This parameter will be non-empty if in the DAG GUI 
#' one vertex has been set as the exposure and another as the outcome, 
#' in which case it will be the total causal risk difference 
#' comparing the probability that the outcome is \code{1} 
#' if the exposure had been intervened on to be \code{1} 
#' against if the exposure had instead been set to \code{0}.
open_newUI <- function(default_effect) {
  if (!isRunning()) {
    return(NULL)
  }
  insertUI(
    selector = "#myplot",
    where = "afterEnd",
    ui = list(
      effectUI(default_effect = default_effect),
      constraintUI(),
      resultsUI()
    )
  )
}

# Changes the UI once the query parsing button is pressed,
# after which the query is fixed, if parsing successful.
#' Dynamically change the UI for entry of causal query.
#' @param effecttext A string representing a causal query.
#' This string will be the default value in the text input field for the causal query.
querychangeUI <- function(effecttext) {
  if (!isRunning()) {
    return(NULL)
  }
  removeUI(
    selector = "#effecttext",
    immediate = TRUE
  )
  insertUI(
    selector = "#effect",
    where = "beforeEnd",
    ui = fluidRow(column(
      width = 8,
      pre(effecttext)
    ))
  )
}

# Changes the UI once the constraints parsing button is pressed,
# after which the constraint is fixed.
#' Dynamically change the UI for input of optional constraints.
#' This fixes the constraint entered into the constraint text input field, 
#' once it has been successfully parsed.
#' @param constrainttext A string representing the user-provided constraint.
constraintschangeUI <- function(constrainttext) {
  if (!isRunning()) {
    return(NULL)
  }
  removeUI(
    selector = "#constrainttext",
    immediate = TRUE
  )
  insertUI(
    selector = "#constraintsdiv",
    where = "beforeEnd",
    ui = fluidRow(column(
      width = 8,
      pre(
        paste(constrainttext,
          collapse = "\n"
        )
      )
    ))
  )
}

# Changes the UI once the 'optimize' button is pressed.
# Opens a text output field the display the bounds, together with
# information on the causal problem and parameter interpretation.
#' Dynamically change the UI for display of results of optimized bounds.
resultschangeUI <- function() {
  if (!isRunning()) {
    return(NULL)
  }
  removeUI(selector = "#resultsText")
  insertUI(
    selector = "#results",
    where = "beforeEnd",
    ui = div(fluidRow(
      column(
        width = 12,
        h3("Results")
      ),
      column(
        width = 12,
        pre(htmlOutput(outputId = "resultsText"))
      )
    ))
  )
}

# Opens a UI with a button to download the results to R,
# and a button to display LaTeX code for the bounds.
# Once the bounds have been computed and displayed textually,
# this UI appears beneath the textual display of the bounds.
#' Dynamically open a new UI with two action buttons; 
#' one for downloading the results to R and
#' one for displaying LaTeX code for the bounds.
open_finalUI <- function() {
  if (!isRunning()) {
    return(NULL)
  }
  insertUI(
    selector = "#results",
    where = "beforeEnd",
    ui = fluidRow(
      column(
        width = 2,
        actionButton(
          inputId = "downloadf",
          label = "Exit and return objects to R",
          style = "background-color: #fb6970"
        )
      ),
      column(
        width = 2,
        actionButton(
          inputId = "latexme",
          label = "Show latex code for bounds"
        )
      )
    )
  )
}

# Opens a UI for textual display of LaTeX code for the bounds.
#' Dynamically open a new UI for display of LaTeX code for the computed bounds.
open_latexUI <- function() {
  if (!isRunning()) {
    return(NULL)
  }
  insertUI(
    selector = "#results",
    where = "afterEnd",
    ui = div(fluidRow(
      column(
        width = 12,
        h3("Latex code")
      ),
      column(
        width = 12,
        pre(htmlOutput(outputId = "latexCode"))
      )
    ))
  )
}

# Utility functions

#' Construct a data-frame encoding a digraph from GUI input.
#' @param myin Digraph input from GUI.
#' @return A data frame encoding the digraph.
edges_from_input <- function(myin) {
    if (length(myin) == 0) {
        return(NULL)
    }
    j <- seq(1, length(myin) - 18, by = 19)
    edges <- data.frame(
        id = paste0("e", myin[j]),
        source = myin[j + 1],
        target = myin[j + 2],
        source.leftside = ifelse(myin[j + 3] == "FALSE", 0, 1),
        target.leftside = ifelse(myin[j + 4] == "FALSE", 0, 1),
        rlconnect = ifelse(myin[j + 5] == "FALSE", 0, 1),
        source.latent = as.numeric(myin[j + 6]),
        target.latent = as.numeric(myin[j + 7]),
        source.outcome = as.numeric(myin[j + 8]),
        target.outcome = as.numeric(myin[j + 9]),
        source.exposure = as.numeric(myin[j + 10]),
        target.exposure = as.numeric(myin[j + 11]),
        source.nvals = as.numeric(myin[j + 12]),
        target.nvals = as.numeric(myin[j + 13]),
        edge.monotone = as.numeric(myin[j + 14]),
        source.x = as.numeric(myin[j + 15]),
        source.y = as.numeric(myin[j + 16]),
        target.x = as.numeric(myin[j + 17]),
        target.y = as.numeric(myin[j + 18])
    )
    edges
}

#' Construct an igraph-object encoding a digraph from a data frame.
#' @param edges A data frame encoding a digraph, as output by edges_from_input.
#' @return An igraph object encoding the digraph.
graphres_from_edges <- function(edges) {
    if (is.null(edges)) {
        empty_graphres <- make_empty_graph()
        return(empty_graphres)
    }
    edges <- edges[, -c(1)]
    vertex.meta <-
        rbind(
            data.frame(
                vnames = edges$source,
                leftside = edges$source.leftside,
                latent = edges$source.latent,
                outcome = edges$source.outcome,
                exposure = edges$source.exposure,
                nvals = edges$source.nvals,
                x = edges$source.x,
                y = -edges$source.y
            ),
            data.frame(
                vnames = edges$target,
                leftside = edges$target.leftside,
                latent = edges$target.latent,
                outcome = edges$target.outcome,
                exposure = edges$target.exposure,
                nvals = edges$target.nvals,
                x = edges$target.x,
                y = -edges$target.y
            )
        )
    graphres <-
        graph_from_data_frame(d = edges[, c(1, 2, 5, 14)], vertices = unique(vertex.meta))
    ogleft <- V(graphres)[V(graphres)$leftside == 1]
    ogright <- V(graphres)[V(graphres)$leftside == 0]
    if (length(ogleft) > 1) {
        graphres <- add_vertices(
            graph = graphres,
            nv = 1,
            name = "Ul",
            latent = 1,
            leftside = 1,
            outcome = 0,
            exposure = 0,
            nvals = 2,
            x = min(V(graphres)$x) - 100,
            y = min(V(graphres)$y) + 20
        )
        graphres <- add_edges(
            graph = graphres,
            edges = unlist(lapply(names(ogleft), function(x) {
                c("Ul", x)
            })),
            rlconnect = rep(0, length(ogleft)),
            edge.monotone = rep(0, length(ogleft))
        )
    }
    if (length(ogright) > 1) {
        graphres <- add_vertices(
            graph = graphres,
            nv = 1,
            name = "Ur",
            latent = 1,
            leftside = 0,
            outcome = 0,
            exposure = 0,
            nvals = 2,
            x = max(V(graphres)$x) + 100,
            y = min(V(graphres)$y) + 20
        )
        graphres <- add_edges(
            graph = graphres,
            edges = unlist(lapply(names(ogright), function(x) {
                c("Ur", x)
            })),
            rlconnect = rep(0, length(ogright)),
            edge.monotone = rep(0, length(ogright))
        )
    }
    graphres
}

# Construct and return a string containing the results of a causal problem.
# resultslist: A list of results as returned by 'specify_graph'.
#' Generate a string describing the results of a linear causal problem.
#' @param resultslist A list of results, as returned by \code{specify_graph()}.
#' @param effecttext A string representing a causal query.
#' @return HTML code with text describing the bounds from a linear causal problem.
resultstext <- function(resultslist, effecttext) {
    effecttext <- sprintf(
        "Computed bounds for the effect %s",
        effecttext
    )
    lkey <-
        letters[1:length(attr(resultslist$obj$parameters, "rightvars"))]
    rkey <- letters[(length(attr(
        resultslist$obj$parameters, "rightvars"
    )) + 1):(length(attr(
        resultslist$obj$parameters, "rightvars"
    )) +
        length(attr(
            resultslist$obj$parameters, "condvars"
        )))]
    if (length(attr(resultslist$obj$parameters, "condvars")) == 0) {
        rkey <- NULL
    }
    sampparm <- paste0(
        "p",
        paste(lkey,
              collapse = ""
        ),
        "_",
        paste(rkey,
              collapse = ""
        )
    )
    probstate <- paste0(
        "P(",
        paste(
            paste0(
                attr(resultslist$obj$parameters, "rightvars"),
                " = ",
                lkey
            ),
            collapse = ", "
        ),
        " | ",
        paste0(
            attr(resultslist$obj$parameters, "condvars"),
            " = ",
            rkey,
            collapse = ", "
        ),
        ")"
    )
    if (length(attr(resultslist$obj$parameters, "condvars")) == 0) {
        probstate <- paste0(
            "P(",
            paste(
                paste0(
                    attr(resultslist$obj$parameters, "rightvars"),
                    " = ",
                    lkey
                ),
                collapse = ", "
            ),
            ")"
        )
    }
    variabletext <- sprintf(
        "The bounds are reported in terms of parameters of the form %s, which represents the probability %s.",
        sampparm,
        probstate
    )
    if (!is.null(resultslist$constraints)) {
        constrainttext <- sprintf(
            "This following constraints have been specifed: \n %s",
            paste(resultslist$constraints,
                  collapse = "\n"
            )
        )
    } else {
        constrainttext <- "No constraints have been specified"
    }
    textres <- lapply(
        X = c(
            effecttext,
            constrainttext,
            variabletext,
            "Bounds: ",
            "Lower = ",
            resultslist$bounds.obs$bounds[1],
            "Upper = ",
            resultslist$bounds.obs$bounds[2]
        ),
        FUN = function(x) {
            x2 <- strsplit(
                x = x,
                split = "\n",
                fixed = TRUE
            )[[1]]
            lapply(
                X = x2,
                FUN = function(x) {
                    tags$p(x)
                }
            )
        }
    )
}
