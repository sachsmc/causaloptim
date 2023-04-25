# UIs

# A UI that appears at the top of the Shiny app web interface, just beneath the title.
# Contains two paragraphs of instructional text.
helptextUI <- function() {
  fluidRow(
    id = "helptext",
    column(
      width = 12,
      helpText(
        "The graph is divided into a left side and a right side. There is no confounding allowed between the left and right sides. All variables on the right side are confounded. Connections between the left and right sides must originate from the left. I.e, no connections from the right to left are allowed. On the left side, arbitrary connections are allowed and the model assumes that all nodes are observed and connected. On the right side, unmeasured variables are allowed, and the procedure assumes unmeasured confounding between all nodes. Once you press 'Analyze the graph', the algorithm will automatically add common causes to each side. "
      )
    ),
    column(
      width = 12,
      helpText(
        "Shift+click to add nodes. Shift+drag to connect nodes. Click to select nodes/edges and press 'd' to remove. Click a node to select and then press 'u' to mark it as unobserved/latent or press 'y' to mark it as the outcome of interest, or press 'e' to mark it as the exposure of interest. Select a node and press a digit to set that number of possible categorical values (all variables default to binary), or press 'c' and enter a number into the prompt. Click an edge and press 'm' to enforce monotonicity for that connection. Other constraints can be specified later. "
      )
    )
  )
}

# A UI for drawing an appropriate DAG. Appears just beneath helptextUI.
# Divided into a left side and a right side.
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
effectUI <- function(default_effect) {
  div(
    id = "effect",
    h3("Specify causal effect of interest (required)"),
    helpText(
      "Use the text box to describe your causal effect of interest. The effects must be of the form p{V11(X=a)=a; V12(X=a)=b;...; W1=a; ...} op1 p{V21(X=b)=a; V22(X=c)=b;...; W1=b} op2 ... where Vij and Wk are names of variables in the graph, a, b are numeric values in the appropriate value sets, and op are either - or +. You can specify a single probability statement (i.e., no operator). Note that the probability statements begin with little p, and use curly braces, and items inside the probability statements are separated by ;. The variables may be potential outcomes which are denoted by parentheses, and if there is nothing on the left side, they may also be observed outcomes which do not have parentheses. Variables may also be nested inside potential outcomes."
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
#' @export
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

# Check for right to left edges.
# edges: A 'data.frame' as output by 'edges_from_input'.
rlcheck0 <- function(edges) {
  if (sum(edges$rlconnect) > 0) {
    error_message <- "No connections from right to left are allowed!"
    if (isRunning()) {
      showNotification(
        ui = error_message,
        type = "error"
      )
    } else {
      print(error_message)
    }
    return(FALSE)
  }
  TRUE
}

#' Construct an igraph-object encoding a digraph from a data frame.
#' @param edges A data frame encoding a digraph, as output by edges_from_input.
#' @return An igraph object encoding the digraph.
#' @export
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

# Check for right side to left side edges in a digraph.
# graphres: An 'igraph' object as e.g. output by 'graphres_from_edges'.
rlcheck <- function(graphres) {
  edges <- E(graph = graphres)
  rlcheck0(edges = edges)
}

# Check that vertices are named appropriately.
vertexnamecheck <- function(graphres) {
  vnames <- names(V(graphres))
  badnames <- grep(
    pattern = "(^[^[:alpha:]])|([[:punct:]])|(^p)",
    x = vnames,
    value = TRUE
  )
  if (length(badnames) > 0) {
    error_message <- sprintf(
      "Invalid names: %s, found in graph vertices!",
      paste(badnames,
        collapse = ","
      )
    )
    if (isRunning()) {
      showNotification(
        ui = error_message,
        type = "error"
      )
    } else {
      print(error_message)
    }
    return(FALSE)
  }
  TRUE
}

# Check that the digraph is acyclic.
cyclecheck <- function(graphres) {
  if (is.dag(graph = graphres)) {
    return(TRUE)
  }
  error_message <- "No cycles in the graph are allowed!"
  if (isRunning()) {
    showNotification(
      ui = error_message,
      type = "error"
    )
  } else {
    print(error_message)
  }
  FALSE
}

# Check that each categorical variable is at least dichotomous.
nvalscheck <- function(graphres) {
  if (any(vertex_attr(graph = graphres)$nvals < 2)) {
    error_message <-
      "Each variable needs to be able to take on at least two distinct possible values!"
    if (isRunning()) {
      showNotification(
        ui = error_message,
        type = "error"
      )
    } else {
      print(error_message)
    }
    return(FALSE)
  }
  TRUE
}

# Check all conditions on the digraph.
# Set 'ret = TRUE' to also return 'graphres' if all checks are passed.
graphrescheck <- function(graphres, ret = FALSE) {
  if (rlcheck(graphres = graphres)) {
    if (cyclecheck(graphres = graphres)) {
      if (vertexnamecheck(graphres = graphres)) {
        if (nvalscheck(graphres = graphres)) {
          if (ret) {
            if (isRunning()) {
              stopApp(returnValue = graphres)
            }
            return(graphres)
          }
          return(TRUE)
        }
      }
    }
  }
  FALSE
}

# Check that causal query is parsable.
# effecttext: A string, e.g., "p{Y(X = 1)=1} - p{Y(X = 0)=1}".
queryparsecheck <- function(effecttext) {
  parsed.test <- tryCatch(
    expr = parse_effect(text = effecttext),
    error = function(e) {
      "fail"
    }
  )
  if (!is.list(parsed.test)) {
    return(FALSE)
  }
  TRUE
}

# Check that the query 'effecttext' can be parsed and that
# the causal problem (effecttext, graphres) satisfies
# the conditions on the query / intervention-set.
querycheck <- function(effecttext, graphres) {
  # Check parsability
  if (!queryparsecheck(effecttext = effecttext)) {
    error_message <- "Unable to parse effect!"
    if (isRunning()) {
      showNotification(
        ui = error_message,
        type = "error"
      )
    } else {
      print(error_message)
    }
    return(FALSE)
  }

  parsed_effect <- parse_effect(text = effecttext)

  chk0 <- lapply(parsed_effect$vars, btm_var)
  interven.vars <- unique(unlist(chk0))
  allnmes <-
    unique(c(interven.vars, unlist(lapply(
      parsed_effect$vars, names
    ))))

  # Check variable names
  realnms <- names(V(graphres))
  if (any(!allnmes %in% realnms)) {
    error_message <-
      sprintf(
        "Names %s in effect not specified in graph!",
        paste(allnmes[which(!allnmes %in% realnms)],
          collapse = ", "
        )
      )
    if (isRunning()) {
      showNotification(
        ui = error_message,
        type = "error"
      )
    } else {
      print(error_message)
    }
    return(FALSE)
  }

  ## Check that children of intervention set are on the right
  any.children.onleft <- sapply(interven.vars, function(v) {
    children <- neighbors(
      graph = graphres,
      v = V(graphres)[v],
      mode = "out"
    )
    any(children$leftside == 1)
  })
  if (any(any.children.onleft)) {
    error_message <- sprintf(
      "Cannot intervene on %s because it has children on the leftside!",
      paste(interven.vars[which(any.children.onleft)],
        collapse = ", "
      )
    )
    if (isRunning()) {
      showNotification(
        ui = error_message,
        type = "error"
      )
    } else {
      print(error_message)
    }
    return(FALSE)
  }

  # If left side contains variables, they must be ancestors of intervention set
  if (any(V(graphres)$leftside == 1 &
    names(V(graphres)) != "Ul")) {
    cond.vars <-
      names(V(graphres)[V(graphres)$leftside == 1 &
        names(V(graphres)) != "Ul"])
    chkpaths <- unlist(lapply(cond.vars, function(x) {
      pths <- all_simple_paths(
        graph = graphres,
        from = x,
        to = allnmes,
        mode = "out"
      )
      unlist(lapply(pths, function(pth) {
        any(interven.vars %in% names(pth))
      }))
    }))
    if (any(!chkpaths)) {
      error_message <- sprintf(
        "Leftside variables %s not ancestors of intervention sets. Condition 6 violated.",
        paste(names(chkpaths)[!chkpaths],
          collapse = ", "
        )
      )
      if (isRunning()) {
        showNotification(
          ui = error_message,
          type = "error"
        )
      } else {
        print(error_message)
      }
      return(FALSE)
    }
  }

  # Check operation validity
  if ("oper" %in% names(parsed_effect) &
    any(!unlist(parsed_effect$oper) %in% c("+", "-"))) {
    whoper <-
      unlist(parsed_effect$oper)[!unlist(parsed_effect$oper) %in% c("+", "-")]
    error_message <- sprintf(
      "Operator '%s' not allowed!",
      whoper
    )
    error_message <- "Unable to parse effect!"
    if (isRunning()) {
      showNotification(
        ui = error_message,
        type = "error"
      )
    } else {
      print(error_message)
    }
    return(FALSE)
  }
  TRUE
}

# Check parsability of constraints.
# constrainttext: A string, e.g., "X(Z = 1) >= X(Z = 0)".
constraintsparsecheck <- function(constrainttext, graphres) {
  obsnames <-
    names(V(graphres)[!names(V(graphres)) %in% c("Ur", "Ul")])
  parsed.ctest <- tryCatch(
    expr = parse_constraints( # Note: does not validate!
      constraints = constrainttext,
      obsnames = obsnames
    ),
    error = function(e) {
      "fail"
    }
  )
  if (!is.list(parsed.ctest)) {
    showNotification(
      ui = "Unable to parse constraints!",
      type = "error"
    )
    return(FALSE)
  }
  TRUE
}

# Check that the variables in a parsed constraint actually appear in the DAG.
# parsed_constraints: A 'data.frame' with 5 columns, e.g.,
# leftout rightout operator leftcond rightcond
# 1       X        X       >=      Z=1       Z=0
# as output by 'parse_constraints' with "X(Z = 1) >= X(Z = 0)".
constraintsnamecheck <- function(parsed_constraints, graphres) {
  allnmes <- unique(c(
    parsed_constraints$leftout,
    parsed_constraints$rightout,
    gsub(
      pattern = "=\\d+",
      replacement = "",
      x = c(
        parsed_constraints$leftcond,
        parsed_constraints$rightcond
      )
    )
  ))
  realnms <- names(V(graphres))
  if (any(!allnmes %in% realnms &
    !is.na(suppressWarnings(expr = as.numeric(allnmes))))) {
    error_message <-
      sprintf(
        "Names %s in constraint not specified in graph!",
        paste(allnmes[which(!allnmes %in% realnms)],
          collapse = ", "
        )
      )
    showNotification(
      ui = error_message,
      type = "error"
    )
    return(FALSE)
  }
  TRUE
}

# Check validity of relations in parsed constraint.
constraintsoperatorcheck <- function(parsed_constraints) {
  if (any(!parsed_constraints$operator %in% c("==", "<", ">", "<=", ">="))) {
    showNotification(
      ui = "Operator not allowed!",
      type = "error"
    )
    return(FALSE)
  }
  TRUE
}

# A complete check of user-provided constraint.
constraintscheck <- function(constrainttext, graphres) {
  if (constraintsparsecheck(constrainttext = constrainttext, graphres = graphres)) {
    obsnames <-
      names(V(graphres)[!names(V(graphres)) %in% c("Ur", "Ul")])
    parsed_constraints <-
      parse_constraints( # Note: does not validate!
        constraints = constrainttext,
        obsnames = obsnames
      )
    if (constraintsnamecheck(parsed_constraints = parsed_constraints, graphres = graphres)) {
      if (constraintsoperatorcheck(parsed_constraints = parsed_constraints)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

# Construct and return a string containing the results of a causal problem.
# resultslist: A list of results as returned by 'specify_graph'.
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

#' Check conditions on causal problem
#'
#' Check that a given causal problem (a causal DAG together with a causal query)
#' satisfies conditions that guarantee that the optimization problem is linear.
#'
#' @param digraph An \code{igraph} object representing a digraph.
#'
#' Expected vertex attributes: \code{leftside}, \code{latent} and \code{nvals}.
#'
#' Optional vertex attributes: \code{exposure} and \code{outcome}.
#'
#' Expected edge attributes: \code{rlconnect} and \code{edge.monotone}.
#'
#' @param query A string representing a causal query / effect.
#'
#' @return \code{TRUE} if conditions are met; \code{FALSE} otherwise.
#'
#' @export
#'
#' @examples
#' b <- graph_from_literal(X - +Y, Ur - +X, Ur - +Y)
#' V(b)$leftside <- c(0, 0, 0)
#' V(b)$latent <- c(0, 0, 1)
#' V(b)$nvals <- c(2, 2, 2)
#' V(b)$exposure <- c(1, 0, 0)
#' V(b)$outcome <- c(0, 1, 0)
#' E(b)$rlconnect <- c(0, 0, 0)
#' E(b)$edge.monotone <- c(0, 0, 0)
#' effectt <- "p{Y(X=1)=1}-p{Y(X=0)=1}"
#' causalproblemcheck(digraph = b, query = effectt)
#'
causalproblemcheck <- function(digraph, query) {
  if (graphrescheck(graphres = digraph)) {
    if (querycheck(effecttext = query, graphres = digraph)) {
      return(TRUE)
    }
  }
  FALSE
}
