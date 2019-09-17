library(shiny)
library(igraph)

selectorButton <- function(id, label = "Selector") {
    ns <- NS(id)
    tagList(
        actionButton(ns("button"), label = label)
    )
}

selector <- function(input, output, session, graph) {
    vars <- names(V(graph))

    count <- reactiveVal(0)

    observeEvent(input$button, {
        count(count() + 1)
    })

    observeEvent(input$button, {

        insertUI(selector="#out", "beforeBegin",
                 selectInput(paste0("var", count()), sprintf("Variable %s", count()), choices = vars)
                 )

        cat(input[[paste0("var", count())]])

    })


    observeEvent(input[[paste0("var", count())]], {
        cat("Hello")
        nestvars <- neighbors(graph, input[[paste0("var", i)]], mode = "in")
        paste(nestvars)
        insertUI(selector=paste0("#var", i), "afterEnd",
                 selectInput(paste0("var", i, "-nest"), "", choices = nestvars)
        )


    }, ignoreInit= TRUE)


    list(count = count)

}

ui <- fluidPage(
    actionButton("selector1", "Variable #1"),
    verbatimTextOutput("out")
)

server <- function(input, output, session) {

    graph <- make_graph(c("D", "X", "X", "Z", "X", "Y", "Z", "Y"), directed = TRUE)

    vars <- names(V(graph))

    count <- reactiveVal(0)

    observeEvent(input$selector1, {
        count(count() + 1)
    })

    observeEvent(input$selector1, {

        insertUI(selector="#out", "beforeBegin",
                 selectInput(paste0("var", count()), sprintf("Variable %s", count()), choices = vars, selectize = FALSE)
        )



    })


    observeEvent(input[[paste0("var", count())]], {
        i <- count()
        nestvars <- neighbors(graph, input[[paste0("var", i)]], mode = "in")

        removeUI(selector = paste0("#var", i, "-nest"))

        if(length(nestvars) > 0) {
        insertUI(selector=paste0("#var", i), "afterEnd",
                 selectInput(paste0("var", i, "-nest"), "", choices = names(nestvars), selectize = FALSE, multiple = TRUE)
        )
        }

    }, ignoreInit= TRUE)

    output$out <- renderText({
        myputs <- paste0("var", 1:count())
        if(count() > 0) {

            vars <- lapply(myputs, function(i) input[[i]])
            paste(vars)

        }

    })

}

shinyApp(ui, server)


