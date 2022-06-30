library(shiny)
library(markdown)
library(igraph)

server <- function(input, output) {
    
    output$outcode <- renderPrint({
        
        myin <- input$edges
        if(length(myin) > 0) {
            
            print(igraphFromList())
        } else {
            
            print("Shift+click to add nodes, shift+drag to add edges")
            
        }
        
    })
    
      
    edgeList <- reactive({
        
        
        myin <- input$edges
        if(length(myin) > 0) {
            j <- seq(1, length(myin) - 2, by = 3)
            
            data.frame(id = paste0("e", myin[j]), source = myin[j+1], target = myin[j+2])
            
            
        } else {
            
            NULL
            
        }
        
    })
    
    
    igraphFromList <- reactive({
        
        myin <- input$edges
        if(length(myin) > 0) {
            
            graph_from_data_frame(edgeList()[, -1])
            
        } else {
            
            NULL
            
        }
        
    })
    
    inList <- reactiveValues(hazards = NULL)
    
    observeEvent(edgeList(), {
        
        mel <- edgeList()
        
        isolate(curhaz <- inList$hazards)
        
        print(curhaz)
        
        newhaz <- setdiff(mel$id, curhaz)
        
        if(length(newhaz) > 0) {
        insertUI(selector = "#outcode", 
                 where = "beforeBegin", 
                 ui = tags$div(id = newhaz, inputPanel(
                                          tags$p(paste("ID:" , newhaz))))
                 )
        
        inList$hazards <- c(curhaz, newhaz)
        
        }
        
        removedhaz <- setdiff(curhaz, mel$id)
   
        print(removedhaz)
        if(length(removedhaz) > 0) {
            for(j in 1:length(removedhaz)) {
            removeUI(selector = paste0("#", gsub(".", "\\.", removedhaz[j], fixed = TRUE)))
            }
        }
        
    })
    
    
}

testUI <- shinyUI(fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css/graph-creator.css")
    ),
    headerPanel("Multistate Network Simulation"), 
    sidebarPanel(verbatimTextOutput("outcode")), 
    mainPanel(tagList(tags$div(id="graph", style="height:640px"), 
                      tags$div(id="toolbox", 
                               tags$input(type="file", id="hidden-file-upload"), 
                               tags$input(id="upload-input", type="image", title="upload graph", src="upload-icon.png", alt="upload graph"), 
                               tags$input(type="image", id="download-input", title="download graph", src="download-icon.png", alt="download graph"), 
                               tags$input(type="image", id="delete-graph", title="delete graph", src="trash-icon.png", alt="delete graph")), 
                      tags$br())
              ), 
    tags$script(src = "js/d3.v3.min.js"),
    tags$script(src = "js/graph-creator.js"), 
    tags$script(src = "js/FileSaver.min.js") 
))


shinyApp(ui = testUI, server)
