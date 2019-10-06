library(shiny)

# Define UI for application that __
ui <- fluidPage(bootstrapPage(theme = "bootstrap2.css",
                              # Application title
                              titlePanel("Abstractastic"),
                              'Created by Dalibor and Skander',
                              textInput("caption", "Search", "Author"),
                              verbatimTextOutput("value")),
                submitButton(text = "Apply Changes", icon = NULL, width = NULL),
                numericInput("maxHits", "#hits:", 10, min = 1, max = 100),
                mainPanel(dataTableOutput('mytable')))

# Define server logic required to 
server <- function(input, output) {
      library(easyPubMed)
      server <- function(input, output) {
            output$value <- renderText({ input$caption })
      }
      source('new_testing.r')
      NewData <- reactive({a= CreateDFR(input$caption)})
      output$mytable  <- renderDataTable({ 
            tooput=NewData()
            print(tooput)
      })
}

# Run the application 
shinyApp(ui = ui, server = server)