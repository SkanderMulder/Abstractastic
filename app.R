

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(bootstrapPage(theme = "bootstrap2.css",
   
   # Application title
   titlePanel("Abstratastic"),
  ('Created by Dalibor and Skander'),
   # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #    sidebarPanel(
        textInput("caption", "Search", "Author"),
        verbatimTextOutput("value")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput('mytable')
   #    )
   # )
))

# Define server logic required to draw a histogram
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

