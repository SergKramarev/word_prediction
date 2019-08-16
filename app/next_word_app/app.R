#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

nn <- "NN"
Kneser_Ney <- "Knesr-Ney"

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Next word prediction"),
    br(),
    textInput("text", label = "Print your text here", value = "Enter text....."),
    br(),
    fluidRow(
        column(4,
               verbatimTextOutput("nn")),
        column(4,
               verbatimTextOutput("Kneser_Ney"))
    )
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$nn <- renderText("NN")
    output$Kneser_Ney <- renderText("KN")

}

# Run the application 
shinyApp(ui = ui, server = server)
