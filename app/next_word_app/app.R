# Function for creation of Shiny application for next word prediction with 
# use of Kneser-Ney statistical method and LSTM Neural net.
# In this function we previously calculated tables for n-grams probability 
# and previously trained LSTM neural net. 

# Loading libraries and function
library(shiny)
library(dplyr)

source("C:/Users/Seezis  Office/Documents/word_prediction/next.word_LSTM.letters.R")
source("C:/Users/Seezis  Office/Documents/word_prediction/next.word_stat.R")

# Define UI for application that predicts the next word
ui <- fluidPage(
    # Application title
    titlePanel("Next word prediction"),
    br(),
    textInput("text", label = "Print your text here", placeholder = "Enter text here....."),
    actionButton("button", "Generate text"),
    br(),
    br(),
    fluidRow(
        column(4,
               h6("Next word using LSTM Neural Net"),
               verbatimTextOutput("nn")),
        column(4,
               h6("Next word using Kneser-Ney algorithm"),
               verbatimTextOutput("Kneser_Ney"))
    )
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    sentence <- reactive({
        input$text
    }) 
    
    nn_output <- reactive({ 
        if (nchar(sentence()) <= maxlen) {
        return(paste("Enter at least", maxlen, "symbols"))
        } else {
            return(NN_next.word(sentence()))} # function from next.word-LSTM.letters.R source
    })
    
    
    kn_output <- eventReactive(input$button, {
        print("something works")
    })
    
    output$nn <- renderText(nn_output())
    output$Kneser_Ney <- renderText(sentence())
    
    # next word using kneser-ney algorithm
    

}

# Run the application 
shinyApp(ui = ui, server = server)
