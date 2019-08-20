# Function for creation of Shiny application for next word prediction with 
# use of Kneser-Ney statistical method and LSTM Neural net.
# In this function we previously calculated tables for n-grams probability 
# and previously trained LSTM neural net. 

# Loading libraries and function
library(shiny)
library(dplyr)

#source("C:/Users/slonoc/Documents/word_prediction/next.word_LSTM.letters.R")
source("C:/Users/slonoc/Documents/word_prediction/next.word_stat.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Next word prediction"),
    br(),
    textInput("text", label = "Print your text here", value = "Enter text....."),
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
    
    output$nn <- renderText("NN")
    output$Kneser_Ney <- renderText("KN")

}

# Run the application 
shinyApp(ui = ui, server = server)
