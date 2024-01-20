#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(stringr)
library(tm)
library(NLP)
library(here)

ngramdata_path = "RData"

# Load N-Gram data along with frequencies
# bigram <- readRDS(file = file.path(ngramdata_path, "bigram.RData"))
# trigram <- readRDS(file = file.path(ngramdata_path, "trigram.RData"))
# fourgram <- readRDS(file = file.path(ngramdata_path, "fourgram.RData"))
# fivegram <- readRDS(file = file.path(ngramdata_path, "fivegram.RData"))

bigram <- readRDS(file = here(ngramdata_path, "bigram.RData"))
trigram <- readRDS(file = here(ngramdata_path, "trigram.RData"))
fourgram <- readRDS(file = here(ngramdata_path, "fourgram.RData"))
fivegram <- readRDS(file = here(ngramdata_path, "fivegram.RData"))


# Function to clean data
dataCleaner <- function(iText) {
  cleanText <- tolower(iText)
  cleanText <- removePunctuation(cleanText, preserve_intra_word_dashes = TRUE)
  cleanText <- removeNumbers(cleanText)
  cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
  cleanText <- stripWhitespace(cleanText)
  
  return(cleanText)
}

# Function to predict the next word
predictNextWord <- function(inputText, ngram_count = 0){
  cleanInput <- dataCleaner(inputText)          # Clean the data
  splitInput <- strsplit(cleanInput, " ")[[1]]  # Split the input text into words
  nWords <- length(splitInput)                  # Get the word count
  pred1 <- "NULL"
  pred2 <- "NULL"
  pred3 <- "NULL"
  
  if (nWords >=5) {
    splitInput = tail(splitInput, 4) 
    nWords  = 4
  }
  if (nWords ==4) {
    df_possible <- fivegram[fivegram$first == splitInput[1] &
                              fivegram$second == splitInput[2] &
                              fivegram$third == splitInput[3] &
                              fivegram$fourth == splitInput[4], ]
    pred1 <- as.character(df_possible$fifth[1])
    pred2 <- as.character(df_possible$fifth[2])
    pred3 <- as.character(df_possible$fifth[3])
    if (is.na(pred1)) {
      splitInput = tail(splitInput, 3)
      nWords  = 3
    }
    
  } 
  
  if (nWords ==3) {
    df_possible <- fourgram[fourgram$first==splitInput[1] &
                              fourgram$second==splitInput[2] &
                              fourgram$third==splitInput[3],]
    pred1 <- as.character(df_possible$fourth[1])
    pred2 <- as.character(df_possible$fourth[2])
    pred3 <- as.character(df_possible$fourth[3])
    if (is.na(pred1)) {
      splitInput = tail(splitInput, 2)
      nWords  = 2
    }
  }
  if (nWords ==2) {
    df_possible <- trigram[trigram$first==splitInput[1] &
                             trigram$second==splitInput[2],]
    pred1 <- as.character(df_possible$third[1])
    pred2 <- as.character(df_possible$third[2])
    pred3 <- as.character(df_possible$third[3])
    if (is.na(pred1)){
      splitInput = tail(splitInput, 1)
      nWords  = 1
    }
  }
  if (nWords ==1) {
    df_possible <- bigram[bigram$first==splitInput[1],]
    pred1 <- as.character(df_possible$second[1])
    pred2 <- as.character(df_possible$second[2])
    pred3 <- as.character(df_possible$second[3])
    if (is.na(pred1)){
      pred1 <- "the"
      pred2 <- "it"
      pred3 <- "a"
    }
  }
  return(c(pred1, pred2, pred3))
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    predictionResult <- reactive({
      textInput <- input$textInput
      predictionResult <- predictNextWord(textInput)
    })
    
    output$predicted_word <- renderText(predictionResult()[1])

    output$prediction2 <- renderText(predictionResult()[2])
    
    output$prediction3 <- renderText(predictionResult()[3])
})

