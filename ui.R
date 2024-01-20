#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that predicts a next word
shinyUI(
  navbarPage("Shiny Application",
             tabPanel("Word Predictor",
                      fluidPage(
                        titlePanel("Predict next word based on N-gram datasets"),
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Enter a set words to trigger the prediction of next word"),
                            hr(),
                            textInput("textInput", "Input text", value = ""),
                            br(),
                            submitButton("Submit")
                          ),
                          
                          # Show a main panel with predicted words
                          mainPanel(
                            h2("Next predicted word..."),
                            verbatimTextOutput("Predicting next word..."),
                            h3(strong(code(textOutput('predicted_word')))),
                            br(),
                            br(),
                            br(),
                            h4(tags$b('Second best prediction:')),
                            textOutput('prediction2'),
                            br(),
                            h4(tags$b('Third best prediction:')),
                            textOutput('prediction3'),
                            br(),
                            br(),
                            br(),
                            h4(tags$b('Remarks: ')),
                            helpText("The prediction takes upto 30 seconds to provide response due to 4-gram and 5-gram files being 20Mb in size."),
                            br(),
                            br()
                          )
                        )
                      )
             ),
             
             tabPanel("About the Data",
                      h2("SwiftKey Data"),
                      hr(),
                      h3("Description"),
                      helpText("For this shiny app, the source of the data is from coursera which in turn has been provided by the course partner SwiftKey.",
                               " As stated in the course material, this data was collected from publicly available material using a web crawler."),
                      h3("Format"),
                      helpText("Four language sets have been provided - English, German, Finnish and Russian.",
                               "Each of the languages has data from blogs, new articles and twitter feeds",
                               "These were sampled to build a corpus which inturn were used to generate N-Gram tokenizations.",
                               "The tokens are generated using thr ngram library implementaion of RWeka called ngra_asweka.",
                               "The prediction is in principle a lookup table based on frequency of occurance.",
                               "Some of the results show that source clean up can be improved, this is evident when ",
                               "ordinal indicators such as th, st or rd are coming as valid predictions."),
              
                      h3("Source"),
                      p("SwiftKey Coursera Dataset"),
                      a("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
             ),
             tabPanel("Link to the Github Repository",
                      a("https://github.com/hisscaredbrain/NextWordPredictor"),
                      hr(),
                      h4("Enjoy the Shiny App!"),
                      h4("Thank you for being a part of my Data Science Learning.")
             )
  )
)
