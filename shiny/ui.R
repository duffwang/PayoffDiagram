#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Payout Diagram v1.1"),
  h5('Author: jeff.wang@cutlergrouplp.com'),
  hr(),
 
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(6,
    h3("Add Option"),
    uiOutput("option.type.select"),
    uiOutput("option.strike.input"),
    uiOutput("option.quantity.input"),
    uiOutput("option.premium.input"),
    actionButton("option.submit", "Submit"),
    uiOutput("option.text")
    ,
      h3("Add Stock"),
      uiOutput("stock.quantity.input"),
      uiOutput("stock.price.input"),
    actionButton("stock.submit", "Submit"),
    uiOutput("stock.text")
    ),
    
    # Show a plot of the generated distribution
    column(6,
       h3("Payoff"),
       plotOutput("payoffPlot"),
       br(),
       h3('Portfolio'),
       DT::dataTableOutput('dt.portfolio'),
       actionButton("delete.input", "Delete Selected Rows")
    )
  )
))
