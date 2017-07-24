#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)
library(DT)
library(scales)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  dt.portfolio <- data.table(Contract = character(), Price = numeric(), Strike = numeric(), Quantity = integer())
  
  output$option.type.select <- renderUI({
    return( radioButtons('option.type', NA, c("Call", "Put"), inline = TRUE))  
  })
  

  output$option.strike.input <- renderUI({
    return( numericInput('option.strike', "Strike", 50L, min = 0, max = NA, step = NA,
                         width = NULL) )  
  })
  
  output$option.quantity.input <- renderUI({
    return( numericInput('option.quantity', "Quantity", 10L, min = NA, max = NA, step = 10,
                         width = NULL) )  
  })
  
  output$option.premium.input <- renderUI({
    return( numericInput('option.premium', "Price/Premium", 0L, min = 0, max = NA, step = 0.5,
                         width = NULL) )  
  })
  1
  output$option.text <- renderText({})
  
  observeEvent(input$option.submit, {
    if (is.numeric(input$option.premium) && is.integer(input$option.quantity) && is.numeric(input$option.strike)) {
      if (input$option.premium >= 0) {
        if (input$option.strike > 0) {
          if (input$option.quantity != 0) {
          output$option.text <- renderText({})
          dt.portfolio <<- rbind(dt.portfolio, data.table(Contract = input$option.type,
                                                       Price = input$option.premium,
                                                       Strike = input$option.strike,
                                                       Quantity = input$option.quantity))
          } else { 
            output$option.text <- renderText({ "Option quantity is zero, no changes made."})
          }} else { 
            output$option.text <- renderText({ "Option strike must be greater than zero."})
          }
        } else {
          output$option.text <- renderText({ "Option premium must be positive."})
        } 
      } else {
      output$option.text <- renderText({ "Inputs must be numeric. Quantities must be integers."})
    }
     })
  
  output$stock.quantity.input <- renderUI({
    return( numericInput('stock.quantity', "Quantity", 1000L, min = NA, max = NA, step = 1000,
                         width = NULL) )  
  })
  
  output$stock.price.input <- renderUI({
    return( numericInput('stock.price', "Price", 50L, min = 0, max = NA, step = 1,
                         width = NULL) )  
  })
  
  output$stock.text <- renderText({})
  
  observeEvent(input$stock.submit, {
    if (is.numeric(input$stock.price) && is.integer(input$stock.quantity)) {
      if (input$stock.price >= 0) {
        if (input$stock.quantity != 0) {
          output$stock.text <- renderText({})
          dt.portfolio <<- rbind(dt.portfolio, data.table(Contract = "Stock",
                                                       Price = input$stock.price,
                                                       Strike = NA,
                                                       Quantity = input$stock.quantity))
          } else {
          output$stock.text <- renderText({ "Stock quantity is zero, no changes made."})
          }
        } else {
        output$stock.text <- renderText({ "Stock price must be greater than or equal to zero."})
        }
      } else {
        output$stock.text <- renderText({ "Inputs must be numeric. Quantities must be integers."})
      }
  })
  
  observeEvent(input$delete.input, {
    dt.portfolio <<- dt.portfolio[-input$dt.portfolio_rows_selected]
   })

  observe({
    input$option.submit
    input$stock.submit
    input$delete.input
    

    output$dt.portfolio <- DT::renderDataTable(dt.portfolio, server=FALSE, selection = list(target = 'row'), rownames= FALSE)
  })
 
  observe({
    input$option.submit
    input$stock.submit
    input$delete.input

    dt.payoff <- data.table(price = numeric(), payoff = numeric())
    
    if (nrow(dt.portfolio) > 0) {
      net.stock = sum(dt.portfolio[Contract == 'Stock', Quantity])
      net.cost = sum(dt.portfolio$Price * dt.portfolio$Quantity * ifelse(dt.portfolio$Contract == 'Stock',1,100))
      
       if (nrow(dt.portfolio[Contract != 'Stock',]) > 0) {
   
      num.price <- sort(unique(dt.portfolio[Contract != 'Stock', Strike]))
      
      num.price <- c(max(min(num.price) - 10, 0), num.price, max(num.price) + 10) #10 is chosen arbitrarily to show what happens as price increases

      num.payoff <- sapply(num.price, function(price.breakpoint) {
        dt.subset <- dt.portfolio[(Contract == 'Call' & Strike < price.breakpoint) |
                                    (Contract == 'Put' & Strike > price.breakpoint),]
        dt.subset[,revenue := abs(Strike - price.breakpoint) * Quantity * 100]
        sum(dt.subset$revenue) + net.stock * price.breakpoint
      })
     
      num.payoff = num.payoff - net.cost
      
      #Have the graph cross the x axis if possible
      left.slope = (num.payoff[[1]] - num.payoff[[2]]) / (num.price[[1]] - num.price[[2]])
      if (left.slope < 0 && num.payoff[[1]] > 0) { 
           num.price <- c(num.price[[1]] - (num.payoff[[1]] + 100) / left.slope , num.price)
           num.payoff <- c(-100, num.payoff)
      }
      if (left.slope > 0 && num.payoff[[1]] < 0) { 
        num.price <- c(num.price[[1]] + (-num.payoff[[1]] + 100) / left.slope, num.price)
        num.payoff <- c(100, num.payoff)
      }
      right.slope = (num.payoff[[length(num.payoff)]] - num.payoff[[length(num.payoff) - 1]])  / (num.price[[length(num.price)]] - num.price[[length(num.price) - 1]])
      if (right.slope < 0 && num.payoff[[length(num.payoff)]] > 0) { 
        num.price <- c(num.price, num.price[[length(num.payoff)]] - (num.payoff[[length(num.payoff)]] + 100) / right.slope)
        num.payoff <- c(num.payoff, -100)
      }
      if (right.slope > 0 && num.payoff[[length(num.payoff)]] < 0) { 
        num.price <- c(num.price, num.price[[length(num.payoff)]] + (100 - num.payoff[[length(num.payoff)]]) / right.slope)
        num.payoff <- c(num.payoff, 100)
      }  
    
      
       } else {
           num.price <- c(0, 100)
           num.payoff <- c(0, 100*net.stock)
           num.payoff = num.payoff - net.cost
           
       }
      dt.payoff <- data.table(price = num.price, payoff = num.payoff)
  
    } 
    
    output$payoffPlot <<- renderPlot({
      
      if (nrow(dt.payoff) > 0) {
        spread = max(dt.payoff$payoff) - min(dt.payoff$payoff)
        ylims = c(min(dt.payoff$payoff) - spread * .1, max(dt.payoff$payoff) + spread * .1)
      } else {
        ylims = c(0,100)
      }

       p <- ggplot(data=dt.payoff, aes(x = price, y = payoff)) + geom_hline(yintercept=0, size = 2, color = "orange")+
        geom_line(size = 1.5) + 
        labs(x = "Underlier Price at Expiration ($)",
             y = "Net P/L ($)")+ theme(text = element_text(size=18)) + scale_y_continuous(limits = ylims, label=comma)
      print(p)
    })
  })

  #dt.portfolio <- data.table(Contract = c('Call', 'Call', 'Put', 'Stock', 'Stock'), Price = c(5, 2, 19, 20, 30), Strike = c(20, 30, 25, NA, NA), Quantity = c(3, -5, 2, 100, -50))
  

  session$onSessionEnded(function() {
    stopApp()
  })
  
})
