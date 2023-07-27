library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(cowplot)
library(knitr)
library(caret)
library(randomForest)
library(shinydashboard)
library(shinycssloaders)
library(maps)
library(leaflet)
library(plotly)
library(corrplot)
library(stargazer)
library(shinythemes)
library(recipes)

setwd("C:/Documents/Github/project3")
house <- read.csv('house.csv')
house$yrBuilt <- as.character(house$yrBuilt)

shinyServer(function(input, output, session) {
setwd('C:/Documents/Github/project3')
house <- read.csv('house.csv')
house$yrBuilt <- as.character(house$yrBuilt)

#Create datasets
#_______________________________________________________________________________________________________
getData <- reactive({
  house
})

getDatadec <- reactive({
  v <- input$dec
  if (v != 'All Decades'){
    newData <- house %>% filter(decadeBuilt == v)
  }
  else if (v == 'All Decades') {
    newData <- house
  }
})

getDatadec2 <- reactive({
  v <- input$dec2
  if (v != 'All Decades'){
    newData2 <- house %>% filter(decadeBuilt == v)
  }
  else if (v == 'All Decades') {
    newData2 <- house
  }
})

#Create quantitative outputs
#_______________________________________________________________________________________________________
output$graph <- renderPlot ({
  type <- input$type
  var <- input$quant
  dec <- input$dec
  newData <- getDatadec()
  g <- ggplot(newData, aes_string(x = var))
  if ((type == 1) &  !input$year) {
    m <- g + geom_boxplot(color = '#FF1493', fill = '#FF1493', alpha = 0.2) +
      coord_flip()
    print(m)
  }
  else if ((type == 1) & input$year & dec != "All Decades") {
    m <- g + geom_boxplot(aes(color = yrBuilt, fill = yrBuilt), alpha = 0.2) + 
      coord_flip()
    print(m)
  }
  else if ((type == 1) & input$year & dec == "All Decades") {
    m <- g + geom_boxplot(aes(color = decadeBuilt, fill = decadeBuilt), 
                          alpha = 0.2) + coord_flip()
    print(m)
  }
  else if ((type == 2) & !input$year) {
    m <- g + geom_histogram(color = '#FF1493', fill = '#FF1493',
                            alpha=0.2, bins = 50)
    print(m)
  }
  else if ((type ==2) & input$year & dec == "All Decades") {
    newData <- getDatadec()
    m <- g + geom_histogram(aes(color = decadeBuilt, fill = decadeBuilt),
                            alpha=0.2, bins = 50)
    print(m)
  }
  else if ((type ==2) & input$year & dec != "All Decades") {
    newData <- getDatadec()
    m <- g + geom_histogram(aes(color = yrBuilt, fill = yrBuilt), 
                            alpha = 0.2)
    print(m)
  }
})

output$quantTable <- renderDataTable({
  var <- input$quant
  stat <- input$stat
  newData <- getDatadec()
  newDataSub <- newData[, c("decadeBuilt", "yrBuilt", var),
                        drop = FALSE]
  tab <- aggregate(newDataSub[[var]] ~ decadeBuilt + yrBuilt,
                   data = newDataSub,
                   FUN = stat)
  colnames(tab) <- c("Decade Built", 'Year Built', "Stat")
  names(tab)[names(tab) == 'Stat'] <- paste0(stat  ,  var)
  tab <- tab %>%                   # Using dplyr functions
    mutate_if(is.numeric,
              round,
              digits = 2)
})

output$title <- renderUI({
  v <- input$dec
  var <- input$quant
  if (v != "All Decades") {
    paste0('Investigation of homes in the ', v)
  }
  else if (v == "All Decades"){
    paste0('Investigation of homes 1900-2000')
  }
})
#Create Categorical Outputs
#______________________________________________________________________________________________________
#
output$title2 <- renderUI({
  v <- input$dec2
  var <- input$cat
  if (v != "All Decades") {
    paste0('Investigation of homes in the ', v)
  }
  else if (v == "All Decades"){
    paste0('Investigation of homes 1900-2000')
  }
})
output$catGraph <- renderPlot ({
  var <- input$cat
  newData2 <- getDatadec2()
  dec2 <- input$dec2
  g <- ggplot(newData2, aes_string(x = var))
  if (!input$year2) {
    newData2 <- getDatadec2()
    m <- g +
      geom_bar(color = '#862efd', fill = '#862efd', alpha = 0.2)
    print(m)
  }
  else if (input$year2 & dec2 != "All Decades") {
    newData2 <- getDatadec2()
    m <- g + geom_bar(aes(color = yrBuilt, fill = yrBuilt), alpha = 0.2)
    print(m)
  }
  else if (input$year2 & dec2 == "All  Decades") {
    m <- g + geom_bar(aes(color = decadeBuilt, fill = decadeBuilt), alpha = 0.2)
    print(m)
  }
})

#Create frequency tables
output$kable <- renderPrint ({
  var <- input$cat
  dec <- input$dec2
  newData <- getDatadec2()
  newDataSub <- newData[, c("decadeBuilt", "yrBuilt", var), drop = FALSE]
  colnames(newDataSub) <- c('decadeBuilt', 'yearBuilt', 'Three')
  if (!input$year2) {
    tab <- addmargins(table(newDataSub$decadeBuilt, newDataSub$Three))
    print(kable(tab, style = "simple",
                caption = paste0('Frequency of ', var , 
                                 ' for the', dec)))
  }
  else if (input$year2) {
    tab <- addmargins(table(newDataSub$yearBuilt, newDataSub$Three))
    print(kable(tab, style = "simple", 
                caption = paste0('Frequency of ', var , 
                                 ' for the ', dec, ' by year')))
  }
})


#End function
#_______________________________________________________________________________________________________
})

  