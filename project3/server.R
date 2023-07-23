library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

shinyServer(function(input, output, session) {
  setwd("C:/Documents/Github/project3")
  student <- read.csv('data.csv')
  getData <- reactive({
    v <- input$grade
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
    newData <- student %>% filter(Grade == v)
    }
    else if (v == "All grades") {
    newData <- student
    }
  })

  output$graph <- renderPlot({
    v <- input$type
    newData <- getData()
    var <- input$var
    if ((v == 1) & !input$gender) {
      newData <- getData()
      g <- ggplot(newData, aes_string(x = var))
      m <- g + geom_boxplot(color = '#FF1493', fill = '#FF1493',
                            alpha=0.2) + coord_flip()
      print(m)
    }
    else if ((v == 1) & input$gender) {
      newData <- getData()
      g <- ggplot(newData, aes_string(x = var))
      m <- g + geom_boxplot(aes(color = Gender, fill = Gender),
                            alpha = 0.2) + coord_flip()
      print(m)
    }
    else if ((v == 2) & !input$gender) {
      newData <- getData()
      g <- ggplot(newData, aes_string(x = var))
      m <- g + geom_histogram(color = '#FF1493', fill = '#FF1493',
                              alpha=0.2, bins = 15)
      print(m)
    }
    else if ((v == 2) & input$gender) {
      newData <- getData()
      g <- ggplot(newData, aes_string(x = var))
      m <- g + geom_histogram(aes(color = Gender, fill = Gender),
                              alpha = 0.2, bins = 15)
      print(m)
    }
  })

  #Update title
  output$title <- renderUI({
    v <- input$grade
    var <- input$var
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
      paste0('Investigation of ', v, 'th graders quantitative variables')
    }
    else if (v == "All grades"){
    paste0('Investigation of all grades quantitative variables')
    }
  })

  output$table <- renderDataTable({
    var <- input$var
    stat <- input$stat
    newData <- getData()
    newDataSub <- newData[, c("Grade", "Gender", "Age", var),
                          drop = FALSE]
    tab <- aggregate(newDataSub[[var]] ~ Grade + Gender + Age,
                     data = newDataSub,
                     FUN = stat)
    colnames(tab) <- c("Grade", 'Gender', "Age", "Stat")
    names(tab)[names(tab) == 'Stat'] <- paste0(stat , var)
    tab <- tab %>%                   # Using dplyr functions
      mutate_if(is.numeric,
                round,
                digits = 2)
  })
})