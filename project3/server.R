library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(cowplot)

shinyServer(function(input, output, session) {
  setwd("C:/Documents/Github/project3")
  student <- read.csv('data2.csv')
  getData <- reactive({
    v <- input$grade
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
    newData <- student %>% filter(Grade == v)
    }
    else if (v == "All grades") {
    newData <- student
    }
  })
  
  getData2 <- reactive({
    v <- input$grade2
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
      newData2 <- student %>% filter(Grade == v)
    }
    else if (v == "All grades") {
      newData2 <- student
    }
  })
  
  getDatafem <- reactive({
    v <- input$grade2
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
      newDatafem <- student %>% filter(Grade == v & Gender == 'Female')
    }
    else if (v == "All grades") {
      newDatafem <- student %>% filter(Gender == 'Female')
    }
  })
  
  getDatamale <- reactive({
    v <- input$grade2
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
      newDatamale <- student %>% filter(Grade == v & Gender == 'Male')
    }
    else if (v == "All grades") {
      newDatamale <- student %>% filter(Gender == 'Male')
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
                              alpha=0.2, bins = 40)
      print(m)
    }
    else if ((v == 2) & input$gender) {
      newData <- getData()
      g <- ggplot(newData, aes_string(x = var))
      m <- g + geom_histogram(aes(color = Gender, fill = Gender),
                              alpha = 0.2, bins = 40)
      print(m)
    }
  })
  
  output$cat_graph <- renderPlot({
    vars <- input$vars
    if (!input$gender2){
      newData2 <- getData2()
      g <- ggplot(newData2, aes_string(x = vars))
      m <- g + 
        geom_bar(color = '#862efd', fill = '#862efd', alpha = 0.2) +
        theme(axis.text.x = 
                element_text(angle = -75))
      print(m)
    }
    else if (input$gender2){
      newDatafem <- getDatafem()
      newDatamale <- getDatamale()
      f <- ggplot(newDatafem, aes_string(x = vars)) + 
        geom_bar(color = '#F987C5', fill = '#F987C5', alpha = 0.2) + 
        theme(axis.text.x = 
                element_text(angle = -75))
      m <- ggplot(newDatamale, aes_string(x = vars)) + 
        geom_bar(color = '#2f4795', fill = '#2f4795', alpha = 0.2) + 
        theme(axis.text.x = 
                element_text(angle = -75))
      print(plot_grid(f, m, labels = c('Female', 'Male')))
    }
  })

  #Update title
  output$quant_title <- renderUI({
    v <- input$grade
    var <- input$var
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
      paste0('Investigation of ', v, 'th graders quantitative variables')
    }
    else if (v == "All grades"){
    paste0('Investigation of all grades quantitative variables')
    }
  })
  
  #Update title
  output$cat_title <- renderUI({
    v <- input$grade2
    var <- input$var
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
      paste0('Investigation of ', v, 'th graders categorical variables')
    }
    else if (v == "All grades"){
      paste0('Investigation of all grades categorical variables')
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