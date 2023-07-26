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
student <- read.csv('data2.csv')

shinyServer(function(input, output, session) {
  setwd("C:/Documents/Github/project3")
  student <- read.csv('data2.csv')
  student <- student[ , -c(24,25)]
  student$Year <- as.character(student$Year)
  dd <- student

#Code for About----------------------------------------------------------------
  
    
#Code for Quantitative---------------------------------------------------------
  #Update quantitative title
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
  
  #Create quantitative dataframe
  getData <- reactive({
    v <- input$grade
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
    newData <- student %>% filter(Grade == v)
    }
    else if (v == "All grades") {
    newData <- student
    }
  })
  
  #Create boxplots and histograms
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
  
  #Create data table
  output$table <- renderDataTable({
    var <- input$var
    stat <- input$stat
    newData <- getData()
    newDataSub <- newData[, c("Grade", "Gender", var),
                          drop = FALSE]
    tab <- aggregate(newDataSub[[var]] ~ Grade + Gender,
                     data = newDataSub,
                     FUN = stat)
    colnames(tab) <- c("Grade", 'Gender', "Stat")
    names(tab)[names(tab) == 'Stat'] <- paste0(stat  ,  var)
    tab <- tab %>%                   # Using dplyr functions
      mutate_if(is.numeric,
                round,
                digits = 2)
  })
  
#Code for Categorical---------------------------------------------------------
  
  #Update categorical title
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
  
  #Create categorical dataframe
  getData2 <- reactive({
    v <- input$grade2
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
      newData2 <- student %>% filter(Grade == v)
    }
    else if (v == "All grades") {
      newData2 <- student %>% filter(Grade == 9 | 10 | 11 | 12)
    }
  })
  
  #Create categorical female dataframe
  getDatafem <- reactive({
    v <- input$grade2
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
      newDatafem <- student %>% filter(Grade == v & Gender == 'Female')
    }
    else if (v == "All grades") {
      newDatafem <- student %>% filter(Gender == 'Female')
    }
  })
  
  #Create categorical male dataframe
  getDatamale <- reactive({
    v <- input$grade2
    if ((v == 9 | 10 | 11 | 12) & v != "All grades") {
      newDatamale <- student %>% filter(Grade == v & Gender == 'Male')
    }
    else if (v == "All grades") {
      newDatamale <- student %>% filter(Gender == 'Male')
    }
  })
  
  #Create plots
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

  #Create frequency tables
  output$kable <- renderPrint ({
    var <- input$vars
    grade <- input$grade2
    newData <- getData2()
    newDataSub <- newData[, c("Grade", "Gender", var), drop = FALSE]
    colnames(newDataSub) <- c('Grade', 'Gender', 'Three')
    if (!input$gender2) {
      tab <- addmargins(table(newDataSub$Grade, newDataSub$Three))
      print(kable(tab, style = "simple",
                  caption = paste0('Frequency of ', var , 
                                   ' for ', grade , 'th Graders')))
    }
    else if (input$gender2) {
      tab <- addmargins(table(newDataSub$Gender, newDataSub$Three))
      print(kable(tab, style = "simple", 
                  caption = paste0('Frequency of ', var , 
                                   ' by Gender for ', grade, 
                                   'th Graders')))
    }
  })
  
#Code for Modeling Info---------------------------------------------------------
  

#Code for Model Fitting---------------------------------------------------------
    InputDataset <- reactive({
      student
    })
    
    inputDataSetNum <- reactive({
      student[ , -c(1, 3:4, 10, 14:17, 21:23)]
    })
    
    InputDataset_model <- reactive({
      if (is.null(input$SelectX)) {
        dt <- student
      }
      else{
        dt <- student[, c(input$SelectX)]
      }
    })
    
    observe({
      lstname <- names(InputDataset())
      updateSelectInput(session = session,
                        inputId = "SelectY",
                        choices = lstname)
    })
    
    splitSlider <- reactive({
      input$Slider1 / 100
    })
    
    set.seed(100)  # setting seed to reproduce results of random sampling
    trainingRowIndex <-
      reactive({
        sample(1:nrow(InputDataset_model()),
               splitSlider() * nrow(InputDataset_model()))
      })# row indices for training data
    
    trainingData <- reactive({
      tmptraindt <- InputDataset_model()
      tmptraindt[trainingRowIndex(), ]
    })
    
    testData <- reactive({
      tmptestdt <- InputDataset_model()
      tmptestdt[-trainingRowIndex(),]
    })
    
    output$cntTrain <-
      renderText(paste("Train Data:", nrow(trainingData()), "records"))
    output$cntTest <-
      renderText(paste("Test Data:", nrow(testData()), "records"))
    
    output$Data <- renderDataTable(InputDataset())
    
    output$Summ <-
      renderPrint(
        stargazer(
          InputDataset(),
          type = "text",
          title = "Descriptive statistics",
          digits = 1,
          out = "table1.txt"
        )
      )
    
    output$Summ_old <- renderPrint(summary(InputDataset()))
    output$structure <- renderPrint(str(InputDataset()))
    
    cormat <- reactive({
     a <- round(cor(inputDataSetNum()), 1)
    })
    
    output$Corr <-
      renderPlot(corrplot(
        cormat(),
        type = "lower",
        order = "hclust",
        method = "number"))

#Code for inear regression---------------------------------------------------------
    
    # recipe_formula <- reactive(trainingData %>%
    #   recipe() %>%
    #     update_role(input$SelectY,new_role = "outcome") %>%
    #       update_role(!!!input$SelectX,new_role = "predictor") %>% 
    #         formula())
    # 
    # lm_reg <- reactive(
    #   lm(recipe_formula(), data = trainingData)
    # )
    # 
    # output$RegOut = renderPrint({summary(lm_reg())})
#Prediction
  
})