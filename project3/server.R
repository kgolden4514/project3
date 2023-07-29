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
library(GGally)
library(corrplot)
library(htmltools)
library(shinyWidgets)
library(Metrics)

setwd("C:/Documents/Github/project3")
house <- read.csv('house.csv')
house$zipcode <- as.character(house$zipcode)
house$renovatedFac <- as.factor(house$renovatedYN)
house$basementFac<- as.factor(house$basementYN)
house$waterfrontFac <- as.factor(house$waterfrontYN)
house$yrBuiltFac <- as.factor(house$yrBuilt)
house$decadeBuiltFac <- as.factor(house$decadeBuilt)
house$zipcodeFac <- as.factor(house$zipcode)
house$yrBuiltCat <- as.character(house$yrBuilt)

shinyServer(function(input, output, session) {
  setwd("C:/Documents/Github/project3")
  house <- read.csv('house.csv')
  house$zipcode <- as.character(house$zipcode)
  house$renovatedFac <- as.factor(house$renovatedYN)
  house$basementFac<- as.factor(house$basementYN)
  house$waterfrontFac <- as.factor(house$waterfrontYN)
  house$yrBuiltFac <- as.factor(house$yrBuilt)
  house$decadeBuiltFac <- as.factor(house$decadeBuilt)
  house$zipcodeFac <- as.factor(house$zipcode)
  house$yrBuiltCat <- as.character(house$yrBuilt)

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

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <-reactive({
  sample(1:nrow(getData()),
           splitSlider() * nrow(getData()))
  })# row indices for training data

trainingData <- reactive({
  tmptraindt <- getData()
  tmptraindt[trainingRowIndex(), ]
})

testData <- reactive({
  tmptestdt <- getData()
  tmptestdt[-trainingRowIndex(),]
})

#Create quantitative outputs
#______________________________________________________________________________________________________
output$vars <- renderUI ({
  if (input$quantCat == 'Quantitative') {
    selectInput('var', 'Choose Quantitative Variable',
                choices = list('Price' = 'price',
                               '# of Bedrooms' = 'bedrooms',
                               '# of Bathrooms' = 'bathrooms',
                               'Living Space (sqft)' = 'sqftLiving',
                               'Lot Space (sqft)' = 'sqftLot',
                               '# of Floors' = 'floors'))
  }
  else if (input$quantCat == 'Categorical') {
    selectInput('var', 'Choose Variables',
                choices = list('Waterfront'= 'waterfrontYN',
                               'Basement' = 'basementYN',
                               'Renovated' = 'renovatedYN'))
  }
})

output$type <- renderUI ({
  if (input$quantCat == 'Quantitative') {
    selectInput('type', 'Choose graph type',
                choices = list('boxplot' = 1,
                               'histogram' = 2))
  }
})

output$graph <- renderPlot ({
  type <- input$type
  var <- input$var
  dec <- input$dec
  newData <- getDatadec()
  g <- ggplot(newData, aes_string(x = var))
  if ((type == 1) &  !input$year) {
    newData <- getDatadec()
    m <- g + geom_boxplot(color = '#FF1493', fill = '#FF1493', alpha = 0.2) +
      coord_flip()
    print(m)
  }
  else if ((type == 1) & input$year & dec != "All Decades") {
    newData <- getDatadec()
    m <- g + geom_boxplot(aes(color = yrBuiltCat, fill = yrBuiltCat), alpha = 0.2) + 
      coord_flip()
    print(m)
  }
  else if ((type == 1) & input$year & dec == "All Decades") {
    newData <- getDatadec()
    m <- g + geom_boxplot(aes(color = decadeBuilt, fill = decadeBuilt), 
                          alpha = 0.2) + coord_flip()
    print(m)
  }
  else if ((type == 2) & !input$year) {
    newData <- getDatadec()
    m <- g + geom_histogram(color = '#FF1493', fill = '#FF1493', alpha=0.2)
    print(m)
  }
  else if ((type ==2) & input$year & (dec == "All Decades")) {
    newData <- getDatadec()
    m <- g + geom_histogram(aes(color = decadeBuilt, fill = decadeBuilt), alpha=0.2)
    print(m)
  }
  else if ((type ==2) & input$year & (dec != "All Decades")) {
    newData <- getDatadec()
    m <- g + geom_histogram(aes(color = yrBuiltCat, fill = yrBuiltCat), alpha = 0.2)
    print(m)
  }
  if (!input$year & (input$quantCat == "Categorical")) {
    newData <- getDatadec()
    m <- g +
      geom_bar(color = '#862efd', fill = '#862efd', alpha = 0.2)
    print(m)
  }
  else if (input$year & (input$quantCat == 'Categorical') & (input$dec == "All Decades")){
    newData <- getDatadec()
    m <- g + geom_bar(aes(color = decadeBuilt, fill = decadeBuilt), alpha = 0.2)
    print(m)
  }
  else if (input$year & (input$quantCat == 'Categorical') & (input$dec != "All Decades")){
    newData <- getDatadec()
    m <- g + geom_bar(aes(color = yrBuiltCat, fill = yrBuiltCat), alpha = 0.2)
    print(m)
  }
})

output$statchoice <- renderUI ({
  if (input$quantCat == 'Quantitative')
    selectInput('stat', "Choose stat",
                choices = list('Mean' = 'mean',
                               'Median' = 'median',
                               'Standard Deviation' = 'sd'))
})

output$quantTable <- renderDataTable ({
  if (input$quantCat == 'Quantitative') {
  var <- input$var
  stat <- input$stat
  newData <- getDatadec()
  newDataSub <- newData[ , c('decadeBuilt', 'yrBuiltCat', var), drop = FALSE]
  tab <- aggregate(newDataSub[[var]] ~ decadeBuilt + yrBuiltCat,
                   data = newDataSub, FUN = stat)
  colnames(tab) <- c("Decade Built", 'Year Built', "Stat")
  names(tab)[names(tab) == 'Stat'] <- paste0(stat  ,  var)
  tab <- tab %>%                   # Using dplyr functions
      mutate_if(is.numeric,
                round,
                digits = 2)
  }
})

output$quantTabley <- renderUI ({
  if (input$quantCat == 'Quantitative') {
    dataTableOutput('quantTable')
  }
})

output$title <- renderUI({
  v <- input$dec
  var <- input$var
  if (v != "All Decades") {
    paste0('Investigation of homes in the ', v)
  }
  else if (v == "All Decades"){
    paste0('Investigation of homes 1900-2000')
  }
})

output$kabley <- renderUI ({
  if (input$quantCat == 'Categorical') {
    verbatimTextOutput('kable')
  }
})

output$kable <- renderPrint ({
  var <- input$var
  dec <- input$dec
  newData <- getDatadec()
  newDataSub <- newData[, c("decadeBuilt", "yrBuiltCat", var), drop = FALSE]
  colnames(newDataSub) <- c('decadeBuilt', 'yearBuilt', 'Three')
  if (!input$year & (input$quantCat == 'Categorical')) {
    tab <- addmargins(table(newDataSub$decadeBuilt, newDataSub$Three))
    print(kable(tab, style = "simple",
                caption = paste0('Frequency of ', var , 
                                 ' for the', dec)))
  }
  else if (input$year & (input$quantCat == 'Categorical')) {
    tab <- addmargins(table(newDataSub$yearBuilt, newDataSub$Three))
    print(kable(tab, style = "simple", 
                caption = paste0('Frequency of ', var , 
                                 ' for the ', dec, ' by year')))
  }
})

#Create EDA
#______________________________________________________________________________________________________
output$Summ <- renderPrint ({
  if (input$begin) {  
  stargazer(
      getData(),
      type = "text",
      title = "Descriptive statistics",
      digits = 1,
      out = "table1.txt")
  }
})

output$structure <- renderPrint ({
  if (input$begin){
  str(getData())
}
})

output$ysum <- renderPrint ({
  if (input$begin) {
  var <- input$resp
  newData <- getData()
  newDataSub <- newData[ , c(var), drop = FALSE]
  summary(newDataSub)
  }
})

output$ggp <- renderPlot ({
  if (input$begin) {
  newData <- getData()
  newData <- newData[ , c(1:6, 9)]
  ggpairs(newData)
  }
})

output$corrp <- renderPlot ({
  if (input$begin) {
  newData <- getData()
  newData <- newData[ , c(1:6, 9)]
  correlation <- cor(newData, method = "spearman")
  corrplot(correlation, type = "upper", tl.pos = "lt", 
           tl.col = "black", tl.cex = 0.75,
           tl.srt = 45,mar = c(2, 0, 1, 0))
  corrplot(correlation, type = "lower", method = "number", 
           add = TRUE,
           diag = FALSE, tl.pos = "n", number.cex = 0.75)
  }
})

#Create Model Fitting
#______________________________________________________________________________________________________
splitSlider <- reactive({
  input$Slider1 / 100
})

output$cntTrain <-
  renderText(paste("Train Data:", nrow(trainingData()), "records"))
output$cntTest <-
  renderText(paste("Test Data:", nrow(testData()), "records"))

output$x <- renderUI ({
  if (input$response == 'yrBuilt') {
    selectInput('predictors', 'Choose predictor(s)',
                choice = list('Waterfront' = 'waterfrontFac',
                              'Basement' = 'basementFac',
                              'Decade Built' = 'decadeBuiltFac',
                              'Renovated' = 'renovatedFac',
                              'Zipcode' = 'zipcodeFac',
                              'Price' = 'price',
                              '# of Bedrooms' = 'bedrooms',
                              '# of Bathrooms' = 'bathrooms',
                              'Living Space (sqrt)' = 'sqftLiving',
                              'Lot Space (sqrt)' = 'sqftLot'), multiple = TRUE)
  }

  else if (input$response == 'price') {
    selectInput('predictors', 'Choose predictor(s)',
                choice = list('Waterfront' = 'waterfrontFac',
                              'Basement' = 'basementFac',
                              'Decade Built' = 'decadeBuiltFac',
                              'Renovated' = 'renovatedFac',
                              'Zipcode' = 'zipcodeFac',
                              '# of Bedrooms' = 'bedrooms',
                              '# of Bathrooms' = 'bathrooms',
                              'Living Space (sqrt)' = 'sqftLiving',
                              'Year Built' = 'yrBuilt',
                              'Lot Space (sqrt)' = 'sqftLot'), multiple = TRUE)
  }
  else if (input$response == 'bedrooms') {
    selectInput('predictors', 'Choose predictor(s)',
                choice = list('Waterfront' = 'waterfrontFac',
                              'Basement' = 'basementFac',
                              'Decade Built' = 'decadeBuiltFac',
                              'Renovated' = 'renovatedFac',
                              'Zipcode' = 'zipcodeFac',
                              'Price' = 'price',
                              '# of Bathrooms' = 'bathrooms',
                              'Living Space (sqrt)' = 'sqftLiving',
                              'Year Built' = 'yrBuilt',
                              'Lot Space (sqrt)' = 'sqftLot'), multiple = TRUE)
    
  }
  else if (input$response == 'bathrooms') {
    selectInput('predictors', 'Choose predictor(s)',
                choice = list('Waterfront' = 'waterfrontFac',
                              'Basement' = 'basementFac',
                              'Decade Built' = 'decadeBuiltFac',
                              'Renovated' = 'renovatedFac',
                              'Zipcode' = 'zipcodeFac',
                              'Price' = 'price',
                              '# of Bedrooms' = 'bedrooms',
                              'Living Space (sqrt)' = 'sqftLiving',
                              'Year Built' = 'yrBuilt',
                              'Lot Space (sqrt)' = 'sqftLot'), multiple = TRUE)
    
  }
  else if (input$response == 'sqftLiving') {
    selectInput('predictors', 'Choose predictor(s)',
                choice = list('Waterfront' = 'waterfrontFac',
                              'Basement' = 'basementFac',
                              'Decade Built' = 'decadeBuiltFac',
                              'Renovated' = 'renovatedFac',
                              'Zipcode' = 'zipcodeFac',
                              'Price' = 'price',
                              '# of Bedrooms' = 'bedrooms',
                              '# of Bathrooms' = 'bathrooms',
                              'Year Built' = 'yrBuilt',
                              'Lot Space (sqrt)' = 'sqftLot'), multiple = TRUE)
  }
  else if (input$response == 'sqftLot') {
    selectInput('predictors', 'Choose predictor(s)',
                choice = list('Waterfront' = 'waterfrontFac',
                              'Basement' = 'basementFac',
                              'Decade Built' = 'decadeBuiltFac',
                              'Renovated' = 'renovatedFac',
                              'Zipcode' = 'zipcodeFac',
                              'Price' = 'price',
                              '# of Bedrooms' = 'bedrooms',
                              '# of Bathrooms' = 'bathrooms',
                              'Living Space (sqrt)' = 'sqftLiving',
                              'Year Built' = 'yrBuilt'), multiple = TRUE)
  }
})

linFit <- reactive ({
  trains <- trainingData()
  y <- input$response
  x <- input$predictors
  f <- as.formula(
    paste(y, 
          paste(x, collapse = " + "), 
          sep = " ~ "))
  
  train(f, data = trains,
        method = "lm",
        preProcess = c("center", "scale"),
        trControl = trainControl(method = "cv", number = 5))
})

output$linsum <- renderPrint ({
  if (input$action) {
    summary(linFit())
  }
})

output$linplot <- renderPlot ({
  if (input$action) {
    plot(linFit())
  }
})

boostFit <- reactive ({
  train <- trainingData()
  y <- input$response
  x <- input$predictors
  f <- as.formula(paste(y, paste(x, collapse = ' + '), sep = " ~ "))
  train(f, data = train,
          method = "gbm",
          preProcess = c("center", "scale"),
          tuneGrid = expand.grid(interaction.depth = c(1,4,7), 
                                 n.trees = c(1:20) , 
                                 shrinkage = 0.1,
                                 n.minobsinnode = c(10,20, 40)),
          trControl = trainControl(method = 'cv', number = 6), verbose=FALSE)
})

output$boostplot <- renderPlot ({
  if (input$action) {
  plot(boostFit())
  }
})

output$boostsum <- renderPrint ({
  if (input$action){
  print(boostFit())
  }
})

rfFit <- reactive ({
  train <- trainingData()
  y <- input$response
  x <- input$predictors
  f <- as.formula(paste(y, paste(x, collapse = ' + '), sep = " ~ "))
  train(f, data = train,
        method = "rf",
        trControl = trainControl(method = "cv",
                                 number = 5),
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(mtry = 1:sqrt(ncol(train))))
})

output$rfplot <- renderPlot ({
  if (input$action) {
  plot(rfFit())
  }
})

output$rfsum <- renderPrint ({
  if (input$action) {
  print(rfFit())
  }
})

output$linRMSE <- renderPrint ({
  if (input$action) {
    lin <- linFit()
    x <- lin$results
    y <- rf$bestTune[[1]]
    c <- x[c(1), c(2, 3, 4)]
    print(c)
  }
})

output$rfRMSE <- renderPrint ({
  if (input$action) {
    rf <- rfFit()
    x <- rf$results
    y <- rf$bestTune[[1]]
    c <- x[y, c(2, 3, 4)]
    print(c)
  }
})

output$boostRMSE <- renderPrint ({
  if (input$action) {
    boost <- boostFit()
    x <- boost$results
    tree <- boost$bestTune[[1]]
    inter <- boost$bestTune[[2]]
    shrink <- boost$bestTune[[3]]
    mino <- boost$bestTune[[4]]
    d <- c %>% filter(shrinkage == shrink)
    d <- d %>% filter(interaction.depth == inter)
    d <- d %>% filter(n.minobsinnode == mino)
    d <- d %>% filter(n.trees == tree)
    c <- d[c(1), c(5, 6, 7)]
    print(c)
  }
})

linPred <- reactive ({
  response <- input$response
  linearPred <- predict(linFit(), newdata = testData())
})

output$linPredsum <- renderPrint ({
  if (input$action) {
    summary(linFit())
  }
})

linRMSE2 <- reactive ({
  response <- input$response
  test <- testData()
  testSub <- test[ , c(response), drop = TRUE]
  linRMSE2<- postResample(linPred(), obs = testSub)
})

rfPred <- reactive ({
  response <- input$response
  rfPred <- predict(rfFit(), newdata = dplyr::select(testData(), -input$response))
})

boostPred <- reactive ({
  response <- input$response
  boostPred <- predict(boostFit(), newdata = dplyr::select(testData(), -response))
})

output$linRMSE2P <- renderPrint ({
  if (input$action) {
    print(linRMSE2())
  }
})

# rfRMSE2 <- renderPrint ({
#   if (input$action) {
#     response <- input$response
#     test <- testData()
#     rfRMSE2 <- postResample(rfPred(), obs = test$response)
#     print(rfRMSE2)
#   }
# })

# boostRMSE2 <- renderPrint ({
#   if (input$action) {
#     response <- input$response
#     test <- testData()
#     boostRMSE2 <- postResample(boostPred, obs = test$response)
#     print(boostRMSE2)
#   }
# })


#End function
#_______________________________________________________________________________________________________
})

  