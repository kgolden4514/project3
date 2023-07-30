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
home <- read.csv('house.csv')

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
  home <- read.csv('house.csv')
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

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex2 <-reactive({
  sample(1:nrow(getData()),
         splitSlider2() * nrow(getData()))
})# row indices for training data

trainingData2 <- reactive({
  tmptraindt2 <- getData()
  tmptraindt2[trainingRowIndex2(), ]
})

testData2 <- reactive({
  tmptestdt2 <- getData()
  tmptestdt2[-trainingRowIndex2(),]
})

getData2 <- reactive ({
  home
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
                                 n.trees = c(1:10) , 
                                 shrinkage = 0.1,
                                 n.minobsinnode = c(10,20, 40)),
          trControl = trainControl(method = 'cv', number = 5), verbose=FALSE)
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
        tuneGrid = 
          data.frame(mtry = 1:sqrt(ncol(train))))
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
    c <- x[ , c(2, 3, 4)]
    # y <- rf$bestTune[[1]]
    # c <- x[c(1), c(2, 3, 4)]
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
    d <- x %>% filter(shrinkage == shrink)
    d <- d %>% filter(interaction.depth == inter)
    d <- d %>% filter(n.minobsinnode == mino)
    d <- d %>% filter(n.trees == tree)
    c <- d[c(1), c(5, 6, 7)]
    print(c)
  }
})

linPred <- reactive ({
  response <- input$response
  test <- testData()
  ix <- which(colnames(test) %in% c(input$response))
  test <- test[ ,-ix]
  linearPred <- predict(linFit(), newdata = test)
})

output$linPredsum <- renderPrint ({
  if (input$action) {
    summary(linFit())
  }
})

linRMSE2 <- reactive ({
  response <- input$response
  test <- testData()
  test <- test[, c(input$response), drop = TRUE]
  linRMSE2<- postResample(linPred(), obs = test)
})

rfPred <- reactive ({
  response <- input$response
  test <- testData()
  ix <- which(colnames(test) %in% c(input$response))
  test <- test[ ,-ix]
  rfPred <- predict(rfFit(), newdata = test)
})

boostPred <- reactive ({
  response <- input$response
  boost <- boostFit()
  test <- testData()
  ix <- which(colnames(test) %in% c(input$response))
  test <- test[ ,-ix]
  x <- boost$results
  tree <- boost$bestTune[[1]]
  boostPred <- predict(boostFit(), newdata = test, n.trees = tree)
})


output$linRMSE2P <- renderPrint ({
  if (input$action) {
    print(linRMSE2())
  }
})

rfRMSE2 <- reactive ({
    response <- input$response
    test <- testData()
    testSub <- test[ , c(response), drop = TRUE]
    rfRMSE2 <- postResample(rfPred(), obs = testSub)
})

output$rfRMSE2P <- renderPrint ({
  if (input$action) {
    print(rfRMSE2())
  }
})

boostRMSE2 <- reactive ({
  response <- input$response
  test <- testData()
  testSub <- test[ , c(response), drop = TRUE]
  boostRMSE2 <- postResample(boostPred(), obs = testSub)
})

output$boostRMSE2P <- renderPrint ({
  if (input$action) {
    print(boostRMSE2())
  }
})

modelsRMSE <- reactive ({
  data.frame(Linear.Model=linRMSE2()[1],
             Random.Forest.Model=rfRMSE2()[1], 
             Boost.Model=boostRMSE2()[1])
})

output$smallest <- renderPrint ({
  if (input$action) {
  smallest_RMSE<-colnames(modelsRMSE())[apply(modelsRMSE(),1,which.min)]
  paste0("When the response variable is ", input$response, " and the predictor is ",
         input$predictors, " the ", smallest_RMSE, " is the winner")
  }
})

linFitAll <- reactive ({
  trains <- trainingData()
  train(price ~ bedrooms + bathrooms + sqftLiving + sqftLot + waterfrontFac +
          decadeBuiltFac + yrBuilt,
        data = trains,
        method = 'lm',
        preProcess = c("center", "scale"),
        trControl = trainControl(method = "cv", number = 5))
})

boostFitAll <- reactive ({
  train <- trainingData()
  train(price ~ bedrooms + bathrooms + sqftLiving + sqftLot + waterfrontFac +
          decadeBuiltFac + yrBuilt, 
        data = train,
        method = "gbm",
        preProcess = c("center", "scale"),
        tuneGrid = expand.grid(interaction.depth = c(1,4,7), 
                               n.trees = c(1:10) , 
                               shrinkage = 0.1,
                               n.minobsinnode = c(10,20, 40)),
        trControl = trainControl(method = 'cv', number = 5), verbose=FALSE)
})

rfFitAll <- reactive ({
  train <- trainingData()
  train(price ~ bedrooms + bathrooms + sqftLiving + sqftLot + waterfrontFac +
          decadeBuiltFac + yrBuilt, 
        data = train,
        method = "rf",
        trControl = trainControl(method = "cv",
                                 number = 5),
        preProcess = c("center", "scale"),
        tuneGrid = 
          data.frame(mtry = 1:sqrt(ncol(train))))
})

data <- reactive({
  df <- data.frame(expand.grid(bedrooms = input$bedrooms, bathrooms = input$bathrooms, 
                               sqftLiving = input$living,
                               sqftLot = input$lot, yrBuilt = input$yearBui, 
                               waterfrontFac = input$waterfront,
                               decadeBuiltFac = input$decade))
})

output$proPred <- renderDataTable ({
  data.frame(expand.grid(bedrooms = input$bedrooms, bathrooms = input$bathrooms, 
                         sqftLiving = input$living,
                         sqftLot = input$lot, yrBuilt = input$yearBui, 
                         waterfrontFac = input$waterfront,
                         decadeBuiltFac = input$decade))
})

output$predSpec <- renderPrint ({
  if (input$modelChoice == 1 & input$prediction) {
    a <- predict(linFitAll(), data())
    print(a)
  }
  else if (input$modelChoice == 2 & input$prediction) {
    a <- predict(rfFitAll(), data())
    print(a)
  }
  else if (input$modelChoice == 3 & input$prediction) {
    a <- predict(boostFitAll(), data())
    print(a)
  }
})

splitSlider2 <- reactive({
  input$Slider2 / 100
})

output$cntTrain2 <-
  renderText(paste("Train Data:", nrow(trainingData2()), "records"))
output$cntTest2 <-
  renderText(paste("Test Data:", nrow(testData2()), "records"))

#Data
#______________________________________________________________________________________________________
subs <- reactive ({
  if (length(input$col) == 0) {
    return(home)
  }
  else if (length(input$col) != 0) {
    home %>% dplyr::select(!!!input$col)
  }
})

output$sub <- renderDataTable({
  subs()
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste(input$col, ".csv", sep = "")
  },
  content = function(file) {
    write.csv(subs(), file, row.names = FALSE)
})
  
#End function
#_______________________________________________________________________________________________________
})

  