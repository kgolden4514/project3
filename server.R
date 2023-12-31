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

#Read in dataset
# setwd("C:/Documents/Github/project3/project3")
house <- read.csv('./house.csv')
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
  # setwd("C:/Documents/Github/project3/project3")
  # house <- read.csv('./house.csv')
  # house$zipcode <- as.character(house$zipcode)
  # house$renovatedFac <- as.factor(house$renovatedYN)
  # house$basementFac<- as.factor(house$basementYN)
  # house$waterfrontFac <- as.factor(house$waterfrontYN)
  # house$yrBuiltFac <- as.factor(house$yrBuilt)
  # house$decadeBuiltFac <- as.factor(house$decadeBuilt)
  # house$zipcodeFac <- as.factor(house$zipcode)
  # house$yrBuiltCat <- as.character(house$yrBuilt)
  # home <- read.csv('house.csv')
#Create datasets
#________________________________________________________________________
#create dataset for house
  getData <- reactive({
  house
})

#Filter data set by decade
getDataDec <- reactive({
  v <- input$dec
  if (v != 'All Decades'){
    newData <- house %>% filter(decadeBuilt == v)
  }
  else if (v == 'All Decades') {
    newData <- house
  }
})

#Filter data by decade
getDataDec2 <- reactive({
  v <- input$dec2
  if (v != 'All Decades'){
    newData2 <- house %>% filter(decadeBuilt == v)
  }
  else if (v == 'All Decades') {
    newData2 <- house
  }
})

#Create train and test sets
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

#Create About page
#_______________________________________________________________________
#Purpose of app
output$purpose <- renderText ({
    paste('The purpose of the app is to look into the King County, Washington State housing sales from May 2014 to May 2015. This app allows us to look at the quantitative and categorical variables, create prediction models, and predict the house price based off of user inputed predictors.')
})

#Data description 
output$dataDesc <- renderText ({
  paste('The data consists of housing sales data from King County in Washington state from May 2014 to May 2015. It provides the price of the home as well as information about the home, such as square footage, views, number of bedrooms, number of bathrooms, etc. This data will allow us to create prediction regression models. The data was obtained from kaggle.com. I did remove the data points for homes built after 2009. I did this because the data set was large and I wanted to lower the computational requirement.')
})

#Create dataset link
url <- a('King County Dataset', href = 'https://www.kaggle.com/datasets/harlfoxem/housesalesprediction')
output$link <- renderUI({
  tagList(url)
})

#Question
output$aboutDesc <- renderText ({
  paste('Gives the user a brief overview of the data and the purpose of the app.')
})

#Data Exploration description
output$dataEx <- renderText ({
  paste("Allows us to look at the correlations between the variables, the distribution of the quantitative variables, and the frequency of the categorical variables.")
})

#Modeling page description
output$modeling <- renderText ({
  paste('Model info: Gives a brief explanation of each type of model used: multiple linear regression, random forest regression, and boosted tree regression. Model fitting: Allows the user to select the predictor variables to consider, the response varaible to consider, the training and test data partition percentage, and train and fit the data to each model. Prediction: Allows the user the provide values for the predictors to predict the price of a home using the model of their choice.')
})

#Data page description
output$dataPage <- renderText ({
  paste('Allows the user to choose which variables to include in a dataframe. Gives the option to download the dataframe as a .csv file')
})

#Modeling info MLR description
output$modelInfoMLR <- renderText ({
  paste("Multiple linear regression is used to examine the relationship between a response and mutiple predictor variables. Advantages: Because we can include both quantitative and categorical variables, it allows us to take into account more variables that might influence the response. Thus reducing error and bias. Disadvantages: The results can be hard to interpret. Also, assumptions and conditions can be hard to meet")
})

#Modeling infor random forest description
output$rfModelInfo <- renderText ({
  paste('A random forest model uses bootstrapping from the data and randomly selected predictors to find the best model. Advantages: because each tree will not include a highly correlated predictor, the model can be a better fit than other bagged methods. Disadvantage: The results can be hard to interpret and it can be computationally expensive.')
})

#Boosted model ino description
output$boostModelInfo <- renderText ({
  paste('A boosted tree model uses bootstrapping to grow trees sequentially. Each following tree is grown on a modified version of the original data. The predictions are then updated as the tree grows. Advantages: Often the best ensemble method. Disadvantages: The results are hard to interpret and it can be computationally expensive.')
})

#Create equation for MLR
output$ex3 <- renderUI({
  withMathJax('$$Y_i= β_0+ β_1 x_1i+ β_2 x_2i+ β_3 x_1i x_2i+ E_i$$')
})
#Create quantitative outputs
#______________________________________________________________________________________________________

#Create selectINput for quantitative variables
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

#Create select input for graph type
output$type <- renderUI ({
  if (input$quantCat == 'Quantitative') {
    selectInput('type', 'Choose graph type',
                choices = list('boxplot' = 1,
                               'histogram' = 2))
  }
})

#Create plot for quantitative variables
output$graph <- renderPlot ({
  type <- input$type
  var <- input$var
  dec <- input$dec
  newData <- getDataDec()
  g <- ggplot(newData, aes_string(x = var))
  if ((type == 1) &  !input$year) {
    newData <- getDataDec()
    m <- g + geom_boxplot(color = '#FF1493', fill = '#FF1493', alpha = 0.2) +
      coord_flip()
    print(m)
  }
  else if ((type == 1) & input$year & dec != "All Decades") {
    newData <- getDataDec()
    m <- g + geom_boxplot(aes(color = yrBuiltCat, fill = yrBuiltCat), alpha = 0.2) + 
      coord_flip()
    print(m)
  }
  else if ((type == 1) & input$year & dec == "All Decades") {
    newData <- getDataDec()
    m <- g + geom_boxplot(aes(color = decadeBuilt, fill = decadeBuilt), 
                          alpha = 0.2) + coord_flip()
    print(m)
  }
  else if ((type == 2) & !input$year) {
    newData <- getDataDec()
    m <- g + geom_histogram(color = '#FF1493', fill = '#FF1493', alpha=0.2)
    print(m)
  }
  else if ((type ==2) & input$year & (dec == "All Decades")) {
    newData <- getDataDec()
    m <- g + geom_histogram(aes(color = decadeBuilt, fill = decadeBuilt), alpha=0.2)
    print(m)
  }
  else if ((type ==2) & input$year & (dec != "All Decades")) {
    newData <- getDataDec()
    m <- g + geom_histogram(aes(color = yrBuiltCat, fill = yrBuiltCat), alpha = 0.2)
    print(m)
  }
  if (!input$year & (input$quantCat == "Categorical")) {
    newData <- getDataDec()
    m <- g +
      geom_bar(color = '#862efd', fill = '#862efd', alpha = 0.2)
    print(m)
  }
  else if (input$year & (input$quantCat == 'Categorical') & (input$dec == "All Decades")){
    newData <- getDataDec()
    m <- g + geom_bar(aes(color = decadeBuilt, fill = decadeBuilt), alpha = 0.2)
    print(m)
  }
  else if (input$year & (input$quantCat == 'Categorical') & (input$dec != "All Decades")){
    newData <- getDataDec()
    m <- g + geom_bar(aes(color = yrBuiltCat, fill = yrBuiltCat), alpha = 0.2)
    print(m)
  }
})

#create selectInput for statistics
output$statChoice <- renderUI ({
  if (input$quantCat == 'Quantitative')
    selectInput('stat', "Choose stat",
                choices = list('Mean' = 'mean',
                               'Median' = 'median',
                               'Standard Deviation' = 'sd'))
})

#Create data table for stat
output$quantTable <- renderDataTable ({
  if (input$quantCat == 'Quantitative') {
  var <- input$var
  stat <- input$stat
  newData <- getDataDec()
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

#Create part of title for data page
output$quantTableY <- renderUI ({
  if (input$quantCat == 'Quantitative') {
    dataTableOutput('quantTable')
  }
})

#Create part of title for data page
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

#Create frequency table output
output$kabley <- renderUI ({
  if (input$quantCat == 'Categorical') {
    verbatimTextOutput('kable')
  }
})

#Creat frequency table
output$kable <- renderPrint ({
  var <- input$var
  dec <- input$dec
  newData <- getDataDec()
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
#Create summary for each response variable
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

#Get structure of data set
output$structure <- renderPrint ({
  if (input$begin){
  str(getData())
}
})

#Create summary of each y value
output$ySum <- renderPrint ({
  if (input$begin) {
  var <- input$resp
  newData <- getData()
  newDataSub <- newData[ , c(var), drop = FALSE]
  summary(newDataSub)
  }
})

#Create correlation plot
output$ggp <- renderPlot ({
  if (input$begin) {
  newData <- getData()
  newData <- newData[ , c(1:6, 9)]
  ggpairs(newData)
  }
})

#Create correlation plot
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
#divide the slider input by 100
splitSlider <- reactive({
  input$Slider1 / 100
})

#Output the size of the training and test data
output$cntTrain <-
  renderText(paste("Train Data:", nrow(trainingData()), "records"))
output$cntTest <-
  renderText(paste("Test Data:", nrow(testData()), "records"))

#Create selectInput for the predictor variables
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

#Linear fit for user selected inputs
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

#Linear fit summary
output$linSum <- renderPrint ({
  if (input$action) {
    summary(linFit())
  }
})

#Linear fit plot
output$linPlot <- renderPlot ({
  if (input$action) {
    plot(linFit())
  }
})

#Boost fit for user selected inputs
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

#Boosted tree plot
output$boostPlot <- renderPlot ({
  if (input$action) {
  plot(boostFit())
  }
})

#Boosted tree summary
output$boostSum <- renderPrint ({
  if (input$action){
  print(boostFit())
  }
})

#Random forest fit using user selected inputs
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

#Create random forest plot
output$rfPlot <- renderPlot ({
  if (input$action) {
  plot(rfFit())
  }
})

#Create random forest summary
output$rfSum <- renderPrint ({
  if (input$action) {
  print(rfFit())
  }
})

#Linear RMSE
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

#Random forest RMSE
output$rfRMSE <- renderPrint ({
  if (input$action) {
    rf <- rfFit()
    x <- rf$results
    y <- rf$bestTune[[1]]
    c <- x[y, c(2, 3, 4)]
    print(c)
  }
})

#Boost RMSE
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

#Linear prediction
linPred <- reactive ({
  response <- input$response
  test <- testData()
  ix <- which(colnames(test) %in% c(input$response))
  test <- test[ ,-ix]
  linearPred <- predict(linFit(), newdata = test)
})

#Linear prediction summary
output$linPredSum <- renderPrint ({
  if (input$action) {
    summary(linFit())
  }
})


#Linear fit to test data
linRMSE2 <- reactive ({
  response <- input$response
  test <- testData()
  test <- test[, c(input$response), drop = TRUE]
  linRMSE2<- postResample(linPred(), obs = test)
})

#Random forest prediction
rfPred <- reactive ({
  response <- input$response
  test <- testData()
  ix <- which(colnames(test) %in% c(input$response))
  test <- test[ ,-ix]
  rfPred <- predict(rfFit(), newdata = test)
})

#Boosted prediction
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

#Linear test RMSE
output$linRMSE2P <- renderPrint ({
  if (input$action) {
    print(linRMSE2())
  }
})

#Random forest test RMSE
rfRMSE2 <- reactive ({
    response <- input$response
    test <- testData()
    testSub <- test[ , c(response), drop = TRUE]
    rfRMSE2 <- postResample(rfPred(), obs = testSub)
})

#Print random forest test RMSE
output$rfRMSE2P <- renderPrint ({
  if (input$action) {
    print(rfRMSE2())
  }
})

#Get boosted test RMSE
boostRMSE2 <- reactive ({
  response <- input$response
  test <- testData()
  testSub <- test[ , c(response), drop = TRUE]
  boostRMSE2 <- postResample(boostPred(), obs = testSub)
})

#Print boosted test RMSE
output$boostRMSE2P <- renderPrint ({
  if (input$action) {
    print(boostRMSE2())
  }
})

#Create data frame of all three RMSE
modelsRMSE <- reactive ({
  data.frame(Linear.Model=linRMSE2()[1],
             Random.Forest.Model=rfRMSE2()[1], 
             Boost.Model=boostRMSE2()[1])
})

#Choose the lowest RMSE
output$smallest <- renderPrint ({
  if (input$action) {
  smallest_RMSE<-colnames(modelsRMSE())[apply(modelsRMSE(),1,which.min)]
  paste0("When the response variable is ", input$response, " and the predictor is ",
         input$predictors, " the ", smallest_RMSE, " is the winner")
  }
})

#Use linear fit to predict price for many predictors
linFitAll <- reactive ({
  trains <- trainingData()
  train(price ~ bedrooms + bathrooms + sqftLiving + sqftLot + waterfrontFac +
          decadeBuiltFac + yrBuilt,
        data = trains,
        method = 'lm',
        preProcess = c("center", "scale"),
        trControl = trainControl(method = "cv", number = 5))
})

#Use boosted tree fit to predict price for many predictors
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

#Use random forest fit to predict price for many predictors
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

#Create data frame for use in predictions
data <- reactive({
  df <- data.frame(expand.grid(bedrooms = input$bedrooms, bathrooms = input$bathrooms, 
                               sqftLiving = input$living,
                               sqftLot = input$lot, yrBuilt = input$yearBui, 
                               waterfrontFac = input$waterfront,
                               decadeBuiltFac = input$decade))
})

#Create data frame of user input
output$proPred <- renderDataTable ({
  data.frame(expand.grid(bedrooms = input$bedrooms, bathrooms = input$bathrooms, 
                         sqftLiving = input$living,
                         sqftLot = input$lot, yrBuilt = input$yearBui, 
                         waterfrontFac = input$waterfront,
                         decadeBuiltFac = input$decade))
})

#Calculate price for user inputs
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
#Create subset data for data page
subs <- reactive ({
  if (length(input$col) == 0) {
    return(home)
  }
  else if (length(input$col) != 0) {
    home %>% dplyr::select(!!!input$col)
  }
})

#Create data table for subsetted data
output$sub <- renderDataTable({
  subs()
})

#Give download option
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

  