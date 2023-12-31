library(ggplot2)
library(shiny)
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

#read in dataset
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


shinyUI(fluidPage(theme = shinytheme("cyborg"),
navbarPage('Golden Project 3',
#Code for About Page ---------------------------------------------------------     
tabPanel('About',
  h1('About'),
    h3('Purpose of the app:'),
    textOutput('purpose'),
    h3('Data Description:'),
    textOutput('dataDesc'),
    uiOutput('link'),
    img(src="kaggle.jpg", width="10%", height = '10%', align="center"),
    h3('Page Descriptions:'),
    h5('About Page'),
    textOutput('aboutDesc'),
    h5('Data Exploration Page'),
    textOutput('dataEx'),
    h5('Modeling Page'),
    textOutput('modeling'),
    h3("Data Page:"),
    textOutput('dataPage')
  ),
navbarMenu('Data Exploration',
  tabPanel('Variables',
    titlePanel(uiOutput('title')),
    sidebarLayout(
    sidebarPanel(
      selectInput('dec', 'Choose Decade', selected = "All Decades",
                  choices = list('All Decades',
                                 '1900s',
                                 '1910s',
                                 '1920s',
                                 '1930s',
                                 '1940s',
                                 '1950s',
                                 '1960s',
                                 '1970s',
                                 '1980s',
                                 '1990s',
                                 '2000s')),
      selectInput('quantCat', 'Choose a variable type',
                  choices = list('Quantitative',
                                 'Categorical')),
      uiOutput('vars'),
      uiOutput('type'),
      checkboxInput('year', 'Color Code by Year Note: All Decades will code 
                    by Decade'),
      uiOutput('statChoice'),
    ),
    mainPanel(
      plotOutput('graph'),
      uiOutput('quantTableY'),
      uiOutput('kabley')
    )
  )),
  
tabPanel('EDA',
titlePanel('EDA'),
  sidebarLayout(
  sidebarPanel(
    selectInput('resp', 'Choose response',
                choices = list('Price' = 'price',
                               '# of Bedrooms' = 'bedrooms',
                               '# of Bathrooms' = 'bathrooms',
                               'Living Space (sqft)' = 'sqftLiving',
                               'Lot Space (sqft)' = 'sqftLot',
                               '# of Floors' = 'floors',
                               'Waterfront'= 'waterfrontYN',
                               'Basement' = 'basementYN',
                               'Renovated' = 'renovatedYN')),
    actionButton('begin', "Begin"),
   ),
  mainPanel(tabBox(
           id = "tabset1",
           height = "1000px",
           width = 12,
           tabPanel(
             "Data Summary",
             box(withSpinner(verbatimTextOutput('ySum')), title = 'Summary of Response', width = 6)
             ),
           tabPanel(
             "Plots",
             box(withSpinner(plotOutput('ggp'))),
             box(withSpinner(plotOutput('corrp')))
           ),
   )
)))),

navbarMenu('Modeling',
    tabPanel('Model Info',
    titlePanel('Model Info'),
    h5('Multiple Linear Regression Model'),
    textOutput('modelInfoMLR'),
    uiOutput('ex3'),
    h5('Random Forest Regression Model'),
    textOutput('rfModelInfo'),
    h5('Boosted Tree Model'),
    textOutput('boostModelInfo')
    ),
tabPanel('Model Fitting',
  titlePanel('Model Fitting'),
    sidebarLayout(
    sidebarPanel(
      sliderInput(
        "Slider1", "Train/Test Split %", min = 0, max = 100, value = 75),
      textOutput("cntTrain"),
      textOutput("cntTest"),
      # selectInput("class", 
      #             label = "Select classification or regression", 
      #             choices = list('Regression' = 'reg',
      #                            'Classification' = 'class')),
      selectInput('response', 'Choose response',
                  choice = list('Price' = 'price',
                                '# of Bedrooms' = 'bedrooms',
                                '# of Bathrooms' = 'bathrooms',
                                'Living Space (sqrt)' = 'sqftLiving',
                                'Year Built' = 'yrBuilt',
                                'Lot Space (sqrt)' = 'sqftLot')),
      uiOutput('x'),
      actionButton("action", label = "Begin Fits"),
    ),
    mainPanel((tabBox(
      id = "tabset1",
      height = "1000px",
      width = 12,
      tabPanel('Fit Statistics',
        box(withSpinner(verbatimTextOutput('smallest')), title = 'Best Fit', 
                   width = 17),
        box(withSpinner(verbatimTextOutput('linRMSE')), title = 'Linear Fit on Training Data', 
            width = 12),
        box(withSpinner(verbatimTextOutput('linRMSE2P')), title = 'Linear Fit on Test Data', 
            width = 12),
        box(withSpinner(verbatimTextOutput('rfRMSE')), title = 'Random Forest Fit on Training Data', 
            width = 12),
        box(withSpinner(verbatimTextOutput('rfRMSE2P')), title = 'Random Forest Fit on Test Data', 
            width = 12),
        box(withSpinner(verbatimTextOutput('boostRMSE')), title = 'Boost Fit on Training Data', 
            width = 12),
        box(withSpinner(verbatimTextOutput('boostRMSE2P')), title = 'Boost Fit on Test Data', 
            width = 12)
        
      ),
      tabPanel("Summary",
        box(withSpinner(verbatimTextOutput('linSum')), title = 'Summary of linear fit'),
        box(withSpinner(verbatimTextOutput('rfSum')), width = 6, 
            title = 'Summary of random forest fit'),
        box(withSpinner(verbatimTextOutput('boostSum')), width = 6, title = 'Summary of boost fit')
      ),
      tabPanel("Plots",
        box(withSpinner(plotOutput('boostPlot')), width = 6, title = 'Plot of boost fit'),
        box(withSpinner(plotOutput('rfPlot')), width = 6, title = 'Plot of random forest fit')
      )
    )
    )
  ))),
tabPanel('Prediction',
titlePanel('Prediction'),
  sidebarLayout(
  sidebarPanel(
    sliderInput(
      "Slider2", "Train/Test Split %", min = 0, max = 100, value = 75),
    textOutput("cntTrain2"),
    textOutput("cntTest2"),
    radioButtons("modelChoice", "Choose your regression model",
                 choices = list("Linear Model" = 1, 
                                "Random Fores Model" = 2, 
                                "Boosting Model" = 3)),
    actionButton("prediction", label = "Get prediction"),
    sliderInput('bedrooms', label = '# of Bedrooms', min = min(house$bedrooms),
                max = max(house$bedrooms), step = 1, value = 3),
    sliderInput('bathrooms', label = '# of Bathrooms', min = min(house$bathrooms),
                max = max(house$bathrooms), step = 0.25, value = 2),
    sliderInput('living', label = 'Living Space (sqft)', min = min(house$sqftLiving),
                max = max(house$sqftLiving), step = 500,
                value = (max(house$sqftLiving)-min(house$sqftLiving))/2),
    sliderInput('lot', label = 'Lot Size (sqft)', min = min(house$sqftLot),
                max = max(house$sqftLot), step = 500,
                value = (max(house$sqftLot)-min(house$sqftLot))/2),
    sliderInput('yearBui', 'Year Built', min = min(house$yrBuilt),
                max = max(house$yrBuilt), step = 1, value = 1950),
    selectInput("waterfront", label = 'Waterfront property?',
                choices = list("No", 'Yes'),
                selected = "No"),
    selectInput("decade", label = 'Decade Built',
                choices = list('1900s', '1910s', '1920s', '1930s', '1940s', '1950s',
                               '1960s', '1970s', '1980s', '1990s', '2000s'),
                selected = '1900s'),
  ),
  mainPanel(
    box(withSpinner(dataTableOutput('proPred')), width = 12, 
        title = 'User provided predictor values.'),
    box(withSpinner(verbatimTextOutput('predSpec')), width = 12, 
        title = "Selected model's predicted price($).")
  ),
))),
tabPanel('Data',
titlePanel('Data'),
sidebarLayout(
sidebarPanel(
  varSelectInput("col", "Choose variable(s) to display. If none selected, 
                 all variables will be displayed.", 
                 home, multiple = TRUE),
  downloadButton("downloadData", "Download"),
),
mainPanel(
  box(withSpinner(dataTableOutput('sub')), width = 12)
),
))
)))
  
  