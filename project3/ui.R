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

shinyUI(fluidPage(theme = shinytheme("cyborg"),
navbarPage('Golden Project 3',
#Code for About Page ---------------------------------------------------------     
tabPanel('About',
  titlePanel('About'),
  sidebarLayout(
  sidebarPanel(),
  mainPanel()
  )),
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
      uiOutput('statchoice'),
    ),
    mainPanel(
      plotOutput('graph'),
      uiOutput('quantTabley'),
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
             box(withSpinner(verbatimTextOutput('ysum')), title = 'Summary of Response', width = 6)
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
    sidebarLayout(
    sidebarPanel(),
    mainPanel()
  )),
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
        box(withSpinner(verbatimTextOutput('linRMSE')), title = 'Linear Model Fit Results', 
            width = 12),
        box(withSpinner(verbatimTextOutput('rfRMSE')), title = 'Random Forest Model Fit Results', 
            width = 12),
        box(withSpinner(verbatimTextOutput('boostRMSE')), title = 'Boost Model Fit Results', 
            width = 12)
      ),
      tabPanel("Summary",
        box(withSpinner(verbatimTextOutput('linsum')), title = 'Summary of linear fit'),
        box(withSpinner(verbatimTextOutput('rfsum')), width = 6, 
            title = 'Summary of random forest fit'),
        box(withSpinner(verbatimTextOutput('boostsum')), width = 6, title = 'Summary of boost fit')
      ),
      tabPanel("Plots",
        box(withSpinner(plotOutput('boostplot')), width = 6, title = 'Plot of boost fit'),
        box(withSpinner(plotOutput('rfplot')), width = 6, title = 'Plot of random forest fit')
      )
    )
    )
  ))),
tabPanel('Prediction',
titlePanel('Prediction'),
  sidebarLayout(
  sidebarPanel(),
  mainPanel()
  ),
)),
tabPanel('Data',
titlePanel('Data'),
sidebarLayout(
sidebarPanel(),
mainPanel()
)),
)))
  
  