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
  tabPanel('Quantitative',
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
      selectInput('quant', 'Choose Quantitative Variable', selected = 'price',
                  choices = list('Price' = 'price',
                                 '# of Bedrooms' = 'bedrooms',
                                 '# of Bathrooms' = 'bathrooms',
                                 'Living Space (sqft)' = 'sqftLiving',
                                 'Lot Space (sqft)' = 'sqftLot',
                                 '# of Floors' = 'floors')),
      checkboxInput('year', 'Color Code by Year Note: All Decades will code 
                    by Decade'),
      selectInput('type', 'Choose graph type', selected = 1,
                  choices = list('Boxplot' = 1,
                                 'Histogram' = 2)),
      selectInput('stat', 'Choose the Statistic', selected = 'mean',
                  choices = list('Mean' = 'mean',
                                 'Median' = 'median',
                                 'Standard Deviation' = 'sd')),
    ),
    mainPanel(
      plotOutput('graph'),
      dataTableOutput('quantTable')
    )
  )),
  tabPanel('Categorical',
    titlePanel(uiOutput('title2')),
    sidebarLayout(
    sidebarPanel(
      selectInput('dec2', 'Choose Decade', selected = "1900s",
                  choices = list('1900s',
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
      selectInput('cat', 'Choose Variables', selected = 'waterfrontYN',
                  choices = list('Waterfront'= 'waterfrontYN',
                                 'Basement' = 'basementYN',
                                 'Renovated' = 'renovatedYN')),
      checkboxInput('year2', 'Color Code by Year Note: All Decades will code 
                    by Decade'),
    ),
    mainPanel(
      plotOutput('catGraph'),
      verbatimTextOutput('kable')
    )
  )),
  tabPanel('EDA',
    titlePanel('EDA'),
    sidebarLayout(
    sidebarPanel(),
    mainPanel()
  ))
),

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
    sidebarPanel(),
    mainPanel()
  )),
  tabPanel('Prediction',
  titlePanel('Prediction'),
  sidebarLayout(
  sidebarPanel(),
  mainPanel()
  )),
),
tabPanel('Data',
titlePanel('Prediction'),
sidebarLayout(
sidebarPanel(),
mainPanel()
)),
)))
  
  