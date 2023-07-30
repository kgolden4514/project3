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
        sidebarPanel(

        ),
        mainPanel(

        )
   )),
#Code for Data Exploration ----------------------------------------------------
   navbarMenu('Data Exploration',
#Code for Quantitative---------------------------------------------------------
     tabPanel('Quantitative',
     # titlePanel('Data Exploration'),
     titlePanel(uiOutput("quant_title")),
     sidebarLayout(
       sidebarPanel(
         selectInput(inputId = "grade" ,
                     label = h4("Select Grade Level",
                                style = "color:#00A8C9;"),
                     choices = list('All grades',
                                    "9th grade" = 9,
                                    "10th grade" = 10,
                                    "11th grade" = 11,
                                    '12th grade' = 12),
                     selected = "All grades"),
         selectInput("var", label = h4("Select variable",
                                       style = "color:#00A8C9;"),
                     choices = list("Height (in)" = "Height",
                                    "Armspan (in)" = "Armspan",
                                    'Right Foot Length' =
                                      'RightFoot',
                                    'Index Finger Length (mm)' =
                                      'IndexFinger',
                                    "Commute Time to School (min)" =
                                      "CommuteTime",
                                    'Reaction Time (sec)' =
                                      'ReactionTime',
                                    'Memory Time (min)' = 'MemoryTime',
                                    'School Night Sleep (hrs)' =
                                      'SchoolNight',
                                    'Nonschool Night Sleep (hrs)' =
                                      'NonschoolNight',
                                    'Number of Home Occupants' =
                                      'HomeOcc'),
                     selected = "Height"),
         selectInput(inputId = 'type',
                     label = h4("Select Graph",
                                style = 'color:#00A8C9;'),
                     choices = list('Boxplot' = 1,
                                    'Histogram' = 2),
                     selected = 1),
         checkboxInput("gender", h4("Color Code by Gender",
                                    style = "color:#00A8C9;")),
         selectInput("stat", label = h4("Select statistic",
                                        style = "color:#00A8C9;"),
                     choices = list("Median" = "median",
                                    "Mean" = 'mean',
                                    "Standard Deviation" = "sd"),
                     selected = "median"),
       ),
        mainPanel(
          plotOutput('graph'),
          dataTableOutput('table')
       )
   )),
#Code for Categorical---------------------------------------------------------
      tabPanel('Categorical',
        titlePanel(uiOutput("cat_title")),
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "grade2" ,
                        label = h4("Select Grade Level",
                                   style = "color:#00A8C9;"),
                        choices = list('All grades',
                                       "9th grade" = 9,
                                       "10th grade" = 10,
                                       "11th grade" = 11,
                                       '12th grade' = 12),
                        selected = "All grades"),
            selectInput("vars", label = h4("Select variable",
                                          style = "color:#00A8C9;"),
                        choices = list("Year" = "Year",
                                       'State' = 'State',
                                       'Age' = 'Age',
                                       "Transportation to School" =
                                         "TransMethod",
                                       "Favorite Physical Activity" =
                                         "FavPhysAct",
                                       'Birth Month' = 'BirthMonth',
                                       'Favorite Season' = 'FavSeason',
                                       "Favorite School Subject" =
                                         "FavSubj",
                                       "Planned Education Level" =
                                         "PlannedEd",
                                       'Superpower' = 'Superpower',
                                       'RoleModel' = 'RoleModel'),
                        selected = NULL),
            checkboxInput("gender2", h4("Color Code by Gender",
                                       style = "color:#00A8C9;")),
              ),
           mainPanel(
             plotOutput('cat_graph'),
             verbatimTextOutput('kable')
              )
    ))),
#Code for Modeling--------------------------------------------------------------
   navbarMenu('Modeling',
#Code for Modeling Info---------------------------------------------------------
   tabPanel('Modeling Info',
     sidebarLayout(
       sidebarPanel(

       ),
      mainPanel(

      )
   )),
#Code for Fitting--------------------------------------------------------------
   tabPanel('Model Fitting',
     titlePanel('Model Fitting'),
     sidebarLayout(
       sidebarPanel(
         sliderInput(
           "Slider1",
           label = h6("Train/Test Split %"),
           min = 0,
           max = 100,
           value = 75),
         textOutput("cntTrain"),
         textOutput("cntTest"),
         checkboxGroupInput("SelectX",
                            label = h6("Select variables:"),
                            choices = names(student),
                            selected = names(student)),
         # selectInput(inputId = "SelectX", 
         #             label = "Independent Variables", 
         #             multiple = TRUE, 
         #             choices = as.list(names(student)), 
         #             selected = names(student)[1]),
         # verbatimTextOutput(outputId = "RegOut"),
         selectInput("SelectY", 
                     label = "Select variable to predict:", 
                     choices = names(student)),
         
       ),
       mainPanel(
         tabBox(
           id = "tabset1",
           height = "1000px",
           width = 12,
           tabPanel(
             "Data Summary",
             box(withSpinner(verbatimTextOutput("Summ")), width = 6),
             box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)),
           tabPanel("Plots",
                    box(withSpinner(plotOutput(
                      "Corr")), width = 12))
         ),
       )
    )),
#Code for Prediction-----------------------------------------------------------
   tabPanel('Prediction',
     sidebarLayout(
       sidebarPanel(

       ),
       mainPanel(

       )
     ))),
#Code for Data-----------------------------------------------------------------
   tabPanel('Data',
     titlePanel('Data'),
     sidebarLayout(
       sidebarPanel(

       ),
       mainPanel(
         tabBox(
           id = "tabset1",
           height = "1000px",
           width = 12,
           
           tabPanel("Data",
                    box(withSpinner(dataTableOutput(
                      "Data"
                    )), width = 12))),

       )
   ))
)))