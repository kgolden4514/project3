library(ggplot2)
library(shiny)
library(tidyverse)
library(scales)
library(cowplot)
library(knitr)
library(caret)

shinyUI(fluidPage(
  navbarPage('Golden Project 3',
      tabPanel('About',
      titlePanel('About'),
      sidebarLayout(
        sidebarPanel(

        ),
        mainPanel(
          
        )
   )),
   navbarMenu('Data Exploration',
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
   
   navbarMenu('Modeling',
   tabPanel('Modeling Info',
     sidebarLayout(
       sidebarPanel(

       ),
      mainPanel(

      )
   )),
   tabPanel('Model Fitting',
     sidebarLayout(
       sidebarPanel(
         textInput("prop", label = h4("Enter data partition 
                                      proportion"),
                   value = 'Must be [0,1]'),
       ),
       mainPanel(
         
       )
    )),
   tabPanel('Prediction',
     sidebarLayout(
       sidebarPanel(
         
       ),
       mainPanel(
         
       )
     ))),
   tabPanel('Data',
     titlePanel('Data'),
     sidebarLayout(
       sidebarPanel(
         
       ),
       mainPanel(
         
       )
   ))
)))