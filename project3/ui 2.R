library(ggplot2)
library(shiny)
library(tidyverse)

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
   tabPanel('Data Exploration',
    # titlePanel('Data Exploration'),
      titlePanel(uiOutput("title")),
        sidebarLayout(
          sidebarPanel(
             selectInput("var", 
                         label = h6("Select variable",
                                    style = "color:#FB6107;"),
                         choices = list("Armspan (in)" = "Armspan_in",
                                        "Height (in)" = "Height_in",
                                        "Commute Time to School (min)" =
                                          "TravelTimeSchool",
                                        "School Night Sleep (hrs)" =
                                          "SleepHrsSchool",
                                        "Nonschool Night Sleep (hrs)" =
                                          "SleepsHrsNonSchool",
                                        "Number of Occupants in Home" =
                                          "HomeOccup"),
                         selected = "Armspan_in"),
              selectInput(inputId = "grade" ,
                          label = h6("Select Grade Level",
                                     style = "color:#FB6107;"),
                          choices = list('All grades',
                                         "9th grade" = 9,
                                         "10th grade" = 10,
                                         "11th grade" = 11,
                                         '12th grade' = 12),
                          selected = "All grades"),
               selectInput(inputId = 'type',
                           label = h6("Select Graph", 
                                      style = 'color:#FB6107;'),
                           choices = list('Boxplot' = 1,
                                          'Histogram' = 2),
                           selected = 1),
               checkboxInput("gender", 
                             h6("Color Code by Gender",
                                style = "color:#FB6107;")),
               selectInput("stat", 
                           label = h6("Select statistic",
                                      style = "color:#FB6107;"),
                           choices = list("Median" = "median",
                                          "Mean" = 'mean',
                                          "Standard Deviation" = "sd"),
                           selected = "median")
                ),
           mainPanel(
               plotOutput('height'),
               dataTableOutput('table')
           )
   )),
             tabPanel('Modeling',
                      titlePanel('Modeling'),
                      sidebarLayout(
                        sidebarPanel(
                          
                        ),
                        mainPanel(
                          
                        )
                      )),
             tabPanel('Data',
                      titlePanel('Data'),
                      sidebarLayout(
                        sidebarPanel(
                          
                        ),
                        mainPanel(
                          
                        )
                      ))
  )))