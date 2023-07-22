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
     titlePanel('Data Exploration'),
     sidebarLayout(
       sidebarPanel(
         radioButtons(inputId = "grade" , 
                      label = h6(strong("Select Grade Level")), 
                      choices = c("9th grade",
                                  "10th grade", 
                                  "11th grade",
                                  '12th grade'),
                      selected = "9th grade"),
         checkboxInput("gender", 
                       h4("Color Code Gender", 
                          style = "color:#FB6107;")),
       ),
        mainPanel(
          plotOutput('height')
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