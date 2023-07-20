library(ggplot2)
library(shiny)
library(tidyverse)

shinyUI(fluidPage(
  
  # Application title
  titlePanel('Project 3'),
  actionButton('about', 'About'),
  actionButton('dataex', 'Data Exploration'),
  actionButton('modeling', 'Modeling'),
  actionButton('data', 'Data'),
  # About
  div(
    class = 'About Page',
    navbarPage(
      "About age")
  ),
  #Data Exploration
  div(
    class = 'Data Exploration',
    navbarPage("Data Exploration")
  ),
  #Modeling
  div(
    class = 'Modeling',
    navbarPage(
      "Modeling")
  ),
  #Data
  div(
    class = 'Data',
    navbarPage("Data")
  ),
  # Sidebar with options for the data set
    sidebarPanel(
      
    ),
    
    # Show outputs
    mainPanel(
    )
    ))
 