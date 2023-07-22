library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

shinyServer(function(input, output, session) {
  setwd("C:/Documents/Github/project3")
  student <- read.csv('data.csv')
  
  # Create Plot
  output$height <- renderPlot({
    #get filtered data
    if (input$grade == '9th grade' & !input$gender){
      filt <- student %>% filter(Grade == 9)
      g <- ggplot(filt, aes(x = Height_in)) 
      m <- g + geom_boxplot() + coord_flip() + 
        labs(title="9th Graders Height in Inches",
             x = "Height in Inches", 
             y = "9th Grade")
      print(m)
    }
    else if (input$grade == '9th grade' & input$gender) {
      filt <- student %>% filter(Grade == 9)
      g <- ggplot(filt, aes(x = Height_in, color = Gender))
      m <- g + geom_boxplot() + coord_flip() + 
        labs(title="9th Graders Height in Inches by Gender",
             x = "Height in Inches", 
             y = "9th Grade")
      print(m)
    }
    else if (input$grade == '10th grade' & !input$gender){
      filt <- student %>% filter(Grade == 10)
      g <- ggplot(filt, aes(x = Height_in))
      m <- g + geom_boxplot() + coord_flip() + 
        labs(title="10th Graders Height in Inches",
             x = "Height in Inches", 
             y = "10th Grade")
      print(m)
    }
    else if (input$grade == '10th grade' & input$gender) {
      filt <- student %>% filter(Grade == 10)
      g <- ggplot(filt, aes(x = Height_in, color = Gender))
      m <- g + geom_boxplot() + coord_flip() + 
        labs(title="10th Graders Height in Inches by Gender",
             x = "Height in Inches", 
             y = "10th Grade")
      print(m)
    }
    else if (input$grade == '11th grade' & !input$gender) {
      filt <- student %>% filter(Grade == 11)
      g <- ggplot(filt, aes(x = Height_in))
      m <- g + geom_boxplot() + coord_flip() + 
        labs(title="11th Graders Height in Inches",
             x = "Height in Inches", 
             y = "11th Grade")
      print(m)
    }
    else if (input$grade == '11th grade' & input$gender) {
      filt <- student %>% filter(Grade == 11)
      g <- ggplot(filt, aes(x = Height_in, color = Gender))
      m <- g + geom_boxplot() + coord_flip() + 
        labs(title="11th Graders Height in Inches by Gender",
             x = "Height in Inches", 
             y = "11th Grade")
      print(m)
    }
    else if (input$grade == '12th grade' & !input$gender) {
      filt <- student %>% filter(Grade == 12)
      g <- ggplot(filt, aes(x = Height_in))
      m <- g + geom_boxplot() + coord_flip() + 
        labs(title="12th Graders Height in Inches",
             x = "Height in Inches", 
             y = "12th Grade")
      print(m)
    }
    else if (input$grade == '12th grade' & input$gender) {
      filt <- student %>% filter(Grade == 12)
      g <- ggplot(filt, aes(x = Height_in, color = Gender))
      m <- g + geom_boxplot() + coord_flip() + 
        labs(title="12th Graders Height in Inches by Gender",
             x = "Height in Inches", 
             y = "12th Grade")
      print(m)
    }
  })
})