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
    if (input$grade == '9th grade'){
    th9 <- student %>% filter(Grade == 9)
    g <- ggplot(th9, aes(x = Height_in))
    m <- g + geom_boxplot() + coord_flip()
    } else {
      print('none')
    }
  })
})