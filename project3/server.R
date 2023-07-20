library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

shinyServer(function(input, output, session) {
  setwd("C:/Documents/Github/project3")
  nhl <- read.csv('hockeydata.csv')
  nhl <- nhl[ , -c(51:102)]
})