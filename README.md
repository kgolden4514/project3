# project3

#The purpose of this app is to look into the King County Washington USA house sales. The app allows us to look at the quantiative variables and categorical variables. We can also fit multiple linear regression, random forest model, and boost tree models. 

x <- c('shiny', 'dplyr', 'ggplot2', 'tidyverse', 'scales', 'cowplot', 'knitr', 'caret', 'randomForest', 'shinydashboard', 'shinycssloaders', 'maps', 'leaflet', 'plotly', 'corrplot', 'stargazer', 'shinythemes', 'recipes', 'GGally', 'corrplot', 'htmltools', 'shinyWidgets', 'Metrics', 'abc')

invisible(lapply(x, install.packages, character.only = TRUE))

invisible(lapply(x, library, character.only = TRUE))

shiny::runGitHub('kgolden4514/project3')
