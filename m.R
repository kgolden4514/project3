x <- c('shiny', 'dplyr', 'ggplot2', 'tidyverse', 'scales', 'cowplot', 'knitr', 'caret', 'randomForest', 'shinydashboard', 'shinycssloaders', 'maps', 'leaflet', 'plotly', 'corrplot', 'stargazer', 'shinythemes', 'recipes', 'GGally', 'corrplot', 'htmltools', 'shinyWidgets', 'Metrics', 'abc')

# invisible(lapply(x, install.packages, character.only = TRUE))

invisible(lapply(x, library, character.only = TRUE))