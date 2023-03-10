library(shiny)
library(tidyverse)
library(tidyverse)
library(readxl)
source("./ui.R")
source("./server.R")

shinyApp(ui=ui, server=server, options=c(launch.browser = .rs.invokeShinyPaneViewer))
