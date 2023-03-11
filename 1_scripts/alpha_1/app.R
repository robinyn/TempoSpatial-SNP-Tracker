library(shiny)
library(tidyverse)
library(tidyverse)
library(readxl)
source("./server.R", local=FALSE)
source("./ui.R", local=TRUE)

# cat(file=stderr(), "Doing application startup\n")
# onStop(function() {
#   cat(file=stderr(), "Doing application cleanup\n")
#   dbDisconnect(db)
# })

shinyApp(ui=ui, server=server, options=c(launch.browser = .rs.invokeShinyPaneViewer))