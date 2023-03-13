library(shiny)
source("./server.R", local=FALSE)
source("./ui.R", local=TRUE)

shinyApp(ui=ui, server=server, options=c(launch.browser = .rs.invokeShinyPaneViewer))