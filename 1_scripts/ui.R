library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(leaflet)
library(shinyWidgets)
source("./server.R")

ui = navbarPage(
  "SNP tracker",
  tabPanel(title="Map viewer", value="pan1", 
           leafletOutput("map"),
           sliderTextInput(
             "animation", "Year:",
             choices = seq(from=start_time, to=end_time, by=-time_step),
             selected = start_time,
             width="100%",
             post=" BP",
             animate=animationOptions(interval=-time_step, loop=TRUE)
           )),
  tabPanel(title="Data explorer", value="pan2")
)