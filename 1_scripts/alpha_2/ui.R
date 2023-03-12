library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(RSQLite)

# SNP_tabs = tabsetPanel(
#   id="SNP_selector",
#   type="hidden",
#   tabPanel(
#     "1",
#     fluidRow(
#       column(width=4, uiOutput("CHR_choice")),
#       column(width=4, uiOutput("SNP_choice"))
#     )
#   )
# )

navbarPage("SNP Tracker", id="navbar", 
           tabPanel("Map", 
                    div(class="outer",
                        tags$head(includeCSS("styles.css")),
                        leafletOutput("map", width="100%", height="100%"),
                        
                        absolutePanel(id="controlPanel", class="panel panel-default", fixed=TRUE, draggable=TRUE,
                                      top=60,  left="auto", right=20, bottom="auto",
                                      width=330, height="auto",
                                      
                                      fluidPage(
                                        h1("Controls"),
                                        radioButtons("num_SNP", "Number of SNPs to track", choices = 1:2, inline=TRUE, selected=1),
                                        fluidRow(
                                          column(width=4, selectInput("CHR_choice1", "Chromosome", CHR_selection)),
                                          column(width=7, selectizeInput("SNP_choice1", "SNP1", choices=NULL, selected=NULL))
                                        ),
                                        conditionalPanel(
                                          condition="input.num_SNP==2",
                                          fluidRow(
                                            column(width=4, selectInput("CHR_choice2", "Chromosome", CHR_selection)),
                                            column(width=7, selectizeInput("SNP_choice2", "SNP2", choices=NULL, selected=NULL))
                                          )
                                        ),
                                        
                                        sliderInput("timestep", label="Time step", min=100, max=5000, value=time_step, step=100, ticks=TRUE)
                                      )
                                      
                                      )
                        )
                    )
           )