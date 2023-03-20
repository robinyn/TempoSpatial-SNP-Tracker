# Title: ui.R
# Author: Euisuk (Robin) Han
# Date: 19/Mar/2023
# Description: This is the client-side script for the R-shiny application "TempoSpatial SNP Tracker"
#              This script mainly defines the user-interface of the application.
# Dependencies: This script requires the following packages:
#               shiny (version 1.7.4)
#               shinyWidgets (version 0.7.6)
#               leaflet (version 2.1.1)
#               plotly (version 4.10.1)
# ** Note: Newer versions of the packages may not be compatible **
# =======================================================================================================================================

library(shiny)
library(shinyWidgets)
library(leaflet)
library(plotly)

# Main page with a navigation bar
navbarPage("TempoSpatial SNP Tracker", id="navbar", 
           # Interactive map tab (Navbar)
           tabPanel("Map", 
                    # Custom div class is used to style the tab using styles.css
                    div(class="outer",
                        # styles.css file is included 
                        tags$head(includeCSS("styles.css")),
                        
                        # The entire outer div is filled with the leaflet map output
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Floating/draggable panel is defined for controls/summary plots
                        absolutePanel(id="mainPanel", class="panel panel-default", fixed=TRUE, draggable=TRUE,
                                      top=70,  left=10, right="auto", bottom="auto",
                                      width=450, height="auto",
                                      
                                      # Panel is divided into tabs
                                      tabsetPanel(id="mainPanel_tabs", type="pills",
                                        
                                        # Tab for the map/data controls (Absolutepanel)
                                        tabPanel("Controls",
                                          fluidPage(
                                            
                                            # First row of controls (No. of SNPs/Plot type/Data type)
                                            fluidRow(
                                              column(width=4, 
                                                     radioButtons("num_SNP", "No. of SNPs", 
                                                                  choices = 1:2, inline=TRUE, selected=1),
                                                     style='padding-left:10px; padding-right:0px; padding-top:15px; padding-bottom:0px;left-margin:-15px;'),
                                              column(width=4, 
                                                     radioButtons("plot_type", "Plot type", 
                                                                  choices = c("Pie", "Bar"), inline=TRUE, selected="Pie"),
                                                     style='padding-left:0px; padding-right:0px; padding-top:15px; padding-bottom:0px;left-margin:-15px;'),
                                              column(width=4, 
                                                     radioButtons("data_type", "Data type", 
                                                                  choices = c("Count", "Freq"), inline=TRUE, selected="Count"),
                                                     style='padding-left:0px; padding-right:0px; padding-top:15px; padding-bottom:0px')
                                            ), # /fluid row
                                            
                                            # Second row of controls (CHR1 choice/SNP1 choice/alleles info)
                                            fluidRow(
                                              column(width=4, selectInput("CHR_choice1", "Chromosome", CHR_selection),
                                                     style='padding-left:10px; padding-right:1px; padding-top:0px; padding-bottom:0px'),
                                              column(width=5, selectizeInput("SNP_choice1", "SNP1", choices=NULL, selected=NULL),
                                                     style='padding-left:5px; padding-right:1px; padding-top:0px; padding-bottom:0px'),
                                              column(width=2, 
                                                     htmlOutput("SNP1_alleles"),
                                                     style='padding-left:6px; padding-right:17px; padding-top:23px; padding-bottom:10px')
                                            ), # /fluid row
                                            
                                            # Conditional panel is only shown when No. of SNPs selected is two
                                            conditionalPanel(
                                              condition="input.num_SNP==2",
                                              
                                              # Third row of controls (CHR2 choice/SNP2 choice/alleles info)
                                              fluidRow(
                                                column(width=4, selectInput("CHR_choice2", "Chromosome", CHR_selection),
                                                       style='padding-left:12px; padding-right:1px; padding-top:1px; padding-bottom:0px'),
                                                column(width=5, selectizeInput("SNP_choice2", "SNP2", choices=NULL, selected=NULL),
                                                       style='padding-left:2px; padding-right:1px; padding-top:1px; padding-bottom:0px'),
                                                column(width=2, 
                                                       htmlOutput("SNP2_alleles"),
                                                       style='padding-left:6px; padding-right:17px; padding-top:23px; padding-bottom:10px')
                                              ) # /fluid row
                                            ), # /conditional panel
                                            
                                            # Fourth row of controls (Show lables/Show missing data)
                                            fluidRow(
                                              column(width=4,
                                                     prettySwitch(inputId = "showlabel", label="Show labels", fill=TRUE)
                                              ),
                                              column(width=4,
                                                     prettySwitch(inputId = "showmissing", label="Show missing data", fill=TRUE, value=TRUE)
                                              ),
                                            ), # /fluid row
                                            
                                            # Fifth row of controls (Grouping variables)
                                            fluidRow(
                                              column(width=7,
                                                radioButtons("grouping_var", "Grouping variable", choices = c("None", "Country", "Distance"), 
                                                             inline=TRUE, selected="None"), 
                                                             style='padding-left:5px; padding-right:0px; padding-top:0px; padding-bottom:0px;'
                                              )
                                            ), # /fluid row
                                            
                                            # Conditional panel is only shown when grouping variable selected is "Distance"
                                            conditionalPanel(
                                              condition="input.grouping_var=='Distance'",
                                              
                                              # Sixth row of controls (Clustering distance)
                                              fluidRow(
                                                column(width=12,
                                                  sliderInput("group_dist_input", "Distance", 
                                                              width="100%", min=0, max=5000, step=1, value=100, ticks=TRUE, post=" km"),
                                                  style="margin-top:-10px;"
                                                )
                                              ) # /fluid row
                                            ), # /conditional panel
                                            
                                            # Seventh row of controls (Time step/Window size)
                                            fluidRow(
                                              column(width=6,
                                                sliderInput("timestep", label="Time step", 
                                                            width="100%", min=100, max=5000, value=time_step, step=100, ticks=TRUE, post=" years"),
                                                style="margin-top:-10px;"
                                              ),
                                              column(width=6,
                                                     sliderInput("windowsize", label="Window size", 
                                                                 width="100%", min=100, max=5000, value=time_step, step=100, ticks=TRUE, post=" years"),
                                                     style="margin-top:-10px;"
                                              )
                                            ), # /fluid row
                                            
                                            # Eigth row of controls (Timeline)
                                            fluidRow(
                                              column(width=12,
                                                sliderTextInput("timeline", label="Timeline", 
                                                                choices = seq(from=time_range[1], to=time_range[2], by=-time_step),
                                                                selected = time_range[1],
                                                                width="100%",
                                                                post=" BP",
                                                                animate=animationOptions(interval=2000, loop=TRUE)),
                                                style="margin-top:-10px;"
                                              )
                                            ), # /fluid row
                                            
                                            # Ninth row of controls (Palettes/Colors/Legend position)
                                            fluidRow(
                                              column(width=4, selectInput("palette_type", "Palettes", choices=c(names(color_palettes)), 
                                                                          selected = "Qualitative"),
                                                     style='padding-left:10px; padding-right:1px; padding-top:1px; padding-bottom:1px'),
                                              column(width=4, selectInput("palette_choice", "Colors", choices=color_palettes[["Qualitative"]], 
                                                                          selected="Dark2"),
                                                     style='padding-left:5px; padding-right:1px; padding-top:1px; padding-bottom:1px'),
                                              column(width=4, selectInput("legend", "Legend", choices=c("None", "TR", "TL", "BR", "BL"), 
                                                                          selected="TR"),
                                                     style='padding-left:5px; padding-right:10px; padding-top:1px; padding-bottom:1px')
                                            ), # /fluid row
                                            
                                            # Tenth row of controls (Opacity/Plot size)
                                            fluidRow(
                                              column(width=6, sliderInput("opacity", label="Opacity", width="100%",
                                                     min=0, max=100, value=100, step=1, post="%", ticks=TRUE)),
                                              column(width=6, sliderInput("plot_size", label="Size", width="100%",
                                                                          min=1, max=100, value=25, step=1, post="%", ticks=TRUE))
                                            ) # /fluid row
                                          ) # /fluid page
                                        ), # /controls tab panel
                                        
                                        # Tab for summary plots (Absolutepanel)
                                        tabPanel("Summary",
                                          
                                          # Summary plots for the worldwide population is drawn for SNP1 and SNP2
                                          fluidPage(
                                            h1("Overall summary"),
                                            fluidRow(
                                              column(width=12, plotlyOutput("total_summary_SNP1", height=200),
                                                     style='padding-left:5px; padding-right:5px; padding-top:5px; padding-bottom:1px')
                                            ), # /fluid row
                                            conditionalPanel(
                                              condition="input.num_SNP==2",
                                              fluidRow(
                                                column(width=12, plotlyOutput("total_summary_SNP2", height=200),
                                                       style='padding-left:5px; padding-right:5px; padding-top:10px; padding-bottom:1px')
                                              ) # /fluid row
                                            ), # /conditional panel
                                          ) # /fluid page
                                        ) # /summary tab panel
                                      ) # /tabset panel
                                      ) # /absolute panel
                        ) # /div outer
                    ), # /Map tab
           
           # Data explorer tab (Navbar)
           tabPanel("Data explorer",
                    fluidPage(
                      
                      # First row with controls
                      fluidRow(
                        column(width=2, selectInput("dataset", "Dataset", choices=sql_datasets), offset = 0,
                               style='padding-left:50px; padding-right:5px; padding-top:10px; padding-bottom:5px'),
                        column(width=2, selectInput("exploreData", "Data type", choices=c("Samples", "SNPs")), offset = 0,
                               style='padding-left:0px; padding-right:5px; padding-top:10px; padding-bottom:5px'),
                        
                        # Choice-specific controls are rendered from the server based on user input
                        column(width=8, uiOutput("exploreUI"), offset = 0,
                               style='padding-left:0px; padding-right:5px; padding-top:10px; padding-bottom:5px')
                      ), # /fluid row
                      
                      # Second row with the data table
                      fluidRow(
                        column(width=12, dataTableOutput("table1"))
                      ) # /fluid row
                    ) # /fluid page
                    ) # /Data explorer tab
           ) # /navbar page