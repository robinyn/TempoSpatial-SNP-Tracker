# =======================================================================================================================================
# Title: server.R
# Author: Euisuk (Robin) Han
# Date: 19/Mar/2023
# Description: This is the server-side script for the R-shiny application "TempoSpatial SNP Tracker"
# Dependencies: This script requires the following packages:
#               shiny (version 1.7.4)
#               tidyverse (version 1.3.2)
#               leaflet (version 2.1.1)
#               leaflet.minicharts (version 0.6.2)
#               RSQLite (version 2.3.0)
#               RColorBrewer (version 1.1-3)
#               plotly (version 4.10.1)
# ** Note: Newer versions of the packages may not be compatible **
# =======================================================================================================================================

library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(RSQLite)
library(RColorBrewer)
library(plotly)

# Set working directory to root folder
setwd(".")

# Main server function
server = function(input, output, session){
  # Server-wise global reactive variable for number of SNPs to track based on user input
  twoSNPs = reactive({
    if(input$num_SNP==1){
      FALSE
    }else{
      TRUE
    }
  })
  
  # Server-wise global reactive variable for distance threshold for clustering based on user input
  dist_threshold = reactive({
    as.numeric(input$group_dist_input)
  })
  
  # Reactive palette choice (Qual, Seq, Div) based on user input
  plot_palette = reactive({
    input$palette_choice
  })
  
  # Reactive plot type (Pie, Bar) based on user input
  plot_type = reactive({
  switch(input$plot_type,
         "Pie"=return("pie"),
         "Bar"=return("bar")
         )
  })
  
  # Reactive opacity based on user input
  opacity_val = reactive({
    as.numeric(input$opacity)/100
  })
  
  # Reactive plot_size based on user input
  plot_size = reactive({
    subset_dat = filteredData()
    num_rows = nrow(subset_dat)
    minimum_size = 10 # Minimum possible size
    maximum_size = 100 # Maximum possible size
    
    # The the user input for plot size is scaled with the min/max above
    multiplier = (maximum_size-minimum_size)/100 
    
    # Scaled plot size is returned
    return((minimum_size+(multiplier*as.numeric(input$plot_size))))
  })
  
  # Reactive variable to toggle a legend
  legend_show = reactive({
    if(input$legend=="None"){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  # Reactive variable for legend position
  legend_pos = reactive({
    switch(input$legend,
           "TR"="topright",
           "TL"="topleft",
           "BR"="bottomright",
           "BL"="bottomleft")
  })
  
  # Reactive variable to toggle labels on the map
  label_show = reactive({
    # If labels are toggled off, remove all labels from map
    if(!input$showlabel){
      leafletProxy("map") %>% 
        clearMarkers()
    }
    
    return(input$showlabel)
  })

  # Reactive variable for data type (Count/Freq)
  data_type = reactive({
    input$data_type
  })
  
  # Reactive variable for SNP1 REF/ALT alleles
  SNP1_alleles = reactive({
    
    # The ID for SNP1 is retrieved
    SNP1_choice = input$SNP_choice1
    
    # Relevant REF/ALT allele information is queried and fetched from SQL database
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT REF, ALT FROM SNP_meta WHERE SNP_ID='%s'", SNP1_choice))
    SNP1_alleles = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    # Numeric encoding for alleles/genotypes is converted to character
    SNP1_alleles[SNP1_alleles=="1" | SNP1_alleles==1]="A"
    SNP1_alleles[SNP1_alleles=="2" | SNP1_alleles==2]="C"
    SNP1_alleles[SNP1_alleles=="3" | SNP1_alleles==3]="G"
    SNP1_alleles[SNP1_alleles=="4" | SNP1_alleles==4]="T"
    
    return(SNP1_alleles)
  })
  
  # Reactive variable for SNP2 REF/ALT alleles
  SNP2_alleles = reactive({
    
    # The ID for SNP2 is retrieved
    SNP2_choice = input$SNP_choice2
    
    # Relevant REF/ALT allele information is queried and fetched from SQL database
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT REF, ALT FROM SNP_meta WHERE SNP_ID='%s'", SNP2_choice))
    SNP2_alleles = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    # Numeric encoding for alleles/genotypes is converted to character
    SNP2_alleles[SNP2_alleles=="1" | SNP2_alleles==1]="A"
    SNP2_alleles[SNP2_alleles=="2" | SNP2_alleles==2]="C"
    SNP2_alleles[SNP2_alleles=="3" | SNP2_alleles==3]="G"
    SNP2_alleles[SNP2_alleles=="4" | SNP2_alleles==4]="T"
    
    return(SNP2_alleles)
  })
  
  # Print REF/ALT allele information
  observe({
    output$SNP1_alleles=renderText(sprintf("REF: %s<br/>ALT: %s", SNP1_alleles()[1], SNP1_alleles()[2]))
  })
  
  observeEvent(input$SNP_choice2,{
    output$SNP2_alleles=renderText(sprintf("REF: %s<br/>ALT: %s", SNP2_alleles()[1], SNP2_alleles()[2]))
  })
  
  # Initial map render 
  output$map = renderLeaflet({
    leaflet(options=leafletOptions(zoomControl=FALSE)) %>% 
      addTiles(options=providerTileOptions(minZoom=2)) %>% 
      setView(40, 30, zoom = 3)
  })
  
  # Initial summary plot render
  output$total_summary = renderPlotly({
    fig_total = plot_ly(type="scatter", mode="lines") %>% 
      layout(title="Total", xaxis=list(range=c(time_range[0],time_range[1])))
  })
  
  # Observer for CHR1 choice - this function will detect changes in CHR1 choice
  observeEvent(input$CHR_choice1, {
    
    # Chromosome number selected by user is retrieved 
    CHR_ID = input$CHR_choice1
    
    # Appropriate SNP data for the selected chromosome is queried and fetched from SQL database
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    # SNP1 Choice selection input control is updated with retrieved SNP data
    updateSelectizeInput(session,"SNP_choice1",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  # Observer for CHR2 choice - this function will detect changes in CHR2 choice
  observeEvent(input$CHR_choice2, {
    
    # Chromosome number selected by user is retrieved 
    CHR_ID = input$CHR_choice2
    
    # Appropriate SNP data for the selected chromosome is queried and fetched from SQL database
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    # SNP2 Choice selection input control is updated with retrieved SNP data
    updateSelectizeInput(session,"SNP_choice2",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  # Observer for timestep change
  observeEvent(input$timestep, {
    
    # If timestep is changed by the user, the timeline is updated to reflect the new timestep
    # Global variable time_step is also modified 
    time_step=input$timestep
    updateSliderTextInput(session, inputId="timeline", 
                          choices=seq(from=time_range[1], to=time_range[2], by=-time_step))
  })
  
  # Observer for palette type change
  observeEvent(input$palette_type, {
    
    # Update color scheme choices based on palette choice
    palette_type_choice = input$palette_type
    updateSelectInput(session, "palette_choice", choices=color_palettes[[palette_type_choice]])
  }, ignoreInit=TRUE)
  
  # Reactive cluster_table - All samples are clustered based on user input and assigned a cluster number
  cluster_table = reactive({
    # MasterID, Lat, Long, Country info for all samples are retrieved from the SQL database
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    query = sprintf("SELECT MasterID, Long, Lat, Country FROM sample_meta")
    res = dbSendQuery(db, query)
    cluster_table = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    # The samples are clustered based on user input
    switch(input$grouping_var,
           # If no grouping variable is selected, samples are grouped by their lat/long coordinates
           # samples with identical coordinates will be clustered together
           "None"={
             cluster_table = cluster_table %>% 
               mutate(Lat = as.numeric(Lat), Long = as.numeric(Long)) %>% 
               group_by(Lat, Long) %>% 
               mutate(Cluster = cur_group_id()) %>% 
               mutate(Country = NA)
           },
           # Samples are clustered by the country where the sample is from
           "Country"={
             cluster_table = cluster_table %>% 
               mutate(Lat = as.numeric(Lat), Long = as.numeric(Long)) %>% 
               group_by(Country) %>% 
               mutate(Cluster = cur_group_id()) %>% 
               select(c(MasterID, Country, Cluster)) %>% 
               merge(country_list, by="Country")
           },
           # Samples are clustered by the distance threshold set by the user
           "Distance"={
             # Clustering function to group samples through hierarchical clustering
             cluster_table = cluster_Samples(cluster_table)
             # Centroid coordinates for the clusters are calculated for plotting
             centroids = calc_Centroid(cluster_table)
             
             cluster_table = cluster_table %>%
               select(c(MasterID, Country, Cluster)) %>% 
               merge(centroids, by="Cluster") %>% 
               mutate(Country = NA)
           })
    
    return(cluster_table)
  })
  
  # Reactive dataframe with Lat/Long coordinates for each cluster
  plot_group = reactive({
    cluster_table() %>% 
    select(Lat, Long, Cluster, Country) %>% 
    distinct()
  })
  
  # Filter data reactive to timeline change
  filteredData = reactive({
    
    # Set start/end times based on timeline/windowsize input
    start_time = input$timeline
    end_time = input$timeline-input$windowsize
    
    # Filter samples based on start/end times
    plot_dat = filter_samples(start_time, end_time)
    
    # If "Show missing data" option is toggled off, remove missing data
    if(!input$showmissing){
      plot_dat = plot_dat %>% 
        select(-any_of(c("SNP1_Missing", "SNP2_Missing")))
    }
    
    # If after removal, there is no data left to show, return NULL
    if(!any(grepl("SNP1|SNP2", colnames(plot_dat)))){
      return(NULL)
    }
    
    # If user choses Freq data type, convert data type
    if(data_type()=="Freq" & input$showmissing){
      plot_dat = allele_freq(plot_dat)
    }else if(data_type()=="Freq" & !input$showmissing){
      plot_dat = allele_freq_no_missing(plot_dat)
    }
    
    return(plot_dat)
  })
  
  # MAIN PLOT FUNCTION - Plot charts on the map based on filtered data
  observe({
    # Retrieve filtered data
    subset_dat = filteredData()
    
    # If filtered data is NULL, clear map and return
    if(is.null(subset_dat)){
      leafletProxy("map") %>%
        clearMinicharts()
      return()
    }
    if(nrow(subset_dat)==0){
      leafletProxy("map") %>%
        clearMinicharts()
      return()
    }
    
    # Print labels based on toggle/cluster options
    if(label_show()){
      switch(input$grouping_var,
             # If no clustering variable is chosen, the Lat/Long coordinates of each "group" will be shown as the labels
             "None"={
               leafletProxy("map") %>% 
                 clearMarkers() %>% 
                 addLabelOnlyMarkers(lng = as.numeric(subset_dat$Long), lat = as.numeric(subset_dat$Lat), 
                                     label = sprintf("%.4f, %.4f", as.numeric(subset_dat$Long), as.numeric(subset_dat$Lat)),
                                     labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                                 direction = "bottom", offset = c(0,5)))
             },
             # If country is chosen, names of the countries will be shown
             "Country"={
               leafletProxy("map") %>% 
                 clearMarkers() %>% 
                 addLabelOnlyMarkers(lng = as.numeric(subset_dat$Long), lat = as.numeric(subset_dat$Lat), label = subset_dat$Country,
                                     labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                                 direction = "bottom", offset = c(0,5)))
             },
             # If distance is chosen, cluster numbers will be shown
             "Distance"={
               leafletProxy("map") %>% 
                 clearMarkers() %>% 
                 addLabelOnlyMarkers(lng = as.numeric(subset_dat$Long), lat = as.numeric(subset_dat$Lat), label = paste("Cluster",subset_dat$Cluster),
                                     labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                                 direction = "bottom", offset = c(0,5)))
             })
    }
    
    # Update map with charts using a proxy to avoid re-rendering the entire map every time
    leafletProxy("map") %>% 
      clearMinicharts() %>% 
      addMinicharts(
        lng = subset_dat$Long, lat=subset_dat$Lat,
        type = plot_type(),
        chartdata = subset_dat[, !(colnames(subset_dat) %in% disregard)],
        colorPalette = brewer.pal(n=9, plot_palette()),
        fillColor = brewer.pal(n=3, plot_palette())[3],
        legend = legend_show(),
        legendPosition = legend_pos(),
        opacity = opacity_val(),
        width = plot_size(),
        height = plot_size()
      )
    
    # Add legend manually if there is only one category in the data
    if(length(c(colnames(subset_dat)[!colnames(subset_dat) %in% disregard]))==1 && legend_show()){
      leafletProxy("map") %>% 
        addLegend(legend_pos(),labels=c(colnames(subset_dat)[!colnames(subset_dat) %in% disregard]),
                  colors = brewer.pal(n=3, plot_palette())[3], layerId="minichartsLegend")
      
    }
  })
  
  # Summary plot generation function - observe change in tabs on absolutepanel
  observeEvent(input$mainPanel_tabs, {
    
    # Return NULL if the "Controls" tab is selected
    if(input$mainPanel_tabs=="Controls"){
      return(NULL)
    }
    
    # Reset old plots while new plot is being rendered
    output$total_summary_SNP1 = renderPlotly({})
    output$total_summary_SNP2 = renderPlotly({})
    
    # Retrieve window size from user input
    window_size = as.numeric(input$windowsize)
    
    # Create a dataframe to be used as the framework for storing plot data based on data type and number of SNPs 
    switch(input$data_type,
           "Count"={
             if(twoSNPs()){
               SNP1_ref = sprintf("%s%s", SNP1_alleles()$REF, SNP1_alleles()$REF)
               SNP1_alt = sprintf("%s%s", SNP1_alleles()$ALT, SNP1_alleles()$ALT)
               SNP1_het = sprintf("%s%s", SNP1_alleles()$REF, SNP1_alleles()$ALT)
               SNP2_ref = sprintf("%s%s", SNP2_alleles()$REF, SNP2_alleles()$REF)
               SNP2_alt = sprintf("%s%s", SNP2_alleles()$ALT, SNP2_alleles()$ALT)
               SNP2_het = sprintf("%s%s", SNP2_alleles()$REF, SNP2_alleles()$ALT)
               
               figure_table = data.frame(SNP=c(rep("SNP1", 4), rep("SNP2", 4)), 
                                         Type=c(SNP1_ref, SNP1_het, SNP1_alt, "Missing",
                                                SNP2_ref, SNP2_het, SNP2_alt, "Missing"))
             }else{
               SNP1_ref = sprintf("%s%s", SNP1_alleles()$REF, SNP1_alleles()$REF)
               SNP1_alt = sprintf("%s%s", SNP1_alleles()$ALT, SNP1_alleles()$ALT)
               SNP1_het = sprintf("%s%s", SNP1_alleles()$REF, SNP1_alleles()$ALT)
               
               figure_table = data.frame(SNP=c(rep("SNP1", 4)), 
                                         Type=c(SNP1_ref, SNP1_het, SNP1_alt, "Missing"))
             }
           },
           "Freq"={
             
             if(twoSNPs()){
               figure_table = data.frame(SNP=c(rep("SNP1", 3), rep("SNP2", 3)), 
                                         Type=c(SNP1_alleles()$REF, SNP1_alleles()$ALT, "Missing",
                                                SNP2_alleles()$REF, SNP2_alleles()$ALT, "Missing"))
             }else{
               figure_table = data.frame(SNP=rep("SNP1", 3), 
                                         Type=c(SNP1_alleles()$REF, SNP1_alleles()$ALT, "Missing"))
             }
           })
    
    # Define a new dataframe to append plot data to
    final_df = data.frame()
    
    # Create a sequence of "start" times based on the date range and time step
    range_vec = seq(from=time_range[1], to=time_range[2], by=-time_step)
    
    # Create a progress bar since data processing takes a little bit of time 
    withProgress(message = "Generating plot", value=0, min=0, max=length(range_vec),{
      # For loop to loop through all of the time steps 
      for(start_time in range_vec){
        # Increment progress bar by 1
        incProgress(1)
        
        # Define "end" time based on start time and window size
        end_time = start_time - window_size
        
        # Query/Fetch relevant samples/SNPs from SQL database based on start/end times
        subset_dat = filter_samples(start_time, end_time)
        
        # Reformat data and summarize by sumation of all clusters (Currently only global summary is possible)
        subset_dat = subset_dat %>%
          pivot_longer(-c(Cluster, Country, Lat, Long), names_to = c("SNP", "Type"), names_sep = "_", values_to = "Count") %>%
          group_by(SNP, Type) %>%
          summarise(n=sum(Count))
        
        # If showing missing data is toggled off, remove missing data/remove "missing" category from the data structure
        if(!input$showmissing){
          subset_dat = subset_dat %>% 
            filter(Type!="Missing")
        }
        if(!input$showmissing){
          figure_table = figure_table %>% 
            filter(Type!="Missing")
        }
        
        # If there is no data left after removal, move to the next time step
        if(nrow(subset_dat)==0){
          next
        }
        
        # If user chooses Freq data type, convert data type
        subset_dat = subset_dat %>% 
          pivot_wider(names_from = c(SNP, Type), values_from = n)
        
        if(data_type()=="Freq" & input$showmissing){
          subset_dat = allele_freq(subset_dat)
        }else if(data_type()=="Freq" & !input$showmissing){
          subset_dat = allele_freq_no_missing(subset_dat)
        }
        
        subset_dat = subset_dat %>%
          pivot_longer(everything(), names_to = c("SNP", "Type"), names_sep = "_", values_to = "n")
        
        # Merge the data to the data structure and add a column for time 
        temp_table = figure_table %>%
          merge(subset_dat, by=c("SNP", "Type"), all=TRUE) %>% 
          mutate(Time=start_time)
        
        # All NA values created during the merge step is changed to 0        
        temp_table[is.na(temp_table)]=0
        
        # Append rows to the final dataframe 
        final_df = rbind(final_df, temp_table)
      } # For loop
    }) # With progress
    
    # Split final_df into SNP1 and SNP2 final for plotting
    SNP1_final = final_df[final_df$SNP == "SNP1", ]
    
    # Render plotly scatter plot based on final_df
    output$total_summary_SNP1 = renderPlotly({
      plot_ly( x=SNP1_final$Time, y=SNP1_final$n, color=as.factor(SNP1_final$Type), type="scatter", mode="lines") %>%
      layout(title="SNP1",
             xaxis=list(title="Year (BP)", autorange="reversed", dtick=2500, tickmode="linear", tickformat="digit"),
             yaxis=list(title=input$data_type),
             hovermode="x unified")
    })
    
    # Split final_df into SNP1 and SNP2 final for plotting
    if(twoSNPs()){
      SNP2_final = final_df[final_df$SNP == "SNP2", ]
      
      # Render plotly scatter plot based on final_df
      output$total_summary_SNP2 = renderPlotly({
        plot_ly( x=SNP2_final$Time, y=SNP2_final$n, color=as.factor(SNP2_final$Type), type="scatter", mode="lines") %>%
          layout(title="SNP2",
                 xaxis=list(title="Year (BP)", autorange="reversed", dtick=2500, tickmode="linear", tickformat="digit"),
                 yaxis=list(title=input$data_type),
                 hovermode="x unified")
      })
    }
    
  }, ignoreInit = TRUE)
  
  # No samples to plot -> return a dataframe with all of the SNPs as missing
  no_samples = function(){
    if(twoSNPs()){
      plot_dat = plot_group() %>% 
        mutate(SNP1 = "Missing") %>% 
        mutate(SNP2 = "Missing")
        
    }else{
      plot_dat = plot_group() %>% 
        mutate(SNP1 = "Missing")
    }
    
    return(plot_dat)
  }
  
  # Main function for querying/processing sample data from sample_meta
  # This function is used to identify samples that appear within a given time window
  filter_samples = function(start_time, end_time){
    # Query and identify samples within given time window from SQL database
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    query = sprintf("SELECT MasterID, Long, Lat, Country FROM sample_meta WHERE CAST(DateMean AS INTEGER) <= %s AND CAST(DateMean AS INTEGER) >=%s", start_time, end_time)
    res = dbSendQuery(db, query)
    samples_to_plot = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    # Query and retrieve appropriate SNP data with the identified samples 
    if(nrow(samples_to_plot)==0){
      plot_dat = no_samples()
    }else{
      plot_dat = query_main(samples_to_plot) 
    }
    
    # Pivot data to be wider - This is the format the leaflet.minicharts packages needs to plot 
    plot_dat = plot_dat %>% 
      pivot_longer(-c(Cluster, Country, Lat, Long)) %>% 
      count(Cluster, Country, Lat, Long, name, value) %>% 
      pivot_wider(names_from = c(name, value), values_from = n) %>%
      replace(is.na(.), 0) %>% 
      relocate(sort(names(.)))
    
    return(plot_dat)
  }

  # Main function for querying/processing SNP data from main
  # This function is used to retrieve the actual allele/genotype data for given SNP IDs and samples
  query_main = function(samples_to_plot){
    
    # Query and retrieve appropriate SNP data with the identified samples
    # The retrieved data is reformatted and merged with cluster information
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    sample_list = paste0(sprintf("`%s`", samples_to_plot$MasterID), collapse=",")
    
    if(twoSNPs()){
      query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s' OR SNP_ID='%s'", sample_list, input$SNP_choice1, input$SNP_choice2)
      res = dbSendQuery(db, query)
      SNP_dat = dbFetch(res)
      dbClearResult(res)
      
      SNP1 = SNP_dat[1,]
      SNP2 = SNP_dat[2,] 
      
      if(input$SNP_choice1==input$SNP_choice2){
        SNP2 = SNP1
      }
      
      if(ncol(SNP_dat)==1){
        SNP1 = c(noquote(colnames(SNP_dat))) %>% 
          cbind(SNP1) %>% 
          as.data.frame() %>% 
          rename("MasterID"=".")
        
        SNP2 = c(noquote(colnames(SNP_dat))) %>% 
          cbind(SNP2) %>% 
          as.data.frame() %>% 
          rename("MasterID"=".")
      }else{
        SNP1 = SNP1 %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP1")
        SNP2 = SNP2 %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP2")
      }
      
      plot_dat = merge(samples_to_plot, SNP1, by="MasterID", all=TRUE) %>% 
        merge(SNP2, by="MasterID", all=TRUE)
      
      plot_dat[is.na(plot_dat)]="00"
      
      plot_dat = plot_dat %>% 
        mutate(SNP1 = replace(SNP1, SNP1=="00", "Missing"), SNP2 = replace(SNP2, SNP2=="00", "Missing")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "1", "A"), SNP2 = str_replace_all(SNP2, "1", "A")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "2", "C"), SNP2 = str_replace_all(SNP2, "2", "C")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "3", "G"), SNP2 = str_replace_all(SNP2, "3", "G")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "4", "T"), SNP2 = str_replace_all(SNP2, "4", "T")) %>% 
        select(-c(Long, Lat, Country)) %>% 
        merge(cluster_table(), by="MasterID") %>% 
        select(Cluster, SNP1, SNP2)
      
    }else{
      query = sprintf("SELECT %s FROM main WHERE SNP_ID='%s'", sample_list, input$SNP_choice1)
      res = dbSendQuery(db, query)
      SNP_dat = dbFetch(res)
      dbClearResult(res) 
      
      SNP_dat = SNP_dat %>% pivot_longer(everything(), names_to = "MasterID", values_to = "SNP1")
      plot_dat = merge(samples_to_plot, SNP_dat, by="MasterID", all=TRUE)
      plot_dat[is.na(plot_dat)]="00"
      
      plot_dat = plot_dat %>% 
        mutate(SNP1 = replace(SNP1, SNP1=="00", "Missing")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "1", "A")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "2", "C")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "3", "G")) %>% 
        mutate(SNP1 = str_replace_all(SNP1, "4", "T")) %>% 
        select(-c(Long, Lat, Country)) %>% 
        merge(cluster_table(), by="MasterID") %>% 
        select(Cluster, SNP1)
    }
    
    plot_dat = plot_group() %>% 
      merge(plot_dat, by="Cluster", all=TRUE)
    plot_dat[is.na(plot_dat)]="Missing"
    
    dbDisconnect(db)
    return(plot_dat)
  }
  
  # Allele frequency calculations
  
  # ** Note ** allele_freq(dat) and allele_freq_no_missing(dat) are almost identical, but they could not be merged into a
  # single function without major restructuring/rewriting of the code. Possible optimization for future versions. 
  allele_freq_no_missing = function(dat){
    uniqchars <- function(x) unique(unlist(strsplit(x, "")))
    switchchr <- function(x) paste(unlist(strsplit(x,""))[2],unlist(strsplit(x,""))[1], sep="")
    SNP1_alleles = c()
    SNP2_alleles = c()
    
    # Identify different genotypes present in the data
    col_names_list = colnames(dat)[!colnames(dat) %in% disregard]
    alleles = str_split(col_names_list, "_", simplify = TRUE) %>% data.frame()
    colnames(alleles) = c("SNP", "Genotype")
    
    # Disregard "Missing" types for now - there shouldn't be any but just in case
    alleles = alleles[!alleles$Genotype=="Missing",]
    
    if(twoSNPs()){
      # Identify all alleles for SNP1 and SNP2 present in the data
      SNP1_alleles = uniqchars(alleles$Genotype[alleles$SNP=="SNP1"])
      SNP2_alleles = uniqchars(alleles$Genotype[alleles$SNP=="SNP2"])
      
      # Calculate total number of individuals for SNP1 and SNP2
      dat = dat %>% 
        ungroup() %>% 
        mutate(SNP1_total=rowSums(select(.,contains("SNP1")))) %>% 
        mutate(SNP2_total=rowSums(select(.,contains("SNP2"))))
      
      # Separate into SNP1 and SNP2
      SNP1_tab = dat %>% 
        select(contains("SNP1"))
      
      SNP2_tab = dat %>% 
        select(contains("SNP2"))
      
      # Calculate allele frequencies for SNP1
      # If there is no data, do nothing, otherwise, calculate allele frequencies based on total number of individuals
      # Ex. f(A) = (f(AA) + f(AB | BA)/2 + f(BB)) / total number of individuals
      if(length(SNP1_alleles)==0){
        
      }else if(length(SNP1_alleles)==1){
        allele_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="")
        col_name = paste("SNP1_", SNP1_alleles[1], sep="")
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name}}:=eval(parse(text={{allele_name}}))/SNP1_total)
      }else{
        allele1_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="") 
        allele2_name = paste("SNP1_", SNP1_alleles[2], SNP1_alleles[2], sep="") 
        col_name1 = paste("SNP1_", SNP1_alleles[1], sep="") 
        col_name2 = paste("SNP1_", SNP1_alleles[2], sep="") 
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name1}}:=(eval(parse(text={{allele1_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP1_total) %>% 
          mutate({{col_name2}}:=(eval(parse(text={{allele2_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP1_total)
      }
      
      # Calculate allele frequencies for SNP2
      if(length(SNP2_alleles)==0){
        
      }else if(length(SNP2_alleles)==1){
        allele_name = paste("SNP2_", SNP2_alleles[1], SNP2_alleles[1], sep="")
        col_name = paste("SNP2_", SNP2_alleles[1], sep="")
        
        SNP2_tab = SNP2_tab %>% 
          mutate({{col_name}}:=eval(parse(text={{allele_name}}))/SNP2_total)
      }else{
        allele1_name = paste("SNP2_", SNP2_alleles[1], SNP2_alleles[1], sep="") 
        allele2_name = paste("SNP2_", SNP2_alleles[2], SNP2_alleles[2], sep="") 
        col_name1 = paste("SNP2_", SNP2_alleles[1], sep="") 
        col_name2 = paste("SNP2_", SNP2_alleles[2], sep="") 
        
        SNP2_tab = SNP2_tab %>% 
          mutate({{col_name1}}:=(eval(parse(text={{allele1_name}}))+((SNP2_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP2_total) %>% 
          mutate({{col_name2}}:=(eval(parse(text={{allele2_name}}))+((SNP2_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP2_total)
      }
      
      # Reformat and add frequency data to data table for plotting
      SNP1_tab = SNP1_tab %>% 
        mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))
      SNP2_tab = SNP2_tab %>% 
        mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))
      
      dat = dat %>% 
        select(any_of(c("Cluster", "Country", "Lat", "Long"))) %>%
        cbind(SNP1_tab) %>% 
        cbind(SNP2_tab) %>% 
        select(matches("Country|Cluster|Lat|Long|MasterID|^SNP[12]_[ACTG]$|^SNP[12]_Missing$"))
      
    }else{
      # Do the same as above, just for one SNP 
      SNP1_alleles = uniqchars(alleles$Genotype[alleles$SNP=="SNP1"])
      
      dat = dat %>% 
        ungroup() %>% 
        mutate(SNP1_total=rowSums(select(.,contains("SNP1"))))
      SNP1_tab = dat %>% 
        select(contains("SNP1"))
      
      if(length(SNP1_alleles)==0){
        
      }else if(length(SNP1_alleles)==1){
        allele_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="")
        col_name = paste("SNP1_", SNP1_alleles[1], sep="")
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name}}:=eval(parse(text={{allele_name}}))/SNP1_total)
        
      }else{
        allele1_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="") 
        allele2_name = paste("SNP1_", SNP1_alleles[2], SNP1_alleles[2], sep="") 
        col_name1 = paste("SNP1_", SNP1_alleles[1], sep="") 
        col_name2 = paste("SNP1_", SNP1_alleles[2], sep="") 
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name1}}:=(eval(parse(text={{allele1_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP1_total) %>% 
          mutate({{col_name2}}:=(eval(parse(text={{allele2_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})))/2))/SNP1_total)
      }
      
      SNP1_tab = SNP1_tab %>% 
        mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))
      
      dat = dat %>% 
        select(any_of(c("Cluster", "Country", "Lat", "Long"))) %>% 
        cbind(SNP1_tab) %>% 
        select(matches("Country|Cluster|Lat|Long|MasterID|^SNP[12]_[ACTG]$|^SNP[12]_Missing$"))
      
    }
    
    # Return the data table for plotting
    return(dat)
  }
  
  # This function is almost identical to allele_freq_no_missing(dat) except this function will calculate frequencies
  # while accounting for missing data
  allele_freq = function(dat){
    uniqchars <- function(x) unique(unlist(strsplit(x, "")))
    switchchr <- function(x) paste(unlist(strsplit(x,""))[2],unlist(strsplit(x,""))[1], sep="")
    SNP1_alleles = c()
    SNP2_alleles = c()
    
    col_names_list = colnames(dat)[!colnames(dat) %in% disregard]
    alleles = str_split(col_names_list, "_", simplify = TRUE) %>% data.frame()
    colnames(alleles) = c("SNP", "Genotype")
    
    
    alleles = alleles[!alleles$Genotype=="Missing",]
    
    if(twoSNPs()){
      SNP1_alleles = uniqchars(alleles$Genotype[alleles$SNP=="SNP1"])
      SNP2_alleles = uniqchars(alleles$Genotype[alleles$SNP=="SNP2"])
      
      print("1")
      print(dat)
      
      dat = dat %>% 
        ungroup() %>% 
        mutate(SNP1_total=rowSums(select(.,contains("SNP1")))) %>% 
        mutate(SNP2_total=rowSums(select(.,contains("SNP2"))))
      
      print("2")
      print(dat)
      
      SNP1_tab = dat %>% 
        select(contains("SNP1"))
      
      SNP2_tab = dat %>% 
        select(contains("SNP2"))
      
      print("3")
      print(SNP1_tab)
      
      if(length(SNP1_alleles)==0){
        SNP1_tab = SNP1_tab %>% 
          mutate(SNP1_Missing=SNP1_Missing/SNP1_total)
      }else if(length(SNP1_alleles)==1){
        allele_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="")
        col_name = paste("SNP1_", SNP1_alleles[1], sep="")
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name}}:=eval(parse(text={{allele_name}}))/SNP1_total) %>% 
          mutate(SNP1_Missing=SNP1_Missing/SNP1_total)
      }else{
        allele1_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="") 
        allele2_name = paste("SNP1_", SNP1_alleles[2], SNP1_alleles[2], sep="") 
        col_name1 = paste("SNP1_", SNP1_alleles[1], sep="") 
        col_name2 = paste("SNP1_", SNP1_alleles[2], sep="") 
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name1}}:=(eval(parse(text={{allele1_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})) - SNP1_Missing)/2))/SNP1_total) %>% 
          mutate({{col_name2}}:=(eval(parse(text={{allele2_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})) - SNP1_Missing)/2))/SNP1_total) %>% 
          mutate(SNP1_Missing=SNP1_Missing/SNP1_total)
      }
      
      if(length(SNP2_alleles)==0){
        SNP2_tab = SNP2_tab %>% 
          mutate(SNP2_Missing=SNP2_Missing/SNP2_total)
      }else if(length(SNP2_alleles)==1){
        allele_name = paste("SNP2_", SNP2_alleles[1], SNP2_alleles[1], sep="")
        col_name = paste("SNP2_", SNP2_alleles[1], sep="")
        
        SNP2_tab = SNP2_tab %>% 
          mutate({{col_name}}:=eval(parse(text={{allele_name}}))/SNP2_total) %>% 
          mutate(SNP2_Missing=SNP2_Missing/SNP2_total)
      }else{
        allele1_name = paste("SNP2_", SNP2_alleles[1], SNP2_alleles[1], sep="") 
        allele2_name = paste("SNP2_", SNP2_alleles[2], SNP2_alleles[2], sep="") 
        col_name1 = paste("SNP2_", SNP2_alleles[1], sep="") 
        col_name2 = paste("SNP2_", SNP2_alleles[2], sep="") 
        
        SNP2_tab = SNP2_tab %>% 
          mutate({{col_name1}}:=(eval(parse(text={{allele1_name}}))+((SNP2_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})) - SNP2_Missing)/2))/SNP2_total) %>% 
          mutate({{col_name2}}:=(eval(parse(text={{allele2_name}}))+((SNP2_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})) - SNP2_Missing)/2))/SNP2_total) %>% 
          mutate(SNP2_Missing=SNP2_Missing/SNP2_total)
      }
      
      SNP1_tab = SNP1_tab %>% 
        mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))
      SNP2_tab = SNP2_tab %>% 
        mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))
      
      dat = dat %>% 
        select(any_of(c("Cluster", "Country", "Lat", "Long"))) %>%
        cbind(SNP1_tab) %>% 
        cbind(SNP2_tab) %>% 
        select(matches("Country|Cluster|Lat|Long|MasterID|^SNP[12]_[ACTG]$|^SNP[12]_Missing$"))
      
    }else{
      SNP1_alleles = uniqchars(alleles$Genotype[alleles$SNP=="SNP1"])
      
      dat = dat %>% 
        ungroup() %>% 
        mutate(SNP1_total=rowSums(select(.,contains("SNP1"))))
      SNP1_tab = dat %>% 
        select(contains("SNP1"))
      
      if(length(SNP1_alleles)==0){
        SNP1_tab = SNP1_tab %>% 
          mutate(SNP1_Missing=SNP1_Missing/SNP1_total)
      }else if(length(SNP1_alleles)==1){
        allele_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="")
        col_name = paste("SNP1_", SNP1_alleles[1], sep="")
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name}}:=eval(parse(text={{allele_name}}))/SNP1_total) %>% 
          mutate(SNP1_Missing=SNP1_Missing/SNP1_total)
        
      }else{
        allele1_name = paste("SNP1_", SNP1_alleles[1], SNP1_alleles[1], sep="") 
        allele2_name = paste("SNP1_", SNP1_alleles[2], SNP1_alleles[2], sep="") 
        col_name1 = paste("SNP1_", SNP1_alleles[1], sep="") 
        col_name2 = paste("SNP1_", SNP1_alleles[2], sep="") 
        
        SNP1_tab = SNP1_tab %>% 
          mutate({{col_name1}}:=(eval(parse(text={{allele1_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})) - SNP1_Missing)/2))/SNP1_total) %>% 
          mutate({{col_name2}}:=(eval(parse(text={{allele2_name}}))+((SNP1_total - eval(parse(text={{allele1_name}})) - eval(parse(text={{allele2_name}})) - SNP1_Missing)/2))/SNP1_total) %>% 
          mutate(SNP1_Missing=SNP1_Missing/SNP1_total)
      }
      
      SNP1_tab = SNP1_tab %>% 
        mutate(across(everything(), ~replace(.x, is.nan(.x), 0)))
      
      dat = dat %>% 
        select(any_of(c("Cluster", "Country", "Lat", "Long"))) %>% 
        cbind(SNP1_tab) %>% 
        select(matches("Country|Cluster|Lat|Long|MasterID|^SNP[12]_[ACTG]$|^SNP[12]_Missing$"))
      
    }
    
    return(dat)
  }
  
  # Functions for distance based clustering

  # Create distance matrix from distM - which has been retrieved from the SQL database in global.R
  dist_Matrix = function(dat){
    # A symmetrical distance matrix is created from all of the given SampleIDs from the input dataframe
    # The distances between each samples are already calculated and can be retrieved from distM
    # The distances were calculated using the haversine formula with a separate script
    
    sample_dist= dat %>% 
      expand(MasterID, MasterID) %>% 
      rename("ID1"="MasterID...1", "ID2"="MasterID...2") %>% 
      left_join(distM, by=c("ID1"="id1", "ID2"="id2")) %>% 
      pivot_wider(everything(), names_from =ID2, values_from = dist) %>% 
      column_to_rownames(var ="ID1")
    
    rnames = rownames(sample_dist)
    
    sample_dist %>% 
      sapply(as.numeric) %>% 
      data.matrix
    
    rownames(sample_dist) = rnames
    
    return(sample_dist)
  }
  
  # Hierarchical clustering based on distance threshold
  cluster_Samples = function(dat){
    # This function clusters samples based on the distance threshold set by the user. 
    # The hierarchichal clustering function takes the distance matrix as input and will produce a cluster tree
    # This tree can then be used to infer the different clusters for each samples
    if(nrow(dat)>1){
      distances = dist_Matrix(dat)
      cluster_tree = hclust(as.dist(distances))
      clusters = cutree(cluster_tree, h=dist_threshold()*1000)
      
      clusters = clusters %>% 
        as.list() %>% 
        data.frame() %>% 
        pivot_longer(everything(), names_to = "MasterID", values_to = "Cluster") %>% 
        group_by(Cluster) 
      
      dat = dat %>% 
        merge(clusters, by="MasterID")
      
    }else{
      dat$Cluster = 1
    }
    
    return(dat)
  }
  
  # Calculate centroid coordinates of clusters
  # This function calculates simple geometric means for the Lat/Long coordinates of each samples in each cluster to
  # calculate the centroid coordinates for each clusters. 
  calc_Centroid = function(dat){
    # Reformat data
    cluster_groups = dat %>% 
      group_by(Cluster, Lat, Long) %>% 
      summarise() %>% 
      sapply(as.numeric) %>% 
      data.frame()
    
    # Create a new dataframe to store the centroid coordinates
    cluster_centroids = data.frame(matrix(NA, nrow=length(unique(cluster_groups$Cluster)),ncol=3))
    colnames(cluster_centroids) = c("Cluster", "Lat", "Long")
    
    # Add cluster numbers into the new dataframe
    cluster_centroids$Cluster = unique(cluster_groups$Cluster)
    
    # Run a for loop to iterate through each unique cluster number
    for(cluster in unique(cluster_groups$Cluster)){
      # Calculate mean lat/long per cluster and store into dataframe
      lat_c = sum(cluster_groups$Lat[cluster_groups$Cluster==cluster])/length(cluster_groups$Lat[cluster_groups$Cluster==cluster])
      long_c = sum(cluster_groups$Long[cluster_groups$Cluster==cluster])/length(cluster_groups$Long[cluster_groups$Cluster==cluster])
      
      cluster_centroids$Lat[cluster_centroids$Cluster==cluster] = lat_c
      cluster_centroids$Long[cluster_centroids$Cluster==cluster] = long_c
    }
    
    return(cluster_centroids)
  } 
  
  # =======================================================================================================================================
  # Data explorer tab
  # =======================================================================================================================================
  
  # Reactive variable for name of dataset to use
  exploreDatabase = reactive({
    input$dataset
  })
  
  # Render UI reactive to user input
  output$exploreUI = renderUI({
    
    switch(input$exploreData,
           # If the user chooses "Samples", display "Countries" and "Time range" options 
           "Samples"={
             db = dbConnect(SQLite(), "data/reich_v50.sqlite")
             query = "SELECT DISTINCT Country FROM sample_meta"
             res = dbSendQuery(db, query)
             country_choices = dbFetch(res)
             dbClearResult(res)
             dbDisconnect(db)
             
             
             country_choices = country_choices[order(country_choices$Country),]
             
             fluidRow(
               column(width=3, selectInput("exploreCountry", "Country", choices=country_choices, selected="Austria", multiple = TRUE)),
               column(width=3, sliderTextInput("exploreRange", "Time range", 
                                               choices=seq(from=time_range[1], to=time_range[2], by=-1),
                                               selected = c(time_range[1], time_range[2]),
                                               width="100%",
                                               post=" BP")),
               column(width=6, checkboxGroupInput("exploreColumns", "Columns", choices=NULL, inline=TRUE))
             )
           },
           # If the user chooses "SNPs", display "Chromosome" options
           "SNPs"={
             db = dbConnect(SQLite(), "data/reich_v50.sqlite")
             query = "SELECT DISTINCT CHR FROM SNP_meta"
             res = dbSendQuery(db, query)
             chr_choices = dbFetch(res)
             dbClearResult(res)
             dbDisconnect(db)
             
             chr_choices = chr_choices[order(as.numeric(chr_choices$CHR)),]
             
             fluidRow(
               column(width=8, selectInput("exploreChr", "Chromosome", choices=chr_choices, selected="1", multiple=TRUE)),
               column(width=4, checkboxGroupInput("exploreColumns", "Columns", choices=NULL, inline=TRUE))
             )
           })
  }) # /Render UI
  
  # Reactive data filter function for data explorer
  filter_explore = reactive({
    
    # Return NULL if the Data Explorer tab isn't active
    if(input$navbar=="Map"){
      return(NULL)
    }
    
    # Retrieve relevant information from the SQL database with the user defined options
    switch(input$exploreData,
           "Samples"={
             if(is.null(input$exploreCountry)){
               return(NULL)
             }
             countries_query = paste("'", input$exploreCountry, "'", collapse=" OR Country==", sep="")
             db = dbConnect(SQLite(), "data/reich_v50.sqlite")
             query = "SELECT * FROM sample_meta"
             query = sprintf("SELECT * FROM sample_meta WHERE CAST(DateMean AS INTEGER) <= %s AND CAST(DateMean AS INTEGER) >=%s AND Country==%s", 
                             input$exploreRange[1], input$exploreRange[2], countries_query)
             res = dbSendQuery(db, query)
           },
           "SNPs"={
             if(is.null(input$exploreChr)){
               return(NULL)
             }
             chr_query = paste("'", input$exploreChr, "'", collapse=" OR CHR==", sep="")
             db = dbConnect(SQLite(), "data/reich_v50.sqlite")
             query = sprintf("SELECT * FROM SNP_meta WHERE CHR==%s", chr_query)
             res = dbSendQuery(db, query)
           })

    data_table = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    # The columns checkbox group input is updated to reflect the columns present in the current data being displayed
    updateCheckboxGroupInput(session, "exploreColumns", choices=colnames(data_table), selected=colnames(data_table), inline=TRUE)
    
    return(data_table)
  })
  
  # Reactive table data to retrieve/filter data to be displayed
  table_data = reactive({
    data_table = filter_explore()
    data_table = data_table[,input$exploreColumns]
    return(data_table)
  })
  
  # Render data table with the filtered data  
  output$table1=renderDataTable(table_data())
  
} # /server
