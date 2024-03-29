library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(RSQLite)
library(RColorBrewer)
library(plotly)

setwd("../")

# Main server function
server = function(input, output, session){
  # Server-wise global reactive variable for number of SNPs
  twoSNPs = reactive({
    if(input$num_SNP==1){
      FALSE
    }else{
      TRUE
    }
  })
  
  # Server-wise global reactive variable for distance threshold for clustering
  dist_threshold = reactive({
    as.numeric(input$group_dist_input)
  })
  
  # Palette choice (Qual, Seq, Div)
  plot_palette = reactive({
    input$palette_choice
  })
  
  # Plot type (Pie, Bar)
  plot_type = reactive({
  switch(input$plot_type,
         "Pie"=return("pie"),
         "Bar"=return("bar")
         )
  })
  
  # Opacity
  opacity_val = reactive({
    as.numeric(input$opacity)/100
  })
  
  plot_size = reactive({
    subset_dat = filteredData()
    num_rows = nrow(subset_dat)
    minimum_size = 10
    maximum_size = 100
    multiplier = (maximum_size-minimum_size)/100
    return((minimum_size+(multiplier*as.numeric(input$plot_size))))
  })
  
  legend_show = reactive({
    if(input$legend=="None"){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })
  
  legend_pos = reactive({
    switch(input$legend,
           "TR"="topright",
           "TL"="topleft",
           "BR"="bottomright",
           "BL"="bottomleft")
  })
  
  label_show = reactive({
    if(!input$showlabel){
      leafletProxy("map") %>% 
        clearMarkers()
    }
    return(input$showlabel)
  })
  
  data_type = reactive({
    input$data_type
  })
  
  SNP1_alleles = reactive({
    SNP1_choice = input$SNP_choice1
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT REF, ALT FROM SNP_meta WHERE SNP_ID='%s'", SNP1_choice))
    SNP1_alleles = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    SNP1_alleles[SNP1_alleles=="1" | SNP1_alleles==1]="A"
    SNP1_alleles[SNP1_alleles=="2" | SNP1_alleles==2]="C"
    SNP1_alleles[SNP1_alleles=="3" | SNP1_alleles==3]="G"
    SNP1_alleles[SNP1_alleles=="4" | SNP1_alleles==4]="T"
    
    return(SNP1_alleles)
  })
  
  SNP2_alleles = reactive({
    SNP2_choice = input$SNP_choice2
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT REF, ALT FROM SNP_meta WHERE SNP_ID='%s'", SNP2_choice))
    SNP2_alleles = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    SNP2_alleles[SNP2_alleles=="1" | SNP2_alleles==1]="A"
    SNP2_alleles[SNP2_alleles=="2" | SNP2_alleles==2]="C"
    SNP2_alleles[SNP2_alleles=="3" | SNP2_alleles==3]="G"
    SNP2_alleles[SNP2_alleles=="4" | SNP2_alleles==4]="T"
    
    return(SNP2_alleles)
  })
  
  # Print Ref/Alt alleles 
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
  
  # Listen to CHR1 choice 
  observeEvent(input$CHR_choice1, {
    CHR_ID = input$CHR_choice1
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    updateSelectizeInput(session,"SNP_choice1",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  # Listen to CHR2 choice
  observeEvent(input$CHR_choice2, {
    CHR_ID = input$CHR_choice2
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    res = dbSendQuery(db, sprintf("SELECT SNP_ID FROM SNP_meta WHERE CHR=%s", CHR_ID))
    SNP_selection = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    updateSelectizeInput(session,"SNP_choice2",choices=unlist(SNP_selection$SNP_ID), server=TRUE)
  })
  
  # Listen to time step change
  observeEvent(input$timestep, {
    time_step=input$timestep
    updateSliderTextInput(session, inputId="timeline", 
                          choices=seq(from=time_range[1], to=time_range[2], by=-time_step))
  })
  
  # Update color scheme choices based on palette choice
  observeEvent(input$palette_type, {
    palette_type_choice = input$palette_type
    updateSelectInput(session, "palette_choice", choices=color_palettes[[palette_type_choice]])
  }, ignoreInit=TRUE)
  
  # Cluster groups
  cluster_table = reactive({
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    query = sprintf("SELECT MasterID, Long, Lat, Country FROM sample_meta")
    res = dbSendQuery(db, query)
    cluster_table = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    switch(input$grouping_var,
           "None"={
             cluster_table = cluster_table %>% 
               mutate(Lat = as.numeric(Lat), Long = as.numeric(Long)) %>% 
               group_by(Lat, Long) %>% 
               mutate(Cluster = cur_group_id()) %>% 
               mutate(Country = NA)
           },
           "Country"={
             cluster_table = cluster_table %>% 
               mutate(Lat = as.numeric(Lat), Long = as.numeric(Long)) %>% 
               group_by(Country) %>% 
               mutate(Cluster = cur_group_id()) %>% 
               select(c(MasterID, Country, Cluster)) %>% 
               merge(country_list, by="Country")
           },
           "Distance"={
             cluster_table = cluster_Samples(cluster_table)
             centroids = calc_Centroid(cluster_table)
             
             cluster_table = cluster_table %>%
               select(c(MasterID, Country, Cluster)) %>% 
               merge(centroids, by="Cluster") %>% 
               mutate(Country = NA)
           })
    
    return(cluster_table)
  })
  
  plot_group = reactive({
    cluster_table() %>% 
    select(Lat, Long, Cluster, Country) %>% 
    distinct()
  })
  
  # Filter data reactive to timeline change
  filteredData = reactive({
    start_time = input$timeline
    end_time = input$timeline-input$windowsize
    
    plot_dat = filter_samples(start_time, end_time)
    
    if(!input$showmissing){
      plot_dat = plot_dat %>% 
        select(-any_of(c("SNP1_Missing", "SNP2_Missing")))
    }
    
    if(!any(grepl("SNP1|SNP2", colnames(plot_dat)))){
      return(NULL)
    }
    
    # If user choses Freq data type, convert
    if(data_type()=="Freq" & input$showmissing){
      plot_dat = allele_freq(plot_dat)
    }else if(data_type()=="Freq" & !input$showmissing){
      plot_dat = allele_freq_no_missing(plot_dat)
    }
    
    return(plot_dat)
  })
  
  # MAIN CHART FUNCTION
  observe({
    subset_dat = filteredData()
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
    
    # Print labels
    if(label_show()){
      switch(input$grouping_var,
             "None"={
               leafletProxy("map") %>% 
                 clearMarkers() %>% 
                 addLabelOnlyMarkers(lng = as.numeric(subset_dat$Long), lat = as.numeric(subset_dat$Lat), 
                                     label = sprintf("%.4f, %.4f", as.numeric(subset_dat$Long), as.numeric(subset_dat$Lat)),
                                     labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                                 direction = "bottom", offset = c(0,5)))
             },
             "Country"={
               leafletProxy("map") %>% 
                 clearMarkers() %>% 
                 addLabelOnlyMarkers(lng = as.numeric(subset_dat$Long), lat = as.numeric(subset_dat$Lat), label = subset_dat$Country,
                                     labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                                 direction = "bottom", offset = c(0,5)))
             },
             "Distance"={
               leafletProxy("map") %>% 
                 clearMarkers() %>% 
                 addLabelOnlyMarkers(lng = as.numeric(subset_dat$Long), lat = as.numeric(subset_dat$Lat), label = paste("Cluster",subset_dat$Cluster),
                                     labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, 
                                                                 direction = "bottom", offset = c(0,5)))
             })
    }
    
    # Print map
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
    
    if(length(c(colnames(subset_dat)[!colnames(subset_dat) %in% disregard]))==1 && legend_show()){
      leafletProxy("map") %>% 
        addLegend(legend_pos(),labels=c(colnames(subset_dat)[!colnames(subset_dat) %in% disregard]),
                  colors = brewer.pal(n=3, plot_palette())[3], layerId="minichartsLegend")
      
    }
  })
  
  # Summary plot generation
  observeEvent(input$mainPanel_tabs, {
    if(input$mainPanel_tabs=="Controls"){
      return()
    }
    
    output$total_summary_SNP1 = renderPlotly({})
    output$total_summary_SNP2 = renderPlotly({})
    
    window_size = as.numeric(input$windowsize)
    
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
    
    final_df = data.frame()
    range_vec = seq(from=time_range[1], to=time_range[2], by=-time_step)
    withProgress(message = "Generating plot", value=0, min=0, max=length(range_vec),{
      for(start_time in range_vec){
        incProgress(1)
        end_time = start_time - window_size
        subset_dat = filter_samples(start_time, end_time)
        
        subset_dat = subset_dat %>%
          pivot_longer(-c(Cluster, Country, Lat, Long), names_to = c("SNP", "Type"), names_sep = "_", values_to = "Count") %>%
          group_by(SNP, Type) %>%
          summarise(n=sum(Count))
 
        if(!input$showmissing){
          subset_dat = subset_dat %>% 
            filter(Type!="Missing")
        }
        
        if(nrow(subset_dat)==0){
          next
        }
        
        print(subset_dat)
        
        # If user choses Freq data type, convert
        if(data_type()=="Freq" & input$showmissing){
          
          subset_dat = subset_dat %>% 
            pivot_wider(names_from = c(SNP, Type), values_from = n)
          
          subset_dat = allele_freq(subset_dat)
          
          subset_dat = subset_dat %>%
            pivot_longer(everything(), names_to = c("SNP", "Type"), names_sep = "_", values_to = "n")
          
        }else if(data_type()=="Freq" & !input$showmissing){
          
          subset_dat = subset_dat %>% 
            pivot_wider(names_from = c(SNP, Type), values_from = n)
          
          subset_dat = allele_freq_no_missing(subset_dat)
          
          subset_dat = subset_dat %>%
            pivot_longer(everything(), names_to = c("SNP", "Type"), names_sep = "_", values_to = "n")
        }
        
        if(!input$showmissing){
          figure_table = figure_table %>% 
            filter(Type!="Missing")
        }
        
        temp_table = figure_table %>%
          merge(subset_dat, by=c("SNP", "Type"), all=TRUE) %>% 
          mutate(Time=start_time)
        
        temp_table[is.na(temp_table)]=0
        
        final_df = rbind(final_df, temp_table)
      } # For loop
    }) # With progress
    
    SNP1_final = final_df[final_df$SNP == "SNP1", ]
    
    output$total_summary_SNP1 = renderPlotly({
      plot_ly( x=SNP1_final$Time, y=SNP1_final$n, color=as.factor(SNP1_final$Type), type="scatter", mode="lines") %>%
      layout(title="SNP1",
             xaxis=list(title="Year (BP)", autorange="reversed", dtick=2500, tickmode="linear", tickformat="digit"),
             yaxis=list(title=input$data_type),
             hovermode="x unified")
    })
    
    if(twoSNPs()){
      SNP2_final = final_df[final_df$SNP == "SNP2", ]
      output$total_summary_SNP2 = renderPlotly({
        plot_ly( x=SNP2_final$Time, y=SNP2_final$n, color=as.factor(SNP2_final$Type), type="scatter", mode="lines") %>%
          layout(title="SNP2",
                 xaxis=list(title="Year (BP)", autorange="reversed", dtick=2500, tickmode="linear", tickformat="digit"),
                 yaxis=list(title=input$data_type),
                 hovermode="x unified")
      })
    }
    
  }, ignoreInit = TRUE)
  
  # No samples to plot -> plot empty map
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
  
  filter_samples = function(start_time, end_time){
    db = dbConnect(SQLite(), "data/reich_v50.sqlite")
    query = sprintf("SELECT MasterID, Long, Lat, Country FROM sample_meta WHERE CAST(DateMean AS INTEGER) <= %s AND CAST(DateMean AS INTEGER) >=%s", start_time, end_time)
    res = dbSendQuery(db, query)
    samples_to_plot = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    # Query and retrieve appropriate SNP data
    if(nrow(samples_to_plot)==0){
      plot_dat = no_samples()
    }else{
      plot_dat = query_main(samples_to_plot) 
    }
    
    # Pivot data to be wider
    plot_dat = plot_dat %>% 
      pivot_longer(-c(Cluster, Country, Lat, Long)) %>% 
      count(Cluster, Country, Lat, Long, name, value) %>% 
      pivot_wider(names_from = c(name, value), values_from = n) %>%
      replace(is.na(.), 0) %>% 
      relocate(sort(names(.)))
    
    return(plot_dat)
  }

  # Main function for querying/processing SNP data from main
  query_main = function(samples_to_plot){
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
  
  allele_freq_no_missing = function(dat){
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
      
      dat = dat %>% 
        mutate(SNP1_total=rowSums(select(.,contains("SNP1")))) %>% 
        mutate(SNP2_total=rowSums(select(.,contains("SNP2"))))
      
      SNP1_tab = dat %>% 
        select(contains("SNP1"))
      
      SNP2_tab = dat %>% 
        select(contains("SNP2"))
      
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
    
    return(dat)
  }
  
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
      
      dat = dat %>% 
        mutate(SNP1_total=rowSums(select(.,contains("SNP1")))) %>% 
        mutate(SNP2_total=rowSums(select(.,contains("SNP2"))))
      
      SNP1_tab = dat %>% 
        select(contains("SNP1"))
      
      SNP2_tab = dat %>% 
        select(contains("SNP2"))
      
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
  
  # Create distance matrix from distM
  dist_Matrix = function(dat){
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
  
  # Calculate centroids of clusters
  calc_Centroid = function(dat){
    if(length(unique(dat$Cluster))==1){
      cluster_centroids = data.frame(matrix(NA, nrow=1,ncol=3))
      colnames(cluster_centroids) = c("Cluster", "Lat", "Long")

      cluster_centroids$Cluster = 1
      cluster_centroids$Lat = dat$Lat[1]
      cluster_centroids$Long = dat$Long[1]
      
      return(cluster_centroids)
    }
    
    cluster_groups = dat %>% 
      group_by(Cluster, Lat, Long) %>% 
      summarise() %>% 
      sapply(as.numeric) %>% 
      data.frame()
    
    cluster_centroids = data.frame(matrix(NA, nrow=length(unique(cluster_groups$Cluster)),ncol=3))
    colnames(cluster_centroids) = c("Cluster", "Lat", "Long")
    
    cluster_centroids$Cluster = unique(cluster_groups$Cluster)
    
    for(cluster in unique(cluster_groups$Cluster)){
      lat_c = sum(cluster_groups$Lat[cluster_groups$Cluster==cluster])/length(cluster_groups$Lat[cluster_groups$Cluster==cluster])
      long_c = sum(cluster_groups$Long[cluster_groups$Cluster==cluster])/length(cluster_groups$Long[cluster_groups$Cluster==cluster])
      
      cluster_centroids$Lat[cluster_centroids$Cluster==cluster] = lat_c
      cluster_centroids$Long[cluster_centroids$Cluster==cluster] = long_c
    }
    
    return(cluster_centroids)
  } 
  
  # ===============================================
  # Data explorer tab
  
  exploreDatabase = reactive({
    input$dataset
  })
  
  output$exploreUI = renderUI({
    switch(input$exploreData,
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
  }) # Render UI
  
  filter_explore = reactive({
    if(input$navbar=="Map"){
      return()
    }
    
    switch(input$exploreData,
           "Samples"={
             if(is.null(input$exploreCountry)){
               return()
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
               return()
             }
             chr_query = paste("'", input$exploreChr, "'", collapse=" OR CHR==", sep="")
             db = dbConnect(SQLite(), "data/reich_v50.sqlite")
             query = sprintf("SELECT * FROM SNP_meta WHERE CHR==%s", chr_query)
             res = dbSendQuery(db, query)
           })

    data_table = dbFetch(res)
    dbClearResult(res)
    dbDisconnect(db)
    
    updateCheckboxGroupInput(session, "exploreColumns", choices=colnames(data_table), selected=colnames(data_table), inline=TRUE)
    
    return(data_table)
  })# %>% debounce(500)
  
  table_data = reactive({
    data_table = filter_explore()
    data_table = data_table[,input$exploreColumns]
    return(data_table)
  })
  
  output$table1=renderDataTable(table_data())
  
} # /server
