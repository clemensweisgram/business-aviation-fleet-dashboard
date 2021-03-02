# shiny app

#install.packages("shinyWidgets")
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(plotly)
library(DT)

ui <- fluidPage(
  titlePanel("Business Aviation Fleet Dashboard"),
  theme = shinythemes::shinytheme('lumen'),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "date", 
                  label = strong("Select date range"), 
                  min = as.Date("2012-12-01","%Y-%m-%d"), 
                  max = as.Date("2020-12-01","%Y-%m-%d"), 
                  value = c(as.Date("2012-12-01","%Y-%m-%d"), as.Date("2020-12-01","%Y-%m-%d")),
                  timeFormat="%Y-%m-%d"), # replace min/max with link to dataset # use "%b %Y" as dateformat to show only month year (not sure which effect that has on output
      selectInput(inputId = "aircraft",
                  label = strong("Select aircraft types"),
                  choices = c("_ALL_","ASTR","BE40","BE45","C25A","C25B","C25M","C500","C501","C502","C510","C525","C526","C52B","C545","C550",
                              "C551","C555","C560","C56X","C601","C604","C650","C680","C68A","C750","CE50","CE55","CJ3","CJ4","CL3","CL30",
                              "CL35","CL60","DA10","DA7X","DA90","E50P","E545","E550","E55P","EA50","F2HT","F2TH","F900","FA10","FA20","FA50",
                              "FA7X","FA8X","FJ10","FJ50","FLT2","FTH2","G100","G150","G200","G280","G350","G400","G450","G500","G550","G650",
                              "GA5C","GALX","GL30","GL5T","GL6T","GL7T","GLEX","GLF2","GLF3","GLF4","GLF5","GLF7","GULF","H125","H25A","H25B",
                              "H25C","H25G","HDJT","HRZN","HSB","J328","JCOM","L29A","L29B","L36","LJ","LJ23","LJ24","LJ25","LJ26","LJ28",
                              "LJ29","LJ30","LJ31","LJ32","LJ35","LJ45","LJ55","LJ60","LJ70","LJ75","LR23","LR36","LRJ","MU30","PRM1","S5T",
                              "S601","SBR1","SBR2","SF50","SJ30","WW23","WW24","WW25"),
                  selected = c("GLEX"), # change to "_ALL_"
                  multiple = TRUE,
                  selectize = TRUE),
      #selectizeInput(inputId = 'aircraft', label = strong("Select aircraft types"), choices = NULL, multiple = TRUE),
      h5(strong("Show by")),
      switchInput(inputId = "equcat",
                  label = NULL,
                  value = TRUE,
                  onLabel = "Type",
                  offLabel = "Cat",
                  onStatus = "primary",
                  offStatus = "primary"),
      radioButtons(inputId = "route", 
                   label = strong("Select route"),
                   choices = c("Total" = "total",
                               "Domestic" = "domestic",
                               "International" = "international")),
      sliderInput(inputId = "MTOW", 
                  label = strong("Select mid-MTOW range (lbs)"), 
                  min = 0, 
                  max = 150000,
                  value = c(26000, 65000),
                  step = 1000,
                  ticks = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Individual", plotlyOutput('plotly101_102')),
        tabPanel("Aggregate", plotlyOutput('plotly201')),
        tabPanel("Change", plotlyOutput('plotly301')),
        tabPanel("Aircraft Codes", DT::dataTableOutput('aircraft_codes')),
        tabPanel("Remarks", htmlOutput("remarks"))
      )
    )
  ),
  tags$footer("© Clemens Weisgram 2021", align = "left")
)

server <- function(input, output, session) {
  
  observe({
###################################
  # installing and loading packages
  #install.packages("tidyquant")
  #install.packages("pracma")
  #install.packages("tfplot")
  library(directlabels)
  library(tfplot)
  library(tidyquant)
  #library(tidyverse)
  library(readxl)
  library(ggplot2)
  library(dplyr)
  library(plotly)
  library(pracma)
  #library(zoo)
  
  # setting wd
  # setwd("/Users/clemensweisgram/Desktop/Business Aviation Analysis") #commented out for shinyapps.io deployment
  
  # importing data
  data_month <- read_excel("data.xlsx", sheet = "month")
  data_year <- read_excel("data.xlsx", sheet = "year")
  data_fleet <- read_excel("data.xlsx", sheet = "fleet_type")
  aircraft_specs <- read_excel("aircraft_specs.xlsx", sheet = "Aircraft Database")
  
  ## MTOW
  # preparing MTOW table
  aircraft_specs$MTOW <- as.numeric(aircraft_specs$MTOW)
  
  MTOW <- aircraft_specs %>%
    select("ICAO_Code", "MTOW") %>%
    arrange(ICAO_Code, desc(MTOW)) %>%
    filter(grepl(paste(unique(data_fleet$EQUIPMENT), collapse="|"), ICAO_Code)) %>%
    distinct(ICAO_Code, .keep_all = TRUE)
  
  # setting limits for weight categories
  MTOW$cat  <- cut(as.numeric(MTOW$MTOW), c(0, input$MTOW[1], input$MTOW[2], Inf), labels=c("small", "mid", "large"))
  ## MTOW END
  
  
  # fixing date column
  data_fleet$YYYYMM <- as.Date(paste0(as.character(data_fleet$YYYYMM), '01'), format='%Y%m%d')
  colnames(data_fleet) <- c("date", "Equipment", "Total", "Domestic", "International")
  
  # prepared code to automize that all selectable aircraft are not hard coded but taken from dataset
  # use select_data to find all aircraft types and copy them in input$aircraft (to be automized)
  # creating select options for shiny app
  # select_data <- data.frame(matrix(nrow=0, ncol=length(unique(data_fleet$Equipment))+1))
  # colnames(select_data) <- append(c("_ALL_"),unique(data_fleet$Equipment))
  # select_data2 <- append(c("_ALL_"),unique(data_fleet$Equipment))
  
  #updateSelectizeInput(session, 'aircraft', choices = select_data2, selected = c("BE40", "GLEX", "ASTR"), server = TRUE)
  
  #
  if ("_ALL_" %in% input$aircraft) {
    input_from_selection <- c("ASTR","BE40","BE45","C25A","C25B","C25M","C500","C501","C502","C510","C525","C526","C52B","C545","C550",
      "C551","C555","C560","C56X","C601","C604","C650","C680","C68A","C750","CE50","CE55","CJ3","CJ4","CL3","CL30",
      "CL35","CL60","DA10","DA7X","DA90","E50P","E545","E550","E55P","EA50","F2HT","F2TH","F900","FA10","FA20","FA50",
      "FA7X","FA8X","FJ10","FJ50","FLT2","FTH2","G100","G150","G200","G280","G350","G400","G450","G500","G550","G650",
      "GA5C","GALX","GL30","GL5T","GL6T","GL7T","GLEX","GLF2","GLF3","GLF4","GLF5","GLF7","GULF","H125","H25A","H25B",
      "H25C","H25G","HDJT","HRZN","HSB","J328","JCOM","L29A","L29B","L36","LJ","LJ23","LJ24","LJ25","LJ26","LJ28",
      "LJ29","LJ30","LJ31","LJ32","LJ35","LJ45","LJ55","LJ60","LJ70","LJ75","LR23","LR36","LRJ","MU30","PRM1","S5T",
      "S601","SBR1","SBR2","SF50","SJ30","WW23","WW24","WW25")
  } else {
    input_from_selection <- input$aircraft
  }

  # selecting date and aircraft types
  data_fleet <- data_fleet %>%
    filter(date >= input$date[1], date <= input$date[2]) %>%
    filter(Equipment %in% input_from_selection) #c("GLEX", "ASTR")) #input$aircraft) #c("GLEX", "G400", "G500", "G550", "G650", "BE40", "ASTR", "G400", "Test"))
  
  
  #####
  ## equipment path
  #####
  # finding aircraft types with less than 12 entries (because can't calculate moving avg)
  data_lack <- data_fleet %>% 
    count(Equipment) %>%
    filter(n <= 12) %>%
    arrange(Equipment)
  
  # removing aircraft types with less data
  data_equ_start <- data_fleet[-which(data_fleet$Equipment %in% data_lack$Equipment),]
  
  if (nrow(data_equ_start) == 0) {
    data_equ_start <- data_fleet}
  
  # building total per month
  data_equ <- data_equ_start %>%
    group_by(date) %>%
    mutate(sumTotal = sum(Total), sumDomestic = sum(Domestic), sumInternational = sum(International)) %>%
    ungroup()
  
  # eliminating display per equipment and only show aggregated variables
  data_equ <- data_equ %>%
    distinct(date, sumTotal, sumDomestic, sumInternational)
  
  # adding change variables
  data_equ$Total_change <- NA
  data_equ$Total_change[12:length(data_equ$Total_change)] <- percentChange(ts(data_equ$sumTotal), lag = 12)
  
  data_equ$Domestic_change <- NA
  data_equ$Domestic_change[12:length(data_equ$Domestic_change)] <- percentChange(ts(data_equ$sumDomestic), lag = 12)
  
  data_equ$International_change <- NA
  data_equ$International_change[12:length(data_equ$International_change)] <- percentChange(ts(data_equ$sumInternational), lag = 12)
  
  # calculate 12m-moving-avg
  data_equ <- data_equ %>%
    tq_mutate(
      # tq_mutate args
      select     = c(sumTotal, sumDomestic, sumInternational),
      mutate_fun = rollapply, 
      # rollapply args
      width      = 12,
      align      = "right",
      FUN        = mean,
      # mean args
      na.rm      = TRUE,
      # tq_mutate args
      col_rename = c("Total12mavg", "Domestic12mavg", "International12mavg"))
  
  # rounding results
  data_equ$Total_change <- round(data_equ$Total_change, digits = 2)
  data_equ$Domestic_change <- round(data_equ$Domestic_change, digits = 2)
  data_equ$International_change <- round(data_equ$International_change, digits = 2)
  
  data_equ$Total12mavg <- round(data_equ$Total12mavg)
  data_equ$Domestic12mavg <- round(data_equ$Domestic12mavg)
  data_equ$International12mavg <- round(data_equ$International12mavg)
  
  
  # visualization: absolute numbers [CODE: 201]
  plot201 <- ggplot(data=data_equ, aes(x=date)) +
    geom_line(aes(y=Total12mavg, group=1), color = "black", linetype = "solid") + 
    geom_line(aes(y=Domestic12mavg, group=1), color = "grey30", linetype = "dashed") + 
    geom_line(aes(y=International12mavg, group=1), color = "grey30", linetype = "dashed") + 
    theme(panel.background = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line( size=.1, color="grey85", linetype = "solid"),
          axis.text.x=element_text(angle=45,hjust=1)) +
    ylim(0, max(data_equ$Total12mavg)+20000) +
    xlab("") +
    ylab("Monthly flights") 
  
  plotly201 <- ggplotly(
    p = plot201,
    width = NULL,
    height = NULL,
    tooltip = "all",
    dynamicTicks = FALSE,
    layerData = 1,
    originalData = TRUE,
    source = "A")
  
  plotly201
  
  # converting input$route for plotly 301
  if (input$route == "total") {
    flights <- data_equ$Total_change
  } else if (input$route == "domestic"){
    flights <- data_equ$Domestic_change
  } else if (input$route == "international"){
    flights <- data_equ$International_change
  }
  
  # visualization: percent change [CODE: 301]
  plot301 <- ggplot(data=data_equ, aes(x=date)) +
    geom_bar(aes(y=flights), stat="identity", color = "grey30") +
    #geom_bar(aes(y=Domestic_change), stat="identity", color = "grey80") +
    #geom_bar(aes(y=International_change), stat="identity", color = "lightblue") +
    theme(panel.background = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line( size=.1, color="grey85", linetype = "solid"),
          axis.text.x=element_text(angle=45,hjust=1)) +
    xlab("") +
    ylab("Change YOY (%)")
  
  plotly301 <- ggplotly(
    p = plot301,
    width = NULL,
    height = NULL,
    tooltip = "all",
    dynamicTicks = FALSE,
    layerData = 1,
    originalData = TRUE,
    source = "A")
  
  plotly301
  
  
  ##########
  ## per equipment type
  ##########
  
  # moving avg per equipment
  table <- data.frame() # creating empty dataframe to store results
  
  for (i in unique(data_equ_start$Equipment)){
    intermediate_data <- data_equ_start %>%
      filter(Equipment == i)
    
    intermediate_data$Total12mavg <- NA
    intermediate_data$Total12mavg[12:length(intermediate_data$Total12mavg)] <- movavg(x = intermediate_data$Total, n = 12, type = "s")
    
    intermediate_data$Domestic12mavg <- NA
    intermediate_data$Domestic12mavg[12:length(intermediate_data$Domestic12mavg)] <- movavg(x = intermediate_data$Domestic, n = 12, type = "s")
    
    intermediate_data$International12mavg <- NA
    intermediate_data$International12mavg[12:length(intermediate_data$International12mavg)] <- movavg(x = intermediate_data$International, n = 12, type = "s")
    
    
    table <- rbind(table, intermediate_data)
  } # closing for loop
  
  # rounding results of new columns
  table$Total12mavg <- round(table$Total12mavg)
  table$International12mavg <- round(table$International12mavg)
  table$Domestic12mavg <- round(table$Domestic12mavg)
  
  # storing result back in previous name
  data_equ_ind <- table
  
  # converting input$route for plotly 101
  if (input$route == "total") {
    flights <- data_equ_ind$Total12mavg
  } else if (input$route == "domestic"){
    flights <- data_equ_ind$Domestic12mavg
  } else if (input$route == "international"){
    flights <- data_equ_ind$International12mavg
  }
  
  # line graph fleet month individual [Code: 101]
  plot101 <- ggplot(data=data_equ_ind, aes(x=date, y=flights, group=Equipment)) +
    geom_line() +
    theme(panel.background = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line( size=.1, color="grey85", linetype = "solid"),
          axis.text.x=element_text(angle=45,hjust=1)) +
    xlab("") +
    ylab("Monthly flights")
  
  plotly101 <- ggplotly(
    p = plot101,
    width = NULL,
    height = NULL,
    tooltip = "all",
    dynamicTicks = FALSE,
    layerData = 1,
    originalData = TRUE,
    source = "A")
  
  plotly101
  
  
  #####
  ## category path
  #####
  
  # adding weight category to original dataframe
  data_cat <- data.frame() # creating empty dataframe to store results
  
  for (j in c("small", "mid", "large")) {
    cat_smallmidlarge <- MTOW %>%
      filter(cat == j) %>%
      select(ICAO_Code)
    
    cat_smallmidlarge <- cat_smallmidlarge[["ICAO_Code"]]
    
    data_cat_smallmidlarge <- data_fleet[which(data_fleet$Equipment %in% cat_smallmidlarge),]
    data_cat_smallmidlarge$cat <- j
    
    data_cat <- rbind(data_cat, data_cat_smallmidlarge)
  } # closing for loop
  
  # adding sums per cat
  data_cat <- data_cat %>%
    group_by(date, cat) %>% 
    mutate(Total_per_cat = sum(Total)) %>% # changed to mutate to keep all columns
    mutate(Domestic_per_cat = sum(Domestic)) %>% # changed to mutate to keep all columns
    mutate(International_per_cat = sum(International)) %>% # changed to mutate to keep all columns
    arrange(date)
  
  # re-arranging data
  data_cat <- data_cat[c("date", "cat", "Total_per_cat", "Domestic_per_cat", "International_per_cat")]
  data_cat <- data_cat %>%
    distinct() %>%
    arrange(cat, date)
  
  # moving avg per cat
  table_cat <- data.frame() # creating empty dataframe to store results
  
  for (j in unique(data_cat$cat)){
    intermediate_data <- data_cat %>%
      filter(cat == j)
    
    intermediate_data$Total12mavg <- NA
    intermediate_data$Total12mavg[12:length(intermediate_data$Total12mavg)] <- movavg(x = intermediate_data$Total_per_cat, n = 12, type = "s")
    
    intermediate_data$Domestic12mavg <- NA
    intermediate_data$Domestic12mavg[12:length(intermediate_data$Domestic12mavg)] <- movavg(x = intermediate_data$Domestic_per_cat, n = 12, type = "s")
    
    intermediate_data$International12mavg <- NA
    intermediate_data$International12mavg[12:length(intermediate_data$International12mavg)] <- movavg(x = intermediate_data$International_per_cat, n = 12, type = "s")
    
    
    table_cat <- rbind(table_cat, intermediate_data)
  } # closing for loop
  
  # rounding results of new columns
  table_cat$Total12mavg <- round(table_cat$Total12mavg)
  table_cat$Domestic12mavg <- round(table_cat$Domestic12mavg)
  table_cat$International12mavg <- round(table_cat$International12mavg)
  
  # storing result to name of previous data set
  data_cat <- table_cat
  
  # adding columns with percent change
  table_cat2 <- data.frame() # creating empty dataframe to store results
  
  for (k in unique(data_cat$cat)){
    intermediate_data <- data_cat %>%
      filter(cat == k)
    
    intermediate_data$Total_change <- NA
    intermediate_data$Total_change[12:length(intermediate_data$Total_change)] <- percentChange(ts(intermediate_data$Total_per_cat), lag = 12)
    
    intermediate_data$Domestic_change <- NA
    intermediate_data$Domestic_change[12:length(intermediate_data$Domestic_change)] <- percentChange(ts(intermediate_data$Domestic_per_cat), lag = 12)
    
    intermediate_data$International_change <- NA
    intermediate_data$International_change[12:length(intermediate_data$International_change)] <- percentChange(ts(intermediate_data$International_per_cat), lag = 12)
    
    table_cat2 <- rbind(table_cat2, intermediate_data)
  } # closing for loop
  
  # storing result to name of previous data set
  data_cat <- table_cat2
  
  # selecting which cat to show
  data_cat <- data_cat %>%
    filter(cat %in% c("small", "mid", "large"))
  
  # converting input$route for plotly 102
  if (input$route == "total") {
    flights <- data_cat$Total12mavg
  } else if (input$route == "domestic"){
    flights <- data_cat$Domestic12mavg
  } else if (input$route == "international"){
    flights <- data_cat$International12mavg
  }
  
  # line graph fleet month per cat [Code: 102]
  plot102 <- ggplot(data=data_cat, aes(x=date, y=flights, group=cat)) +
    geom_line() +
    theme(panel.background = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line( size=.1, color="grey85", linetype = "solid"),
          axis.text.x=element_text(angle=45,hjust=1)) +
    xlab("") +
    ylab("Monthly flights")
  
  plotly102 <- ggplotly(
    p = plot102,
    width = NULL,
    height = NULL,
    tooltip = "all",
    dynamicTicks = FALSE,
    layerData = 1,
    originalData = TRUE,
    source = "A")
  
  plotly102
  
  
  ###################
  ## APPENDIX
  ###################
  
  # # show plotly of last ggplot
  # ggplotly(
  #   p = ggplot2::last_plot(),
  #   width = NULL,
  #   height = NULL,
  #   tooltip = "all",
  #   dynamicTicks = FALSE,
  #   layerData = 1,
  #   originalData = TRUE,
  #   source = "A")
  
  ###################
  ## data_year
  ###################
  
  # line graph Total year
  ggplot(data=data_year, aes(x=Year, y=Total, group=1)) +
    geom_line() +
    ylim(0, max(data_year$Total)+20000)
  
  # bar chart changes compared to prev year's
  ggplot(data=data_year, aes(x=Year)) +
    geom_bar(aes(y=Change_Total), stat="identity", color = "grey30") +
    #geom_bar(aes(y=Change_Domestic), stat="identity", color = "grey80") +
    #geom_bar(aes(y=Change_International), stat="identity", color = "lightblue") +
    theme(panel.background = element_blank(), 
          panel.grid.major.x = element_blank(), 
          panel.grid.major.y = element_line( size=.1, color="grey30", linetype = "solid"),
          axis.text.x=element_text(angle=45,hjust=1))
  
  
  
  
  
  
  
  
  
##################################
  
  # visualizing tab 1
  if (input$equcat == TRUE) {
    output$plotly101_102 <- renderPlotly(plotly101)
  } else if (input$equcat == FALSE) {
    output$plotly101_102 <- renderPlotly(plotly102)
  }

  
  # visualizing tab 2
  output$plotly201 <- renderPlotly(plotly201)
  
  # visualizing tab 3
  output$plotly301 <- renderPlotly(plotly301)
  
  # aircraft codes tab
  aircraft_codes <- aircraft_specs %>%
    select("Manufacturer", "Model", "ICAO_Code", "MTOW") %>%
    filter(ICAO_Code %in% c("ASTR","BE40","BE45","C25A","C25B","C25M","C500","C501","C502","C510","C525","C526","C52B","C545","C550",
                            "C551","C555","C560","C56X","C601","C604","C650","C680","C68A","C750","CE50","CE55","CJ3","CJ4","CL3","CL30",
                            "CL35","CL60","DA10","DA7X","DA90","E50P","E545","E550","E55P","EA50","F2HT","F2TH","F900","FA10","FA20","FA50",
                            "FA7X","FA8X","FJ10","FJ50","FLT2","FTH2","G100","G150","G200","G280","G350","G400","G450","G500","G550","G650",
                            "GA5C","GALX","GL30","GL5T","GL6T","GL7T","GLEX","GLF2","GLF3","GLF4","GLF5","GLF7","GULF","H125","H25A","H25B",
                            "H25C","H25G","HDJT","HRZN","HSB","J328","JCOM","L29A","L29B","L36","LJ","LJ23","LJ24","LJ25","LJ26","LJ28",
                            "LJ29","LJ30","LJ31","LJ32","LJ35","LJ45","LJ55","LJ60","LJ70","LJ75","LR23","LR36","LRJ","MU30","PRM1","S5T",
                            "S601","SBR1","SBR2","SF50","SJ30","WW23","WW24","WW25"))
  
  
  output$aircraft_codes <- DT::renderDataTable({ aircraft_codes })
  
  
  }) #closing observe
  
  # static text in "Remarks" tab
  output$remarks <- renderText({
    paste("<br>Source: FAA Business Jet Reports 2012-2021, Aircraft Characteristics Database <br><br>
          <b>USER GUIDE</b><br><br>
          <b>Visualization specifications:</b> Equally weighted 12m moving average (to offset seasonality and day-of-week effects) <br><br>
          <b>Aircraft type input field</b> cannot be empty. Select new aircraft type(s) before removing previously used aircraft type(s) <br><br>
          <b>MTOW slider input:</b> marked range (blue) represents mid category. Below lower marker is small category, above upper marker is large category <br><br>
          <b>Individual tab</b> shows performance of each aircraft type (if show by switch is set to type) or weight category (if show by switch is set to cat) for either total, domestic, or international (depending on selection) monthly flights (12m moving average) <br><br>
          <b>Aggregate tab</b> shows total, domestic, and international monthly flights (12m moving average) of all selected aircraft types aggregated into one group <br><br>
          <b>Change tab</b> shows year-over-year change of monthly flights for the selected aircraft type(s) for either total, domestic, or international trips (depending on selection) <br><br>
          Contact: cweisgram2019@student.hult.edu")
  })
  
  
}

shinyApp(ui = ui, server = server)

