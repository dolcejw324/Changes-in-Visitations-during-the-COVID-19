#Sys.setenv('R_MAX_VSIZE'=128000000000)
# install.packages("spData")
# install.packages("sf")
# install.packages('DT')
# install.packages("leaflet")
# install.packages("usmap")
# install.packages("rgdal")
# install.packages("dygraphs")
# install.packages("xts")
# install.packages("colorRamps")
#install.packages("shinydashboard")
library(ggplot2)
library(dplyr)
library(tidyr)
library(iotools)
library(shiny)
library(tidyverse)
library(data.table)
library(waldo)
library(stringr)
library(readr)
library(spData) # For getting spatial data
library(sf) # For preserving spatial data
library(leaflet) # For making maps
library(DT)
library(usmap)
library(rgdal)
library(dygraphs)
library(xts)
library(htmltools)
library(colorRamps)
library(scales)
library(shinydashboard)
#setwd("~/Texas_a&m/Courses/CSCE700_data_analysis_and_visuaization/05_project")
poi <- read_csv("covidpoi_all_final.csv")
poi[c("sub_category")][is.na(poi[c("sub_category")])] <- "indoor"
poi_v2 <- poi %>% 
  mutate(type = if_else(sub_category ==  "All Other Amusement and Recreation Industries"| sub_category== "Promoters of Performing Arts, Sports, and Similar Events with Facilities"| sub_category== "Historical Sites"| sub_category== "Fitness and Recreational Sports Centers"| sub_category== "Museums"| sub_category== "Amusement Arcades"| sub_category== "Bowling Centers"| sub_category=="Casinos (except Casino Hotels)"| sub_category== "indoor", "indoor", "outdoor"))
# por_barplot <- poi_v2 %>% group_by(type, start_date) %>% dplyr::summarize(visitation_sum = sum(raw_visit_counts)) %>% as.data.frame()
por_barplot <- poi_v2 %>% group_by(type, start_date, ZIP) %>% dplyr::summarize(visitation_sum = sum(raw_visit_counts)) %>% as.data.frame()
por_barplot <- arrange(por_barplot, start_date, ZIP)
por_barplot$ZIP = as.character(por_barplot$ZIP)
names(por_barplot)[3] <- 'zipcode'
por_barplot <- complete(por_barplot, type, start_date, zipcode, fill = list(visitation_sum = 0))
por_barplot['year'] <- format(por_barplot$start_date, format="%Y")

#map stuff
# map_zip <- readOGR("harris-county-tx-zipcodes.shp")
map_zip <- readOGR("COH_ZIPCODE.shp")
zipcode <- unique(poi_v2$ZIP)
map_data <- poi_v2 %>% group_by(type, start_date, ZIP) %>% dplyr::summarize(visitation_sum = sum(raw_visit_counts)) %>% as.data.frame()
#tachometer


ui <- dashboardPage(
  dashboardHeader(title = "Safegraph visitation data exploration"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Changes in Visitations during the COVID-19", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Indoor vs. Outdoor public places", tabName = "dashboard1", icon = icon("tachometer-alt")),
      menuItem("Mobility of People", tabName = "dashboard2", icon = icon("tachometer-alt")),
      menuItem("About", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
        fluidRow(
          box(
            title = "Controls", width = 3, #solidHeader = TRUE,
            selectInput(inputId = "year",
                        label = "Year",
                        choices = c("All","before covid", "after covid", 
                                    "difference - only for map", "2019","2020","2021")
            ),
            radioButtons(inputId ="type",label = "type",choices = c("All",
                                                                    unique(as.character(por_barplot$type)))
            )
          )
          ,
          box(  title = "Visitation Map", width = 9, status = "primary", #solidHeader = TRUE,
                collapsible = TRUE, leafletOutput(outputId = "map_zipcode")),
          
          
        ),
        fluidRow(
          box(  title = "Visitation stats", width = 12, status = "primary", #solidHeader = TRUE,
                collapsible = TRUE, plotOutput("VisitationBarPlot", height = 350)),
          
          
        )
      )
      ,
      tabItem(tabName = "dashboard1",
              h2("j tab content"),
              fluidRow(
                box(
                  title = "Controls", width = 3, #solidHeader = TRUE,
                  selectInput(inputId = "year1",
                              label = "Year",
                              choices = c("All","before covid", "after covid", 
                                          "difference - only for map", "2019","2020","2021")
                  ),
                  radioButtons(inputId ="type1",label = "type",choices = c("All",
                                                                          unique(as.character(por_barplot$type)))
                  )
                )
                ,
                box(  title = "Visitation line plot", width = 9, status = "primary", #solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("VisitationlinePlot")),
                
                
              ),
              fluidRow(
                box(  title = "Visitation violin plot", width = 12, status = "primary", #solidHeader = TRUE,
                      collapsible = TRUE, plotOutput("VisitationviolinPlot", height = 350)),
                
              )
      )
      ,
      tabItem(tabName = "dashboard2",
              h2("d tab content")
      )
      ,
      tabItem(tabName = "widgets",
              h2("About the study "),
              h4("Last update"),
              "03 May 2022",
              h4("Introduction"),
              "The COVID-19 pandemic has been the most critical global public health crisis during the past two years. However, the pandemic cannot yet be declared that it is over, and due to the prolonged period of the pandemic, people's visits to public places may be subject to change as a result of policy changes or as a result of acclimating to the pandemic. This study aims to understand changes to Arts, Entertainment, and Recreation-related public spaces visitation before and after the pandemic by using the commercial dataset SafeGraph.",
              "The data contain anonymized mobile phone records on visit counts to 4.4 million POIs across the US through lo-cation-enabled applications. A POI is a point that represents the geographical location of a place that someone may find interesting. POIs were classified based on the North American Industry Classification System and the one categorized as 71- Arts, Entertainment, and Recreation were extracted. We look at the visitation to public places during the pandemic, starting from the beginning of 2019 to the end of 2021, and compare the visitation with those prior to the pandemic. Additionally, we examine which census block each visitor originates from. The second dataset we use is the COVID-19 daily confirmed cases and daily death cases.",
              h4("Code"),
              "Code and input data used to generate this Shiny mapping tool are available on Github:https://github.com/dolcejw324/Changes-in-Visitations-during-the-COVID-19",
              h4("Sources"),
              "Visitation data: Commercial dataset from SafeGraph. The data contain anonymized mobile phone records on visit counts to 4.4 million POIs across the US through location-enabled applications",
              "Total confirmed COVID-19 cases: Johns Hopkins Center for Systems Science and Engineering",
              
              h4("Authors"),
              "Ruben Lopez, Texas A&M University",
              "Yizhen Ding, Texas A&M University",
              "Jiwoon Jeong, Texas A&M University"
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
#First tab
  output$VisitationBarPlot = renderPlot({
    por_barplot0 <- por_barplot %>% group_by(type, start_date, year) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    if (input$year == "after covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date > as.Date("2020-03-01"))
    }
    else if (input$year == "before covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date < as.Date("2020-04-01"))
    }
    # else {
    else if (input$year != "All" & input$year != "difference - only for map") {
      por_barplot0 <- filter(por_barplot0, year == input$year)
    }
    else {
    }
    if (input$type != "All") {
      por_barplot0 <- filter(por_barplot0, type == input$type)
    }
    ggplot(data=por_barplot0, aes(x=start_date, y=visitation_sum)) +
      geom_bar(stat="identity", fill="steelblue")+
      scale_x_date(date_breaks = "1 month", #labels
                   date_labels="%m-%y")+ #date_format("%d-%m-%Y"))+
      # geom_text(aes(label="number of visits"), vjust=-0.3, size=3.5)+
      #scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
      scale_y_continuous(labels = scales::label_number_si())+
      #labels = unit_format(unit = "M", scale = 1e-6))+
      xlab("Date (Month-Year)") +
      ylab("Number of visits")  +
      theme(axis.text.x=element_text(size=16,angle=90,hjust=1,vjust=0.5),
            axis.text.y=element_text(size=16))
  })
  
  
  output$map_zipcode <- renderLeaflet({
    if (input$type != "All") {
      por_barplot <- filter(por_barplot, type == input$type)
    }
    if (input$year == "after covid") {
      por_barplot <- filter(por_barplot, por_barplot$start_date > as.Date("2020-03-01"))
      por_barplot1 <- por_barplot %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    }
    else if (input$year == "before covid") {
      por_barplot <- filter(por_barplot, por_barplot$start_date < as.Date("2020-04-01"))
      por_barplot1 <- por_barplot %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    }
    else if (input$year == "difference - only for map") {
      por_barplot1 <- filter(por_barplot, por_barplot$start_date > as.Date("2020-03-01")) %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
      por_barplot0 <- filter(por_barplot, por_barplot$start_date < as.Date("2020-04-01")) %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
      por_barplot2 <- por_barplot0
      por_barplot2['visitation_sum'] <- por_barplot1$visitation_sum - por_barplot0$visitation_sum 
      por_barplot1 <- por_barplot2
    }
    else if (input$year != "All") {
      por_barplot <- filter(por_barplot, year == input$year)
      por_barplot1 <- por_barplot %>% group_by(zipcode) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    }
    else {
      por_barplot1 <- por_barplot
    }
    por_barplot1$visitation_sum = por_barplot1$visitation_sum/1000000
    # add data to map
    orderzip <- match(map_zip@data$ZIP_CODE, por_barplot1$zipcode)
    map_zip@data <- por_barplot1[orderzip, ]
    pal <- colorNumeric(palette=matlab.like(256), domain = map_zip$visitation_sum)
    labels <- sprintf("<strong> Zip code: %s</strong><br/>Number of visits %g <sup>2</sup>", map_zip$zipcode, map_zip$visitation_sum) %>% lapply(htmltools::HTML)
    m <- leaflet(map_zip) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(visitation_sum),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.8,
          bringToFront = TRUE),
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~visitation_sum,
        opacity = 0.8, # title = NULL,
        title = "<small>Visits<br> per <br>million</small>"

      )
  })
  
  
  
  
#second tab
  output$VisitationlinePlot <-  renderPlot({
    por_barplot <- poi_v2 %>% group_by(type, start_date, ZIP) %>% dplyr::summarize(visitation_sum = sum(raw_visit_counts)) %>% as.data.frame()
    por_barplot <- arrange(por_barplot, start_date, ZIP)
    por_barplot$ZIP = as.character(por_barplot$ZIP)
    names(por_barplot)[3] <- 'zipcode'
    por_barplot <- complete(por_barplot, type, start_date, zipcode, fill = list(visitation_sum = 0))
    por_barplot['year'] <- format(por_barplot$start_date, format="%Y")
    
    por_barplot2 <- poi_v2 %>% group_by(type, start_date, ZIP) %>% dplyr::summarize(TotalConfirmedCases = sum(TotalConfirmedCases)) %>% as.data.frame()
    por_barplot2 <- arrange(por_barplot2, start_date, ZIP)
    por_barplot2$ZIP = as.character(por_barplot2$ZIP)
    names(por_barplot2)[3] <- 'zipcode'
    por_barplot2 <- complete(por_barplot2, type, start_date, zipcode, fill = list(visitation_sum = 0))
    por_barplot2['year'] <- format(por_barplot2$start_date, format="%Y")
    
    por_barplot2 <-  por_barplot2[, c('zipcode','TotalConfirmedCases')]
    por_barplot$TotalConfirmedCases= paste(por_barplot2$TotalConfirmedCases)
    por_barplot0<-por_barplot
    
    por_barplot0$visitation_sum <- as.numeric(por_barplot0$visitation_sum)
    por_barplot0$TotalConfirmedCases <- as.numeric(por_barplot0$TotalConfirmedCases)
    
    colors <- c("indoor" = "blue", "outdoor" = "red", "Total Confirmed Covid-19 Cases" = "green")
    
    #date_range <- which(por_barplot0$start_date %in% as.Date(c("2020-03-01", "2020-04-01")) )
    if (input$year1 == "after covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date > as.Date("2020-03-01"))
    }
    else if (input$year1 == "before covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date < as.Date("2020-04-01"))
    }
    # else {
    else if (input$year1 != "All" & input$year1 != "difference - only for map") {
      por_barplot0 <- filter(por_barplot0, year == input$year1)
    }
    else {
    }
    if (input$type1 != "All") {
      por_barplot0 <- filter(por_barplot0, type == input$type1)
    }
    ggplot(data=por_barplot0, aes(x=start_date, y=visitation_sum, color=type)) +
      stat_summary(aes(y = visitation_sum), fun=mean, geom = "line") +
      stat_summary(aes(y = TotalConfirmedCases, color="Total Confirmed Covid-19 Cases"), fun=mean, geom = "line", col = "green") +
      scale_x_date(date_breaks = "1 month", #labels
                   date_labels="%b-%Y")+ #date_format("%d-%m-%Y"))+
      # geom_text(aes(label="number of visits"), vjust=-0.3, size=3.5)+
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
      geom_vline(aes(xintercept = as.Date("2020-03-01")), col = "black")+
      geom_vline(aes(xintercept = as.Date("2020-04-01")), col = "black")+
      xlab("Date") +
      ylab("Number of visits") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title="Changes in Visitations", caption = "Black lines: when the lockdown policy started & ended in Texas")+
      scale_colour_manual(name="Lines",values=colors)
    
  })
  
  output$VisitationviolinPlot <- renderPlot({
    
    por_barplot0 <- por_barplot %>% group_by(type, start_date, year) %>% dplyr::summarize(visitation_sum = sum(visitation_sum)) %>% as.data.frame()
    if (input$year1 == "after covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date > as.Date("2020-03-01"))
    }
    else if (input$year1 == "before covid") {
      por_barplot0 <- filter(por_barplot0, por_barplot0$start_date < as.Date("2020-04-01"))
    }
    # else {
    else if (input$year1 != "All" & input$year1 != "difference - only for map") {
      por_barplot0 <- filter(por_barplot0, year == input$year1)
    }
    else {
    }
    if (input$type1 != "All") {
      por_barplot0 <- filter(por_barplot0, type == input$type1)
    }
    ggplot(data=por_barplot0, aes(x=type, y=visitation_sum, color=type)) +
      geom_violin()+
      geom_point()+
      #scale_x_date(date_breaks = "1 month", #labels
      #             date_labels="%b-%Y")+ #date_format("%d-%m-%Y"))+
      # geom_text(aes(label="number of visits"), vjust=-0.3, size=3.5)+
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
      xlab("Type") +
      ylab("Number of visits") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      labs(title="Distribution of Visitations")
    
  }) 
}

shinyApp(server=server, ui=ui)












