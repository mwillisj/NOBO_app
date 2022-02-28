
# Loading Libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggmap)
library(sf)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflegend)
library(htmltools)
library(htmlwidgets)
library(raster)
library(gstat)
library(spatial)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(rgdal)
library(RColorBrewer)
library(HatchedPolygons)
library(ggspatial)
library(stars)
library(ggpattern)
library(maps)
library(mapproj)
library(readxl)



options(scipen=999)


# Loading Variable Sets



## Loading Boundaries
State_Boundaries_Zip <- st_read("StateBoundariesZIP", quiet = T)


### loading counties

natl_priority_map <- st_read("NOBO_Boundary_Aug2021_Dissolve_ST_Draft", 
                             layer="NOBO_Boundary_Aug2021_Dissolve_ST_Draft")

transf_natl_PA <- natl_priority_map %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


##Loading Goal Collection (GCT) Data
GCT_data <- read_xlsx("NOBODATA_ForLeaflet_Final.xlsx")





##Joining Boundaries and GCT Data
GCT_and_Geographies <- st_as_sf(left_join(GCT_data, State_Boundaries_Zip, by =c("STATE" ="NAME")))



## read state abbr for later mapping
state_abbr <- data.frame(tolower(state.abb) %>%
                           cbind(tolower(state.name)))

colnames(state_abbr) <- c("state","state_name")







# Creating Labels for GCT Data + Bird Data


## Creating Label for States
GCT_and_Geographies$label <- 
  paste("<b>", "<big>", GCT_and_Geographies$STATE,"</b>", "</big>",
        "<br><i>", GCT_and_Geographies$REGION, "</i>",
        "<br>", "Values shown below reflect the combined value of", "<br>", "Original Program Goals and Framework Goals",
        "<br>",
        "<br>",
        "<b style='font-size:90%'> Top 3 Core Conservation Practices: </b>",
        "<br>",
        GCT_and_Geographies$TOP3CORE,
        "<br>",
        "<br>",
        "<b style='font-size:90%'>", "Financial Assistance:", "</b>","<br>",
        "$ ", prettyNum(GCT_and_Geographies$REQFIN_FRAME,big.mark=","),
        "<br>",
        "<br>",  "<b style='font-size:90%'>", "Total CP Coverage, Acres:", "</b>",
        "<br>", "Core:", prettyNum(GCT_and_Geographies$ACRE_FRAME_CORE, big.mark = ","),
        "<br>", "Supplemental:", prettyNum(GCT_and_Geographies$ACRE_FRAME_SUPP, big.mark = ","),
        "<br>", "Core and Supp Combined:", prettyNum(GCT_and_Geographies$ACRE_FRAME_CAS, big.mark = ","),
        "<br>",
        "<br>", "<b style='font-size:90%'>", "Total CP Coverage, Feet:", "</b>",
        "<br>", "Core:", prettyNum(GCT_and_Geographies$FT_FRAME_CORE, big.mark = ","),
        "<br>", "Supplemental:", prettyNum(GCT_and_Geographies$FT_FRAME_SUPP, big.mark = ","),
        "<br>", "Core and Supp Combined:", GCT_and_Geographies$FT_FRAME_CAS,
        "<br>",
        "<br>", "<b style='font-size:90%'>", "Total CP Coverage, Number of X:", "</b>",
        "<br>",GCT_and_Geographies$X_FRAME,
        "<br>",
        "<br>", "<b style='font-size:90%'>", "Number of Written Plans:", "</b>",
        "<br>",GCT_and_Geographies$WRITTEN_FRAME,
        "<br>",
        "<br>", "<b style='font-size:90%'>", "Number of Applied Plans:", "</b>",
        "<br>",GCT_and_Geographies$APPLIED_FRAME) %>% 
  lapply(htmltools::HTML)

bird_df <- readRDS(file = "forapp.rds") %>%
  rename(stateabbr = STATE)

bird_dat <- readRDS(file = "bird_dat.rds")

GCT_and_Geographies_df <- as.data.frame(GCT_and_Geographies)

bird_df <- bird_df %>%
  left_join(GCT_and_Geographies_df[,c(1, 38)], by = c("stateabbr" = "STUSPS"))


# Functions -------------------

## Plots -----

### create a function to plot data in bottom panel of shiny app ------------

depth_plot <- function(var_x, var_y, var_z = "REGION", var_state) {
  
  bird_df$fill <- ifelse(bird_df$STATE == var_state,
                                        "01Selected State",
                         bird_df$REGION)
  
  ggplot(bird_df) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_jitter(aes(.data[[var_x]], .data[[var_y]],
                   color = fill,
                   fill = fill,
                   size = abund),
               alpha = 0.5, shape = "circle") +
    scale_fill_manual(values = c("#f1a340","#af8dc3","black","#7fbf7b"), 
                      aesthetics = c("fill", "color"),
                      labels = c(var_state, "Central", "Northeast", "Southeast"),
                      name = NULL) +
    scale_size_continuous(name = "Abundance") +
    # geom_boxplot(aes(.data[[var_x]], .data[[var_y]])) +
    coord_flip() +
    labs(x = NULL, y = "Trend since 2002 (percent change)") +
    theme(
      legend.position = "bottom", 
      axis.title = element_text(size = 16),
      axis.text.x = element_text(family = "Trebuchet MS", size = 12),
      panel.grid = element_blank())
      # labs( x = NULL, y = var_y))
}



state_plot <- function(var_x, var_y, var_z) {
  GCT_and_Geographies_df$fill <- ifelse(GCT_and_Geographies_df$STATE == var_z,
                                        "Selected State",
                                        "Other State")
    
  ggplot(GCT_and_Geographies_df) + 
    geom_col(aes(x = reorder(.data[[var_x]], 
                             -.data[[var_y]]), 
                 y = .data[[var_y]],
                 fill = fill)) +
    scale_fill_manual(name = NULL, values = c("grey69","#f1a340"), 
                      labels = c("Other States", var_z)) +
    scale_x_discrete(name = NULL)
}



# create leaflet -------------

# TREND
bins_trend <- seq(min(bird_dat$trend),
                  max(bird_dat$trend), by = .25)


numPal <- colorNumeric(brewer.pal(11, "RdYlGn"), bird_dat$trend)


#ABUND
bins_abund <- seq(min(bird_dat$abund),
                  max(bird_dat$abund), 
                  by = 10)


symbols_size <- function(numpal = numPal, colorvalues = bird_dat$trend, Color = "black",
                          strokewidth = .15, Values = bird_dat$abund,
                          Shape = "circle",
                          Opacity = 0.8,
                          basesize) {
  makeSizeIcons(
    pal = numpal, colorValues = colorvalues, color = Color, strokeWidth = strokewidth,
    values = Values,
    shape = Shape,
    opacity = Opacity,
    baseSize = basesize
  )
}



# setup Leaflet
nobo_and_gct_leaflet <- leaflet(options=leafletOptions(minZoom = 4)) %>%
  addProviderTiles("Esri.WorldImagery", group="Aerial",
                   tileOptions(opacity = .6)) %>% 
  addProviderTiles(providers$Stamen, group = "Map", 
                   options = tileOptions(opacity = .5)) %>%

  #set Max Bounds
  setMaxBounds(lng1=-140.791110603, 
               lat1= 15,
               lng2= -55.96466,
               lat2= 81.3577635769) %>%
  
  #create layer toggle  
  addLayersControl(
    baseGroups = c("Aerial", "Map"),
    overlayGroups = c("Points", "States", "Priority Counties"), 
    position = "topleft", 
    options = layersControlOptions(collapsed = T)
  ) 

# Shiny App ----------------------------
## UI ---------

ui <- fluidPage(
  titlePanel("Northern Bobwhite in the US"),
  mainPanel(
    leafletOutput("map",
                  height = 400,
                  width = '100%'),
    sliderInput(inputId = "size", label = "Marker Size", 2, 10, value = 2),
    navbarPage("Plots",
               tabPanel("Bird Data",
                 selectInput("select", label = h5("Northern Bobwhite Abundance"),
                             choices = c("Priority Area" = "priority_area", 
                                         "State" = "STATE",
                                         "Region" = "REGION"),
                             selected = "STATE"),
                 plotOutput("Plot")),
               tabPanel("State Data",
                        selectInput("stateselect", label = h5("Continuous Variable"),
                                    choices = colnames(GCT_and_Geographies_df[,c(4:33)]), 
                                    selected = "REQFIN_PRO"),
                        plotOutput("statePlot"))
    )),
  # absolutePanel(
  #   id = "controls",
  #   top = 40, left = 40,
  #   selectInput("select", label = h3("Continuous Variable"),
  #               choices = colnames(bird_df))
  # ),
  sidebarPanel(
    # textInput('address', 'Enter address'),
    # actionButton('search', 'Search'),
    # textOutput('result1'),
    # textOutput('result2'),
    selectInput("state", label = h5("Select State"),
                choices = GCT_and_Geographies$STATE),
    htmlOutput("goals")
    
  )
)

## Server -----------

server <- function(input, output, session) {
  output$goals <- renderUI({
    x <- paste0(GCT_and_Geographies %>%
                  as.data.frame() %>%
                  filter(STATE == {input$state}) %>%
                  subset(select = "label") %>%
                  substr(7, nchar(GCT_and_Geographies %>%
                                    as.data.frame() %>%
                                    filter(STATE == {input$state}) %>%
                                    subset(select = "label")) - 2))
    HTML(x)
    })
  output$Plot <- renderPlot(depth_plot(input$select, "trend", var_state = input$state))
  output$statePlot <- renderPlot(state_plot("STUSPS", input$stateselect, input$state))
    output$map <- renderLeaflet({
    nobo_and_gct_leaflet
  })
    observe({
      leafletProxy("map", data = bird_dat) %>%
        clearMarkers() %>%
        clearControls() %>%
      addMarkers(data = bird_dat,
                 icon = symbols_size(basesize = {input$size}),
                 label=paste("2002-2019:","Trend:", 
                             round(bird_dat$trend,digits=2),"%, Abundance: ", 
                             round(bird_dat$abund), sep = ""), 
                 popup=~label, 
                 group="Points") %>%
        
        #Add State Data 
        clearShapes() %>%
        addPolygons(data=GCT_and_Geographies,
                    highlightOptions = highlightOptions(fillColor="green", fillOpacity=.3),
                    weight=1, 
                    fillColor= "white",
                    color = "black",
                    fillOpacity=.05,
                    popup = paste0(GCT_and_Geographies$STATE, " - ", GCT_and_Geographies$REGION), 
                    group="States",
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "11px",
                      direction = "auto")) %>% 
        addPolygons(data = GCT_and_Geographies %>% filter(STATE == {input$state}),
                    fillColor = "purple",
                    fillOpacity = .3,
                    popup = paste0({input$state})
                    ) %>%
        addPolygons(data=transf_natl_PA, 
                    color="Black",
                    fillColor = "mediumpurple2", 
                    highlightOptions = highlightOptions(fillColor = "red", fillOpacity = .7),
                    popup = "Priority Area",
                    opacity = .5,
                    weight =1,
                    group="Priority Counties") %>% 
        
        addLegendSize(values = bird_dat$abund,
                      color = "black",
                      fillColor = "black",
                      title = "Abundance",
                      orientation = "vertical",
                      baseSize = {input$size},
                      shape = "circle") %>%
        addLegend(title = paste("Trend 2002-2019<br>(Avg Annual Change)"), 
                  colors = brewer.pal(9, "RdYlGn"),
                  labels = c("4 Percent Decrease","","","",
                             "No Change","","","","4 Percent Increase"), 
                  position = "topright")
        
    })
    
    # latlon <- reactive({
    #   input$search
    #   geocode(isolate(input$address),
    #           output = "latlona", # this returns a list of 3: latitude, longitude, and address
    #           source = "google")
    # })
    
    # output$result1 <- renderText({
    #   if (!is.na(latlon())) {
    #     paste('Address:', latlon()$address)
    #   } else {
    #     ''
    #   }
    # })
    # 
    # output$result2 <- renderText({
    #   if (!is.na(latlon())) {
    #     paste('Coordinates:', latlon()$lon, latlon()$lat)
    #   } else {
    #     ''
    #   }
    # })
    
    # observe({
    #   leafletProxy("map", data = latlon()) %>%
    #     addMarkers()
    # })
}


## Run App -----------

shinyApp(ui, server)



