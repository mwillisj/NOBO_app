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



options(scipen=999)

#loading counties

natl_priority_map <- st_read("NOBO_Boundary_Aug2021_Dissolve_ST_Draft", 
                             layer="NOBO_Boundary_Aug2021_Dissolve_ST_Draft")

transf_natl_PA <- natl_priority_map


##Loading BIRD DATA RDS
bird_data <- readRDS("NOBO_route_level_trends.rds")

bird_data <- st_as_sf(bird_data)


## Converting Projection
bird_dat <- bird_data %>%
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



# Creating Label for NOBO Trend Data
bird_dat$label <- 
  paste("NOBO Trend Data","<br>", "Abundance:", round(bird_dat$abund, digits=3),"<br>","Trend:", round(bird_dat$trend, digits=3),"</b>")%>%
  lapply(htmltools::HTML)



# Datasets ---------------------

# temp_priority_data <- natl_priority_map %>%
#   st_as_sf() %>%
#   st_transform(crs = "WGS84")

sf::sf_use_s2(F)

# temp_priority_data$geometry <- temp_priority_data$geometry %>%
#   s2::s2_rebuild() %>%
#   sf::st_as_sfc()

bird_df <- bird_dat
bird_df <- bird_df %>%
  st_as_sf() %>%
  st_transform(crs = "WGS84") %>%
  st_join(y = natl_priority_map %>%
            st_as_sf() %>%
            st_transform(crs = "WGS84")) %>%
  rename(priority_area = STATE) %>%
  mutate(priority_area = case_when(is.na(priority_area) == F ~ "Priority Area",
                                   is.na(priority_area) == T ~ "Non-Priority Area")) %>%
  as.data.frame() %>%
  mutate(STATE = substr(bird_df$strat, 4, nchar(bird_df$strat) - 3),
         REGION = ifelse(STATE %in% c("IL",
                                      "IN",
                                      "IA",
                                      "KS",
                                      "MN",
                                      "MO",
                                      "NE",
                                      "TX"), "Central", 
                         ifelse(STATE %in% c("DE",
                                             "MD",
                                             "NJ",
                                             "OH",
                                             "WV"), "Northeast", "Southeast")))

# gct_columns <- as.data.frame(colnames(GCT_and_Geographies))
# gct_columns
# 
# GCT_and_Geographies_clean <- GCT_and_Geographies %>%
#   mutate()


# create table of summary stats between priority and non-priority areas
# bird_stats <- cbind(rbind("Non-Priority","Priority"),
#                     rbind(bird_df %>%
#                             group_by(priority_area) %>%
#                             summarise(avg_trend = mean(trend),
#                                       med_trend = median(trend),
#                                       avg_abund = mean(abund),
#                                       med_abund = median(abund),
#                                       avg_prec_i = mean(prec_i),
#                                       med_prec_i = median(prec_i)))
#                     )


saveRDS(bird_df, file = "forapp.rds")
saveRDS(bird_dat, file = "bird_dat.rds")
