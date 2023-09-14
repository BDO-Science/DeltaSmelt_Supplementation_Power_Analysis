# Purpose: Prepare swim distance data for analysis
# Author: Brian Mahardja
# Date: 2023-09-12

# Set working directory
root <- "C:/Users/bmahardja/Documents/GitHub/DeltaSmelt_Supplementation_Power_Analysis"
setwd(root)

data_root<-file.path(root,"data")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

# Load Package Libraries
library(gdistance)
library(raster)
library(sf)
library(sp)
library(rgdal)
library(tidyverse)
library(readxl)
library(ggspatial)
library(lubridate)
library(viridis)
library(readr)
library(rgeos)
library(deltafish)

# Bay Delta Raster TIP with cross channel closure
#TIF raster file was made in Arc GIS Pro from the bay delta shapefile with a 50 meter buffer. 
#TIF stored the associated CRS information. PNG did not.  
#Tile size was set at 50m x 50m to try and keep the file size and processing time manageable. 
#Conversion of the shapefile to a raster in R did not work properly. It merged across sloughs in the Delta resulting in one giant polygon of water. 
raster_baydelta<-raster(file.path(data_root,"baydelta.tif"))
raster_baydelta #Check out resolution information and CRS = 26910 (NAD 1983 UTM Zone 10)
raster_baydelta[is.na(raster_baydelta)] <- 0  # code land as 0, meaning no passage is possible
plot(raster_baydelta) #Check Raster file to make sure water ways look okay

# Read the pre-processed raster file since the conversion takes several minutes
raster_baydelta_tr <- readRDS(file.path(data_root,"raster_baydelta_tr.rds"))

# Load WY 2023 EDSM xlsx data sent by Claudia on Sept 8 2023 and grab station coordinates
edsm_stations <- read_excel(file.path(data_root,"22_23Phase1_Brian.xlsx")) %>% mutate(Count=ifelse(is.na(Count),0,Count)) %>%
  #Remove gear condition code 4
  filter(GearConditionCode != 4) %>% mutate(TowNumber=ifelse(GearConditionCode==9,1,TowNumber)) %>%
  mutate(Survey="EDSM",StationCode=as.character(StationCode)) %>%
  #To reduce calculation time, we're going to combine lat and long to each station
  group_by(StationCode,Survey) %>% summarise(Latitude=median(LatitudeStart),Longitude=median(LongitudeStart))

# Compile SKT tation Coordinates
skt_stations <- read_csv(file.path(data_root,"SKTstations.csv")) %>%
  dplyr::select(-1, -Description) %>%
  mutate(Longitude = Longitude * -1) %>% rename(StationCode=Station) %>% mutate(StationCode=as.character(StationCode))
  

# Compile coordinates for Tracy and Skinner facilities
# TFCF: 37.815176 -121.560709 (WGS84)
# Skinner: 37.82968216253291, -121.55652895353651

salvage_stations <- data.frame(Survey = c("CVP", "SWP"),
                          StationCode = c("CVP","SWP"),
                          Latitude = c(37.815176,37.82968216253291),
                          Longitude = c(-121.560709, -121.55652895353651))

# Broodstock fish: 38° 3.816’ -121° 47.915’ (from Luke through Cat)

broodstock_station  <- data.frame(Survey  = "Broodstock",
                         StationCode = "Broodstock",
                             Latitude = 38.063600,
                             Longitude = -121.798583) 

chipps_station  <- data.frame(Survey  = "Chipps",
                      StationCode = "Chipps",
                         Latitude = 38.04598,
                         Longitude = -121.909917) 


# Combine datasets into 1 and convert to SF
station_all <- bind_rows(edsm_stations, skt_stations, salvage_stations,broodstock_station,chipps_station)

station_all_sf <- station_all %>% filter(Longitude > -500) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(raster_baydelta))
plot(station_all_sf)

# Add release sites
releases <- read_excel(file.path(data_root,"release_locations_dates_2023.xlsx")) %>% rename(Tag=Mark)

releases$Tag <- str_replace(releases$Tag ,'AdClipped','ad')

releases_sf <- releases %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(raster_baydelta))


# Calculate distances with least cost analysis
dist_2023 <- costDistance(raster_baydelta_tr,
                          fromCoords = as(as_Spatial(station_all_sf), "SpatialPoints"),
                          toCoords = as(as_Spatial(releases_sf), "SpatialPoints"))
rownames(dist_2023)<-as.vector(station_all_sf$StationCode)
colnames(dist_2023)<-as.vector(releases_sf$Tag)

# Turn back to dataframe
Least_Cost_data_2023 <-data.frame(dist_meters = as.vector(as.matrix(dist_2023)),
                                  comparison = as.vector(outer(Y=colnames(as.matrix(dist_2023)) ,
                                                               X=rownames(as.matrix(dist_2023)) ,
                                                               paste, sep="xxx"))) %>%
  mutate(StationCode=sub("xxx.*","", comparison),Tag=sub(".*xxx","", comparison),dist_km=dist_meters/1000) %>%
  dplyr::select(StationCode,Tag,dist_km)

# Export dataset out
write.csv(Least_Cost_data_2023,file=file.path(output_root,"WY2023_distance_from_release_covariate.csv"),row.names = F)


