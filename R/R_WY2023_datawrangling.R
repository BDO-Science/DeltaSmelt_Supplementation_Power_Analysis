# Purpose: Prepare WY2023 Delta Smelt catch data for analysis
# Author: Brian Mahardja
# Date: 2023-09-06

# Set working directory
root <- "C:/Users/bmahardja/Documents/GitHub/DeltaSmelt_Supplementation_Power_Analysis"
setwd(root)

data_root<-file.path(root,"data")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

# Load necessary packages
library(tidyverse)
library(lme4)
library(deltafish)
library(readxl)
library(boot)

# Load Delta Smelt catch data from Catarina Pien for WY2023 so far
smelt_data <-read.csv(file.path(data_root,"WY2023_deltasmelt_catch.csv")) %>% filter(Marked=="marked") %>%  
  mutate(Survey = str_replace_all(Survey, "CVP Salvage", "CVP")) %>%
  mutate(Survey = str_replace_all(Survey, "SWP Salvage", "SWP")) %>% mutate(StationCode=ifelse(Survey %in% c("CVP","SWP"),Survey,StationCode))

# Load hatchery fish release information from Catarina Pien
release_info <- read.csv(file.path(data_root,"ReleaseInfo.csv")) %>% mutate(FirstDayRelease=as.Date(FirstDayRelease, "%m/%d/%Y"))
# Change the ad-clip notation to match smelt data
release_info$Tag <- str_replace(release_info$Tag ,'Ad-clipped','ad')

# Add release information data into Delta Smelt catch data
smelt_data_join <- smelt_data %>% left_join(release_info)  %>% mutate(Date=as.Date(Date, "%m/%d/%Y"),FirstDayRelease=as.Date(FirstDayRelease, "%m/%d/%Y")) %>%
  mutate(DaySinceRelease=as.numeric(Date-FirstDayRelease)) %>%  mutate(Release.Method = str_replace_all(Release.Method, "Trailer Release", "Hard Release")) %>%
  #Remove Chipps and Broodstock data because they won't add much to the analysis
  filter(!(Survey %in% c("Broodstock","Chipps"))) %>% rename(SampleDate=Date)


# Load WY 2023 EDSM xlsx data sent by Claudia on Sept 8 2023
edsm_data <- read_excel(file.path(data_root,"22_23Phase1_Brian.xlsx")) %>% mutate(Count=ifelse(is.na(Count),0,Count)) %>%
  #Remove gear condition code 4
  filter(GearConditionCode != 4) %>% mutate(TowNumber=ifelse(GearConditionCode==9,1,TowNumber)) %>% group_by(StationCode,TowNumber,SampleDate) %>%
  summarise(EDSM_Count=sum(Count)) %>% mutate(Survey="EDSM",StationCode=as.character(StationCode))

# Load WY 2022-2023 SKT data sent by Vanessa Mora on Sept 6 2023
skt_data <- read_excel(file.path(data_root,"SKT 2022-2023 Data.xlsx")) %>% rename(SKT_Catch = 'DS Catch') %>%
  #Use just WY 2023 data for year 2 dataset
  filter(Year==2023) %>%
  dplyr::select(SampleDate,SKT_Catch,StationCode,SurveyNumber) %>% mutate(Survey="SKT",StationCode=as.character(StationCode)) 

# Load csv salvage data grabbed from DFW website
# https://apps.wildlife.ca.gov/Salvage/Chart/AcrefeetSalvage?Adipose=All&SampMethod=Both&orgCode=26&orgDes=Delta%20Smelt&endDate=05%2F01%2F2023%2000%3A00%3A00&StartDate=11%2F01%2F2021%2000%3A00%3A00&ShowValue=False
salvage_data <- read.csv(file.path(data_root,"SalvageExport.csv")) %>% rename(SampleDate=Sample.Date,Survey=Facility) %>%
  mutate(SampleDate=as.Date(SampleDate, "%Y-%m-%d"),StationCode=Survey) %>% dplyr::select(-Species) %>%
  #There were NS data with 0 acre feet pumped for SWP, so remove because we assume no sampling was done
  filter(Salvage != "NS") %>% mutate(Salvage=as.numeric(Salvage)) %>% filter(SampleDate>="2022-11-01")

# Join all survey data
allsurvey_data <- bind_rows(edsm_data,salvage_data,skt_data) %>% mutate(SampleDate = as.Date(SampleDate))


temp_date_data <- release_info %>% dplyr::select(Tag,FirstDayRelease,Total,Release.Method) %>%
  mutate(Release.Method = ifelse(Release.Method=="Trailer Release","Hard Release",Release.Method))

# Summarize smelt_data_join data frame for instances where multiple fish were caught in the same sampling event
temp_catch_data <- smelt_data_join %>% group_by(SampleDate,Survey,StationCode,Tag,TowNumber) %>% summarise(Count=sum(Count)) %>%
  # Convert "Count" column to just 1s and 0s
  mutate(Count=ifelse(Count>1,1,Count)) %>%
  # Also convert CVP and SWP data StationCode to match with salvage data
  mutate(StationCode = ifelse(Survey %in% c("CVP","SWP"), Survey, StationCode))

list_data<-rep(list(allsurvey_data), 6)
names(list_data)<- release_info$Tag

for (i in release_info$Tag) {
  list_data[[i]] <- list_data[[i]] %>% mutate(Tag=i) %>% left_join(temp_date_data) %>%
    left_join(temp_catch_data) %>% mutate(Count=ifelse(is.na(Count),0,Count)) %>%
    #Remove sampling events before release date
    filter(SampleDate>FirstDayRelease) %>% mutate(DaysSinceRelease=as.numeric(SampleDate-FirstDayRelease))
  
}

sum(list_data[["LOA"]]$Count)
sum(list_data[["LRA"]]$Count)

# Combine datasets from all the release groups
data_WY2023 <- data.frame(do.call(rbind, list_data)) %>% mutate(Survey=as.factor(Survey),Tag=as.factor(Tag),Release.Method=as.factor(Release.Method))

distance_data <- read.csv(file.path(output_root,"WY2023_distance_from_release_covariate.csv")) %>% select(StationCode,Tag,dist_km)

# Add in distance from release site as data
data_WY2023 <- left_join(data_WY2023,distance_data,multiple="all")

# Remove infinite distance values from a couple of stations: 23-26-SM01, 23-39-SD07
data_WY2023 <- data_WY2023 %>% filter(dist_km!="Inf")

# Export dataset out
write.csv(data_WY2023,file=file.path(output_root,"WY2023_catch_data.csv"),row.names = F)
