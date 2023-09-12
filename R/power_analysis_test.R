# Purpose: Run power analysis for Delta Smelt Release Strategy study (Soft vs. Hard)
#
# Author: Brian Mahardja
# Date: 2022-04-21

# Set working directory
setwd("D:/Projects/Nick's Power Analysis")

# Load necessary packages
library(simr)
library(tidyverse)
library(lme4)

# Load Delta Smelt catch data from Catarina Pien for WY2023 so far
smelt_data <-read.csv("WY2023_deltasmelt_catch.csv") %>% filter(Marked=="marked")

# Load hatchery fish release information from Catarina Pien
release_info <- read.csv("ReleaseInfo.csv")

# Add release information data into Delta Smelt catch data
smelt_data_join <- smelt_data %>% left_join(release_info) %>% filter(!is.na(Release.Method)) %>% mutate(Date=as.Date(Date, "%m/%d/%Y"),FirstDayRelease=as.Date(FirstDayRelease, "%m/%d/%Y")) %>%
  mutate(DaySinceRelease=as.numeric(Date-FirstDayRelease))

# Keep just the EDSM data for the sake of simplicity
smelt_data_join <- smelt_data_join %>% filter(Survey=="EDSM") %>% group_by(Date, StationCode,Stratum,Tag,Release.Method,FirstDayRelease) %>%
  # Keep count as 1 for the same station because we're just doing presence/absence
  summarise(Count=mean(Count),DaySinceRelease=mean(DaySinceRelease))

# Load csv data converted from EDSM report from April 19th 2023 as a proxy for sampling effort
edsm_data<-read.csv("EDSM_Phase1_example.csv")
# Summarize data to get actual sampling events (date and station) (ignoring number of tows)
edsm_data_sum<- edsm_data %>% filter(NumberTows>0) %>% group_by(Date, StationCode,Stratum) %>% 
  summarise(TowDuration=mean(as.numeric(TowDuration),na.rm=T)) %>% mutate(Count=0,Date=as.Date(Date, "%m/%d/%Y")) %>%
  # Remove dates already in smelt_data_join
  filter(!StationCode %in% (smelt_data_join$StationCode))
# Note that there was no event when hard and soft release were captured at the same time

# Create data for soft release
soft_release <- full_join(smelt_data_join,edsm_data_sum %>% select(Date,Stratum,Count)) %>% mutate(Count=ifelse(Release.Method=="Soft Release",1,0)) %>%
  mutate(Tag = "RGP",Release.Method="Soft Release", FirstDayRelease= as.Date("2023-01-24")) %>% mutate(Count=ifelse(is.na(Count),0,Count))

# Create data for hard release
hard_release <- full_join(smelt_data_join,edsm_data_sum %>% select(Date,Stratum,Count)) %>% mutate(Count=ifelse(Release.Method=="Hard Release",1,0)) %>%
  mutate(Tag = "LOA",Release.Method="Hard Release", FirstDayRelease= as.Date("2023-01-25")) %>% mutate(Count=ifelse(is.na(Count),0,Count))

# For the data subsets above, I only used ship channel releases because the model can't estimate 0 recaptures from the Rio Vista soft release.
# It is also so that we are comparing apples to apples

# Combine the 2 datasets for analysis
# Also added YearTest variable to extend the number of years
combined_data<- bind_rows(soft_release,hard_release) %>% mutate(Release.Method=as.factor(Release.Method),
                                                                DaySinceRelease=as.numeric(Date-FirstDayRelease)) %>%
  filter(DaySinceRelease>0) %>% mutate(YearTest=1,Stratum=as.factor(Stratum))

summary(combined_data$Release.Method)
# Create the model from the data
# I expected that the Release Method will only affect the intercept because it is assumed that survival after the first week would be similar
# I included day since release to account for the expectation that there will be less fish recaptured as time goes on
# Lastly, I added random effect for region/strata
test_model<- glmer(Count ~ Release.Method + DaySinceRelease+ (1|Stratum), data = combined_data, family = "binomial")

# The results from the ship channel indicate that Soft Release is worse with a coefficient of ~-0.2
# Release.MethodSoft Release  -0.2001     0.5620  -0.356  0.72186    
summary(test_model)

# Since this is log-odds, we have to convert it
exp(-0.2)
# So this means that roughly, there was 20% higher recap rate for hard release. But this parameter is not significant at alpha 0.05 

# Because the expectation is that Soft Release is better, I applied the opposite to the model
# so that soft release is somehow 20% better than hard release
fixef(test_model)["Release.MethodSoft Release"] <- 0.2

# Run the power simulation to see that if this is the truth, how often we would observe a p value of <0.05
powerSim(test_model, nsim=200,seed=123)
# 7.00% ( 3.88, 11.47)
# Time elapsed: 0 h 2 m 36 s

# Run the simulation to see if we do a second year, what our power would be
test_model2 <- extend(test_model, along="YearTest", n=2)
powerSim(test_model2, nsim=200,seed=123)
# 11.00% ( 7.02, 16.18)
# Time elapsed: 0 h 4 m 9 s

# Run the simulation to see if we do this for 10 years (one paired release per year), what our power would be
test_model3 <- extend(test_model, along="YearTest", n=10)
powerSim(test_model3, nsim=200,seed=123)
#35.50% (28.88, 42.56)
# Time elapsed: 0 h 16 m 11 ss


#Check that the data looks right
data_model_check<-getData(test_model3)
