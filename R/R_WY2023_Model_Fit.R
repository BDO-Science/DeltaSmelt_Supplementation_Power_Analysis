# Purpose: Fit logistic regression model to WY2023 data from released Delta Smelt
# Author: Brian Mahardja
# Date: 2023-09-13

# Set working directory
root <- "C:/Users/bmahardja/Documents/GitHub/DeltaSmelt_Supplementation_Power_Analysis"
setwd(root)

data_root<-file.path(root,"data")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")

# Load necessary packages
library(tidyverse)
library(lme4)
library(AICcmodavg)


# Load pre-processed Delta Smelt catch data for WY2023
data_WY2023 <- read.csv(file.path(output_root,"WY2023_catch_data.csv")) %>%
  mutate(Release.Method=as.factor(Release.Method),StationCode=as.factor(StationCode),Survey=as.factor(Survey))

data_WY2023$Survey <- relevel(data_WY2023$Survey , "EDSM")

str(data_WY2023)
# Model selection 
Cand.set.WY23 <- list( )

Cand.set.WY23[[1]] <-  glm(Count~NULL, data=data_WY2023, family="binomial")
Cand.set.WY23[[2]] <-  glm(Count~DaysSinceRelease, data=data_WY2023, family="binomial")
Cand.set.WY23[[3]] <-  glm(Count~dist_km, data=data_WY2023, family="binomial")
Cand.set.WY23[[4]] <-  glm(Count~Survey, data=data_WY2023, family="binomial")
Cand.set.WY23[[5]] <-  glm(Count~Total, data=data_WY2023, family="binomial")
Cand.set.WY23[[6]] <-  glm(Count~Release.Method, data=data_WY2023, family="binomial")
Cand.set.WY23[[7]] <-  glm(Count~DaysSinceRelease+dist_km, data=data_WY2023, family="binomial")
Cand.set.WY23[[8]] <-  glm(Count~DaysSinceRelease+Survey, data=data_WY2023, family="binomial")
Cand.set.WY23[[9]] <-  glm(Count~DaysSinceRelease+Total, data=data_WY2023, family="binomial")
Cand.set.WY23[[10]] <-  glm(Count~DaysSinceRelease+Release.Method, data=data_WY2023, family="binomial")
Cand.set.WY23[[11]] <-  glm(Count~dist_km+Survey, data=data_WY2023, family="binomial")
Cand.set.WY23[[12]] <-  glm(Count~dist_km+Total, data=data_WY2023, family="binomial")
Cand.set.WY23[[13]] <-  glm(Count~dist_km+Release.Method, data=data_WY2023, family="binomial")
Cand.set.WY23[[14]] <-  glm(Count~Survey+Total, data=data_WY2023, family="binomial")
Cand.set.WY23[[15]] <-  glm(Count~Survey+Release.Method, data=data_WY2023, family="binomial")
Cand.set.WY23[[16]] <-  glm(Count~Total+Release.Method, data=data_WY2023, family="binomial")
# Start 3 parameters here
Cand.set.WY23[[17]] <-  glm(Count~DaysSinceRelease+Survey+dist_km, data=data_WY2023, family="binomial")
Cand.set.WY23[[18]] <-  glm(Count~DaysSinceRelease+Survey+Total, data=data_WY2023, family="binomial")
Cand.set.WY23[[19]] <-  glm(Count~DaysSinceRelease+Survey+Release.Method, data=data_WY2023, family="binomial")
# Start 4 parameters here
Cand.set.WY23[[20]] <-  glm(Count~DaysSinceRelease+Survey+dist_km+Total, data=data_WY2023, family="binomial")
Cand.set.WY23[[21]] <-  glm(Count~DaysSinceRelease+Survey+dist_km+Total+Release.Method, data=data_WY2023, family="binomial")


##create a vector of names to trace back models in set
Modnames <- paste("mod","DeltaSmelt", 1:length(Cand.set.WY23), sep = "_")

##generate AICc table
aictab(cand.set = Cand.set.WY23, modnames = Modnames, sort = TRUE)

summary(Cand.set.WY23[[8]])

summary(Cand.set.WY23[[17]])
summary(Cand.set.WY23[[20]])
