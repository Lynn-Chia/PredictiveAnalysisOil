setwd("C:/Users/Xiao Lian/OneDrive - Nanyang Technological University/NBS Y2S1/BC2406 Analytics 1")
library(data.table)
library(reshape2)
library(ggplot2)
library(corrplot)
library(car)
library(caTools)
library(rpart)
library(rpart.plot) 
library(dplyr)
library(nnet)


oilspill <- fread("Oilspill.csv")
options(scipen = 999, digits = 10)
summary(oilspill)

#shutdown time
a <- as.POSIXlt(oilspill$`Shutdown Date/Time`, format="%m/%d/%Y %H:%M")
b<- as.POSIXlt(oilspill$`Restart Date/Time`, format="%m/%d/%Y %H:%M")
time_diff <- difftime(b,a, units="hours")
time_diff
hours_diff <- as.numeric(time_diff)
hours_diff <- round(hours_diff,2)
oilspill$Shutdown_hours <- hours_diff
oilspill[is.na(`Shutdown_hours`), `Shutdown_hours` := 0]

#
oilspill$`Barrel loss (%)` <- round(oilspill$`Net Loss (Barrels)`/oilspill$`Unintentional Release (Barrels)` *100,2)
which(is.na(oilspill), arr.ind = TRUE)
oilspill[is.na(`Barrel loss (%)`), `Barrel loss (%)` := 0]


oilspill <- oilspill[,c("Accident Year", "Accident Date/Time", "Cause Category", "Cause Subcategory", "Net Loss (Barrels)", 
                "Barrel loss (%)", "Pipeline Location", "Pipeline Type", "Liquid Type", "Liquid Ignition", 
                "Liquid Explosion", "Pipeline Shutdown", "Shutdown_hours",
                "Property Damage Costs", "Lost Commodity Costs", 
                "Public/Private Property Damage Costs", "Emergency Response Costs", 
                "Environmental Remediation Costs", "Other Costs", "All Costs")]

sum(is.na(oilspill)) #51 NAs
which(is.na(oilspill), arr.ind = TRUE) #NA in column 13-18, which are all about costs -> replace with 0
oilspill[is.na(oilspill)] <- 0


oilspill <- oilspill %>% mutate_if(is.character, list(~na_if(.,"")))

oilspill$`Cause Category` <- factor(oilspill$`Cause Category`)
oilspill$`Cause Subcategory` <- factor(oilspill$`Cause Subcategory`)
oilspill$`Pipeline Shutdown` <- factor(oilspill$`Pipeline Shutdown`)
oilspill$`Pipeline Location` <- factor(oilspill$`Pipeline Location`)
oilspill$`Pipeline Type` <- factor(oilspill$`Pipeline Type`)
oilspill$`Liquid Type` <- factor(oilspill$`Liquid Type`)
oilspill$`Liquid Ignition` <- factor(oilspill$`Liquid Ignition`) 
oilspill$`Liquid Explosion` <- factor(oilspill$`Liquid Explosion`)
str(oilspill)


write.csv(oilspill, file = "oilspill_cleaned.csv", row.names = F)

