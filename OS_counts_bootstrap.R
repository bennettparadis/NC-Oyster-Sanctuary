setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R")


library(plyr)
library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(data.table)
library(scales)
library(devtools)
library(car)
library(carData)
library(nlme)
library(lme4)

# Data Import - formatting into dataframe with factors and numerical values 
OS_import <- read.csv("All 19-22 R Data.csv") #Read in ANNUAL Dataset .csv
as.data.frame(unclass(OS_import),stringsAsFactors=TRUE) #makes dataset a dataframe


OS_import<-subset(OS_import, OS_import$Collection.Method != "Non-Extracted") #remove observation data rows
# OS_import <- subset(OS_import, OS_import$Year == "2022") #select a year for analysis


OS_import<- dplyr::rename(OS_import,OS_ID=OS.ID,OS_Name=OS.Name,Site_ID=Site..,Material_Age=Material.Age,SH_mm=SH..mm., Size_Class=Size.Class, Mussel_Cover=Mussel.Cover,Boring_Sponge=Boring.Sponge, Sample_Depth=Sample.Depth)

OS_import$OS_ID<-as.numeric(gsub("OS-","",OS_import$OS_ID)) #remove OS-" from OS_ID
OS_import$SH_mm<-as.numeric(OS_import$SH_mm)

# converts necessary columns to factors
factor_cols <- c("Year","OS_ID","OS_Name","Material","Material.Sub.Category","Size_Class","Strata","Spat.Y.N","Sub.Legal.Y.N","Legal.Y.N")
OS_import[factor_cols] <- lapply(OS_import[factor_cols], factor)

OS_import <- OS_import%>%
  tidyr::drop_na(SH_mm)

  
length(unique(OS_import$SID))


size_cohorts <- c("Legal","Sub-Legal","Spat")


OS_all_counts <- OS_import%>%
  group_by(Year,OS_Name,Site_ID,SID,Material_Age)%>%
  summarise(total_count=n())%>%
  arrange(desc(Site_ID))


OS_legal_counts <- OS_import%>%
  group_by(Year,OS_Name,Site_ID,SID,Material_Age)%>%
  filter(Size_Class=='Legal')%>%
  summarise(legal_count=n())


OS_non_spat_counts <- OS_import%>%
  group_by(Year,OS_Name,Site_ID,SID,Material_Age)%>%
  filter(Size_Class!='Spat')%>%
  summarise(non_spat_count=n())

OS_spat_counts <- OS_import%>%
  group_by(Year,OS_Name,Site_ID,SID,Material_Age)%>%
  filter(Size_Class=='Spat')%>%
  summarise(spat_count=n())


write.csv(OS_all_counts,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R/19-22_oyster_counts.csv", row.names=FALSE)




head(OS_all_counts)
plot(total_count~Material_Age,dat=OS_all_counts)

results = lm(total_count~Material_Age,data=OS_all_counts)



master_counts <- merge()








