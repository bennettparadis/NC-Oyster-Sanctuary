setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R")

library(plyr)
library(dplyr)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(data.table)
library(scales)
library(devtools)
library(car)
library(carData)

# Annual Data Import
OS_import <- read.csv("2023 R Data.csv") #Read in ANNUAL Dataset .csv

#clean dataframe
OS_import<- dplyr::rename(OS_import, Collection_Method=Collection.Method, Material_Age=Material.Age,Oyster_Cover=Oyster.Cover,Mussel_Cover=Mussel.Cover,Boring_Sponge=Boring.Sponge, Sample_Depth=Sample.Depth)

OS_import$OS_ID<-as.numeric(gsub("OS-","",OS_import$OS_ID)) #remove OS-" from OS_ID
OS_import$Material <- factor(OS_import$Material)
OS_import$SH_mm<-as.numeric(OS_import$SH_mm)

OS_import<-OS_import %>% #cut down dataset to only include what is needed, renaming condensed dataset as "data"
  dplyr::select(Year,OS_ID,OS_Name,Material,Collection_Method,Site_ID,SID,Material_Age,SH_mm,Oyster_Cover,Mussel_Cover,Boring_Sponge,Sedimentation,B.Do,B.Temp,Sample_Depth,Relief)


# ISOLATE ENVIRONMENTAL DATA
# Create a dataframe with all the environmental data and Sample ID (SID will be the key for a join)
environ_data <- OS_import %>%
  select(SID,Collection_Method,Material_Age,Oyster_Cover,Mussel_Cover,Boring_Sponge,Sedimentation,B.Do,B.Temp,Sample_Depth,Relief)

# removes duplicate rows, only unique SID remains; dataframe should have the same number of rows as dens_total dataframe
environ_data <- environ_data[!duplicated(environ_data$SID),]


# SPLIT OYSTER DATA
# Separate sample data by method used - extraction or observation
OS_extract<-subset(OS_import, OS_import$Collection_Method == "Extraction") #isolate extraction data rows
OS_extract$SH_mm <- as.numeric((OS_extract$SH_mm))

OS_obs <- subset(OS_import, OS_import$Collection_Method == "Observation") #isolate observation data rows


######################################################################
########       ESTIMATING DENSITIES OF EXCAVATED SAMPLES      ########    
######################################################################

# EXTRACTED DENSITIES - work with extracted dataframe to calculate densities
# Break up the oysters into subclasses; creates DF object for all oyster measurements for legal, sublegal, and spat size classes
oysters_legal <- filter(OS_extract, OS_extract$SH_mm >75) #exclusive of 75
oysters_sublegal <- filter(OS_extract, between(SH_mm, 26, 75)) #inclusive of 26 and 75
oysters_spat <- filter(OS_extract, SH_mm <=25) #inclusive of 25

# Function to calculate oyster density 
  oyster_density <- function(x) {
    (x)%>%
      group_by(Year, OS_ID, OS_Name, Site_ID,SID, Material)%>%
      dplyr::summarise(oyst_density =4*n ())
  }  
 
#Run the various size class data(all, legal, sublegal, spat) through density function, then join to the environmental data.  
#Each of these dataframes now treats each sampled site as an individual point, with the associated environmental data. 
#These can now be used in models to explore relationships between variables that might influence oyster density.
  
#total density across all size classes, join environmental data to each unique sample
dens_total <- oyster_density(OS_extract)
dens_total <- left_join(dens_total,environ_data,by="SID")
#write.csv(dens_total,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/density breakdown/dens_total.csv", row.names=FALSE)

#legal oyster density, merge to dens_total master dataframe
dens_legal <- oyster_density(oysters_legal)
dens_total <- merge(dens_total,dens_legal[, c("SID", "oyst_density")], by="SID", all.x=TRUE)
names(dens_total)[names(dens_total) == "oyst_density.y"] <- "legal_density"
dens_total$legal_density[is.na(dens_total$legal_density)]<-0

#sublegal oyster density, join to master dataframe
dens_sublegal <- oyster_density(oysters_sublegal)
dens_total <- merge(dens_total,dens_sublegal[, c("SID", "oyst_density")], by="SID", all.x=TRUE)
names(dens_total)[names(dens_total) == "oyst_density"] <- "sublegal_density"
dens_total$sublegal_density[is.na(dens_total$sublegal_density)]<-0

#spat oyster density, join environmental data to each unique sample
dens_spat <- oyster_density(oysters_spat)
dens_total <- merge(dens_total,dens_spat[, c("SID", "oyst_density")], by="SID", all.x=TRUE)
names(dens_total)[names(dens_total) == "oyst_density"] <- "spat_density"
names(dens_total)[names(dens_total) == "oyst_density.x"] <- "total_density"
dens_total$spat_density[is.na(dens_total$spat_density)]<-0


#rearrange columns, save to share drive -- this is just the excavated samples
extract_density <- dens_total[, c(1,2,3,4,5,6,9,8,7,18,19,20,10,11,12,13,14,15,16,17)]
#clean up variables in the workspace/global environment
rm(dens_legal,dens_spat,dens_sublegal,dens_total,oysters_legal,oysters_spat,oysters_sublegal)


######################################################################
########     ESTIMATING DENSITIES OF OBSERVATIONAL SAMPLES    ########    
######################################################################

#STEP 1 - Create a basic linear model

#narrow down the master dataset to get just the samples that had a % cover estimate for oysters; only need 3 columns, remove NAs
cover_data <- extract_density%>%
  select(OS_ID, total_density, Oyster_Cover)%>%
  filter(!is.na(Oyster_Cover))

# analyze the relationship between percent cover and density using all of the data points in a basic linear model
plot(total_density~Oyster_Cover, data=cover_data)
results_norm=lm(total_density~Oyster_Cover, data=cover_data)
summary(results_norm)
abline(results_norm)

#STEP 2 - Assess the assumptions of linearity in the basic model

#Residuals vs Fitted Values Plot: This plot helps you check for linearity and homoscedasticity (constant variance). 
#If the points are randomly scattered around the horizontal line at 0, it suggests that the assumption of constant variance is reasonable.
plot(fitted(results_norm), resid(results_norm), xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")
#looks like a violation of heteroscedasticity --> the points move further out from the line as you move down the x-axis
#this makes sense though, as it is a common occurrence when dealing with counts and biological data

#Normal Q-Q Plot: This plot helps you assess the normality of the residuals in a similar way. 
#If the points fall approximately along the diagonal line, it suggests that the residuals are normally distributed.
#Try to look for an S-curve --> this would be a warning about normal distribution
qqnorm(resid(results_norm))
qqline(resid(results_norm))
#for the most part, looks like residuals fall along the diagonal. Eight points fall off above the line at the end

#check's if the data/residuals are normally distributed -- if the p-value is greater than 0.05, the null hyp of normal distribution is accepted
shapiro.test(residuals(results_norm))
# p=0.00123 --> non-normally distributed residuals


#STEP 3 - address heteroscedasticity by applying a transformation or a new model
#Generalized linear model - Poisson distribution model 
#results = glm(Y~X1+X2, data=datum, family = poisson)
library(nlme)
plot(total_density~Oyster_Cover, data=cover_data)
results_glm <- glm(total_density ~ Oyster_Cover, data=cover_data, family = poisson)
abline(results_glm)
summary(results_glm)
#I don't think this is the answer -- line doesn't quite fit and the estimates of effect (Betas) are very small even after exp(B) calculation


#Transforming the response variable to see if this addresses the violation of homoscedasticity 
cover_data$log_total_density <- log(cover_data$total_density)
cover_data$sqrt_total_density <- sqrt(cover_data$total_density)


#square root transformed
plot(sqrt_total_density~Oyster_Cover, data=cover_data)
results_sqrt=lm(sqrt_total_density~Oyster_Cover, data=cover_data)
summary(results_sqrt)
# R-squared -- 0.5673, p = 9.09 e-15
abline(results_sqrt)


#Residuals vs Fitted Values Plot: This plot helps you check for linearity and homoscedasticity (constant variance). 
#If the points are randomly scattered around the horizontal line at 0, it suggests that the assumption of constant variance is reasonable.
plot(fitted(results_sqrt), resid(results_sqrt), xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")
#this looks a little better than before

#Normal Q-Q Plot: This plot helps you assess the normality of the residuals in a similar way. 
#If the points fall approximately along the diagonal line, it suggests that the residuals are normally distributed.
#Try to look for an S-curve --> this would be a warning about normal distribution
qqnorm(resid(results_sqrt))
qqline(resid(results_sqrt))
#for the most part, looks like residuals fall along the diagonal. Eight points fall off above the line at the end

#check's if the data/residuals are normally distributed -- if the p-value is greater than 0.05, the null hyp of normal distribution is accepted
shapiro.test(residuals(results_sqrt))
# p=0.3546 --> normally distributed residuals, success.

#FINAL APPROACH
#the square root transformed model has a good R-squared value, and a significant p-value
#the transformed data does not appear to violate any assumptions of linearity (heteroscedasticity, normally distributed residuals)


#STEP 4 - Interpolation
#create a dataframe that uses a basic linear model with transformed data. The columns are then fit, prediction, and upper/lower confidence intervals
CIs <- data.frame(predict(results_sqrt, interval = "confidence"))
#add the original X values (oyster cover) to the CIs data
CIs$Oyster_Cover <- cover_data$Oyster_Cover
plot(sqrt_total_density~Oyster_Cover, data=cover_data)
abline(results_sqrt)
lines(CIs$Oyster_Cover, CIs$lwr, lty = 2)
lines(CIs$Oyster_Cover, CIs$upr, lty = 2)

#grab the coefficients and SE from the model - these will be used to estimate densities based on percent coverages
coef <- coef(results_sqrt)
se <- summary(results_sqrt)$coef[, "Std. Error"]

#using the model, calculate predicted densities using the percent cover recorded at observational sites
#add columns and remove unnecessary ones to format this dataframe to match the structure of the excavated density df
OS_obs <- OS_obs %>%
  add_column(legal_density = NA,
             sublegal_density = NA,
             spat_density = NA)%>%
  mutate(predicted_density = round((Oyster_Cover * coef[2] + coef[1])^2), digits = 0)%>%
  select(SID, Year, OS_ID, OS_Name, Site_ID, Material, Material_Age, Collection_Method, predicted_density,legal_density,sublegal_density,spat_density, Oyster_Cover, Mussel_Cover, Boring_Sponge, Sedimentation, B.Do, B.Temp, Sample_Depth, Relief)%>%
  rename(total_density = predicted_density)


#STEP 5 - Merge and save a final density estimate file
#merge the two density dataframes into a single "master", then save to a .csv file
all_density<- rbind(OS_obs, extract_density)
master_density <- merge(all_density, environ_data, by = "SID")

write.csv(master_density,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/2023_density_estimates.csv", row.names=FALSE)