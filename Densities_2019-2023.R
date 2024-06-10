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
OS_import <- read.csv("All 19-23 R Data.csv") #Read in ANNUAL Dataset .csv
as.data.frame(unclass(OS_import),stringsAsFactors=TRUE) #makes dataset a dataframe


OS_import<-subset(OS_import, OS_import$Collection.Method == "Extraction") #remove observation data rows
# OS_import <- subset(OS_import, OS_import$Year == "2022") #select a year for analysis


OS_import<- dplyr::rename(OS_import,OS_ID=OS.ID,OS_Name=OS.Name,Site_ID=Site..,Material_Age=Material.Age,SH_mm=SH..mm., Size_Class=Size.Class, Mussel_Cover=Mussel.Cover,Boring_Sponge=Boring.Sponge, Sample_Depth=Sample.Depth)

OS_import$OS_ID<-as.numeric(gsub("OS-","",OS_import$OS_ID)) #remove OS-" from OS_ID
OS_import$SH_mm<-as.numeric(OS_import$SH_mm)

# converts necessary columns to factors
factor_cols <- c("Year","OS_ID","OS_Name","Material","Material.Sub.Category","Size_Class","Strata","Spat.Y.N","Sub.Legal.Y.N","Legal.Y.N")
OS_import[factor_cols] <- lapply(OS_import[factor_cols], factor)

OS_import <- OS_import%>%
  tidyr::drop_na(SH_mm)

#####################################################################################################################


# Create a new dataframe for just oyster density data - keeping only what is needed for calculating density estimates for each site. Keeping SID as the primary key to combine again with environmental data 

oyster_count<-OS_import %>% #cut down dataset to only include what is needed, renaming condensed dataset as "data"
  dplyr::select(Year,OS_ID,OS_Name,Material,Site_ID,SID,SH_mm,Size_Class)%>%
  tidyr::drop_na(SH_mm)



# Break up the oysters into subclasses; creates object for all oyster measurements for legal, sublegal, and spat size classes
legal_count <- filter(oyster_count, SH_mm >75) #exclusive of 75

sublegal_count <- oyster_count%>%
  dplyr::filter(dplyr::between(SH_mm, 26, 75)) #inclusive of 26 and 75

spat_count <- filter(oyster_count, SH_mm <= 25) #inclusive of 25

non_spat_count <- filter(oyster_count, SH_mm >25) #everything above spat size; all that made it past the settlement phase



# Function to calculate oyster density 
oyst_density <- function(x) {
  (x)%>%
    group_by(Year, OS_ID, OS_Name, Site_ID,SID, Material)%>%
    dplyr::summarise(oyster_density =4*n ())
}  


# Create dataframes with the calculated density for each site, and by each size class grouping

all_density <- oyst_density(oyster_count)

non_spat_density <- oyst_density(non_spat_count)

spat_density <- oyst_density(spat_count)

sublegal_density <- oyst_density(sublegal_count)

legal_density <- oyst_density(legal_count)



# Create a dataframe with all the environmental data and Sample ID (SID will be the key for a join)
environ_data <- OS_import %>%
  select(Collection.Method,Sample.Method,Latitude,Longitude,SID,Deployment.Year,Deployment.Month,Material_Age,Oyster.Cover,Mussel_Cover,Algae.Cover,Mussel_Cover,Sedimentation,Boring_Sponge,Strata,Sample_Depth,OS.Depth,Relief,Spat.Y.N,Sub.Legal.Y.N,Legal.Y.N,S.DO,B.DO,S.Sal,B.Sal,S.Temp,B.Temp)
  
# removes duplicate rows, only unique SID remains; dataframe should have the same number of rows as dens_total dataframe
environ_data <- environ_data[!duplicated(environ_data$SID),]




# Build a holistic density dataframe, with an estimate for density at each site for each size class
master_density <- merge(x=all_density, y =legal_density[ ,c("SID","oyster_density")], by = "SID", all.x=TRUE)
master_density <- dplyr::rename(master_density,legal=oyster_density.y,total=oyster_density.x)

master_density <- merge(x=master_density, y =sublegal_density[ ,c("SID","oyster_density")], by = "SID", all.x=TRUE)
master_density <- dplyr::rename(master_density,sublegal=oyster_density)

master_density <- merge(x=master_density, y =spat_density[ ,c("SID","oyster_density")], by = "SID", all.x=TRUE)
master_density <- dplyr::rename(master_density,spat=oyster_density)

master_density <- merge(x=master_density, y =non_spat_density[ ,c("SID","oyster_density")], by = "SID", all.x=TRUE)
master_density <- dplyr::rename(master_density,non_spat=oyster_density)


# convert NAs in the density columns to 0s

master_density <- master_density%>%
  mutate_at(c('legal','sublegal','spat','non_spat'), ~replace_na(.,0))


# Merge the holistic dataframe with the environmental data recorded at each site
complete_density_final <- left_join(master_density,environ_data,by="SID")




# Double check work - make sure dataframe is ideal structure
# Check to make sure material types don't have any spaces or typos ("Marl" and "Marl " will appear the same in excel)
complete_density_final %>%
  group_by(Material) %>%
  tally()


head(complete_density_final)
str(complete_density_final)



#create .csv file with density estimates
write.csv(complete_density_final,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R/2019-2023_oyster_densities.csv", row.names=FALSE)









#### test code ### not in use at the moment###

############################################################################################################

#### test code ### not in use at the moment###



# Material Age

plot(total~Material_Age,data=complete_density_final)

results1=lm(total~Material_Age,data=complete_density_final)
summary(results1)
confint(results1)
# highly statistically significant, nice confidence intervals; need to apply random effect due to sanctuary


results2=lme(total~Material_Age,data=complete_density_final,random=~1|OS_ID)
summary(results2)

anova(results2,results1)
# F-drop test suggests that the linear model is a better fit to the data



# Material Type

plot(total~Material,data=complete_density_final)

results3=lm(total~Material,data=complete_density_final)
summary(results3)
confint(results3)

results4=lme(total~Material,data=complete_density_final,random=~1|OS_ID)
summary(results4)

anova(results4,results3)
# F-drop test suggests that the linear model is a better fit to the data



plot(total~Relief,data=complete_density_final)
plot(total~Strata,data=complete_density_final)

results2=lm(total~Relief+Strata,data=complete_density_final)
summary(results2)
confint(results2)
# large confidence intervals, high p-value -- linear regression is not statistically significant; though the plot looks non-linear


relief_non_spat = lme(legal~Relief,data=complete_density_final,random=(~1|OS_ID))





plot(legal~Material,data=complete_density_final)


plot(legal~Material_Age,data=complete_density_final)








density_final$Relief <- as.numeric(as.character(density_final$Relief))
density_final$Material_Age <- as.numeric(as.character(density_final$Material_Age))

results5=lm(Relief~Material_Age,data=density_final)



results = lm(legal~Material,data=m)
summary(results)

plot(legal~Material,data=m)




results7 = lme(legal~Material,data=m,random=~1|OS_ID)
summary(results7)


library(ggplot2)

ggplot(data=m, aes(x=Material,y=legal)) +
  geom_col() + facet_grid(Year~.)



write.csv(m,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/densities.csv", row.names=FALSE)
















plot(oyst_density~Material,data=dens_total, ylab=expression("Frequency (Oysters per m"^2*")"))

write.csv(dens_total,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/All 19-22 Data/dens_total.csv", row.names=FALSE)


#legal oyster density, join environmental data to each unique sample
dens_legal <- oyster_density(oysters_legal)
dens_legal <- left_join(dens_legal,environ_data_unique,by="SID")

plot(oyst_density~Material,data=dens_legal,ylab=expression("Frequency (Oysters per m"^2*")"))

write.csv(dens_legal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/dens_legal.csv", row.names=FALSE)


#sublegal oyster density, join environmental data to each unique sample
dens_sublegal <- oyster_density(oysters_sublegal)
#dens_sublegal <- left_join(dens_sublegal,environ_data_unique,by="SID")

plot(oyst_density~Material,data=dens_sublegal,ylab=expression("Frequency (Oysters per m"^2*")"))

# write.csv(dens_sublegal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/dens_sublegal.csv", row.names=FALSE)


#spat oyster density, join environmental data to each unique sample
dens_spat <- oyster_density(oysters_spat)
#dens_spat <- left_join(dens_spat,environ_data_unique,by="SID")

plot(oyst_density~Material,data=dens_spat, ylab=expression("Frequency (Oysters per m"^2*")"))

# write.csv(dens_spat,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/dens_spat.csv", row.names=FALSE)










# Power Analysis

# Function to calculate the yearly average density for each site and material type
material_site_density <- function(x) {
  (x)%>%
    group_by(Year,OS_ID, OS_Name, Material)%>%
    dplyr::summarise(n(), Mean= mean(oyst_density), sd=sd(oyst_density))
}


MSD_total <- material_site_density(dens_total)

MSD_legal <- material_site_density(dens_legal)

MSD_sublegal <- material_site_density(dens_sublegal)

MSD_spat <- material_site_density(dens_spat)


write.csv(MSD_total,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/MSD_total.csv", row.names=FALSE)
write.csv(MSD_legal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/MSD_legal.csv", row.names=FALSE)
write.csv(MSD_sublegal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/MSD_sublegal.csv", row.names=FALSE)
write.csv(MSD_spat,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/MSD_spat.csv", row.names=FALSE)



# Function for mean density at each site for each material, averaged across all years **need to look into if age of material has biological significant affect on density*
avg_material_site_density <- function(x) {
  (x)%>%
    group_by(Material)%>%
    dplyr::summarise(n(),Mean= mean(oyst_density), sd=sd(oyst_density))
}


avg_mat_dens_all <- avg_material_site_density(dens_total)

avg_mat_dens_legal <- avg_material_site_density(dens_legal)

avg_mat_dens_sublegal <- avg_material_site_density(dens_sublegal)

avg_mat_dens_spat <- avg_material_site_density(dens_spat)




write.csv(avg_mat_dens_all,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/avg_mat_dens_all.csv", row.names=FALSE)
write.csv(avg_mat_dens_legal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/avg_mat_dens_legal.csv", row.names=FALSE)
write.csv(avg_mat_dens_sublegal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/avg_mat_dens_sublegal.csv", row.names=FALSE)
write.csv(avg_mat_dens_spat,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/Calculations/avg_mat_dens_spat.csv", row.names=FALSE)


#writes dataframes to a .csv file in file pathway

# write.csv([],"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/[].csv", row.names=FALSE)






