setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R")

library(dplyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(tidyr)
library(nlme)
library(RColorBrewer)
#library(lme4)
#library(splines)
#library(carData)
#library(av)
#library(cowplot)
#library(gapminder)
# library(data.table)
# library(scales)
# library(devtools)
# library(car)
# library(carData)

density_import = read.csv("2023_density_estimates.csv")
head(density_import)
str(density_import)

# Create density_table dataframe
density_table <- density_import %>%
  dplyr::select(OS_ID, OS_Name, Site_ID, total_density) %>%
  group_by(OS_Name) %>%
  summarise(Mean = mean(total_density), SD = sd(total_density)) %>%
  distinct(OS_Name, Mean, SD)

# Create the bar plot
p <- ggplot(density_table, aes(x = OS_Name, y = Mean)) + 
  geom_bar(stat = "identity", fill = "light grey") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = .2,
                position = position_dodge(.9)) +
  theme_classic() +
  labs(x = NULL, y = expression("Oyster Count (per m"^2*")")) +
  ggtitle("2023 Oyster Sanctuary Densities") +
  theme(plot.title = element_text(hjust = 0.5))

p



#Compare material performance

#change Material variable to a factor for categorical analysis
density_import$Material <- as.factor(density_import$Material)

#Total density - material analysis
# Statistics - comparing material performance for total oyster density
plot(total_density~Material,data=density_import, ylab=expression("Total Frequency (Oysters per m"^2*")"))

#Nice plot
box_all <- ggplot(density_import, aes(x=Material, y = total_density))+
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#56B4E9",
               color = "black",
               alpha = 0.5,
               outlier.color = "tomato1",
               outlier.size = 2)+
  labs(x = "Material Type", y = expression("Oyster Density (per m"^2*")"), title = "Oyster Sanctuary Material Performance")+
  theme(axis.title.x = element_text(size = 16, margin = margin(t = 20)),  # Add top margin to x-axis label
         axis.title.y = element_text(size = 16, margin = margin(r = 20)),  # Add right margin to y-axis label
         axis.text.x = element_text(size = 12, margin = margin(t = 10)),    # Add top margin to x-axis text
         axis.text.y = element_text(size = 12, margin = margin(r = 10)),
         plot.title = element_text(size = 20, hjust = 0.5))
        
box_all


#ANOVA using the lm function
total_lm <- lm(total_density~Material, data=density_import)
summary(total_lm)
confint(total_lm)
total_anova <- aov(total_lm)
summary(total_anova)
TukeyHSD(total_anova)
#crushed concrete and granite were statistically significant from the 4 other materials, but not from each other

#ANCOVA with material and material age
ancova_model <- lm(total_density ~ Material + Material_Age + Material:Material_Age, data=density_import)

#test linearity assumption w/ residuals vs fitted values plot
plot(ancova_model, add.smooth = FALSE, which =1)

#test for normality
plot(ancova_model, which =2)

#constant variance assumption
plot(ancova_model, add.smooth = FALSE, which =3) # ---> no strong pattern, variability doesn't go up or down with fitted values

#print results of ancova
anova(ancova_model)

#There were significant effects of material age (ANCOVA: F=39.3, df = 1,124, p=4.34e-09) and material type (F=10.3, df=5,124; p=2.95e-08) 
#on total oyster density for sanctuary monitoring 2023. The interaction between material type and material age was also significant in the
#2023 dataset (F=6.3; df=5,124; p=3.0e-05). 

#plot --- not the best graph because of the lines, but might look better with more data (2019-2023)
pred.data <- expand.grid(Material_Age = 1:28, Material = c("Basalt", "Consolidated Concrete", "Crushed Concrete", "Granite", "Marl", "Reef Ball"))
pred.data <- mutate(pred.data, total_density = predict(ancova_model, pred.data))


ggplot(pred.data, aes(x= Material_Age, y = total_density, color = Material))+
  geom_line() + geom_point(data = density_import) +
  xlab("Material Age") + ylab("Total Oyster Density") + xlim(0,30) + ylim(0,8000)




#for comparing the material performance between size classes, only exacavated data should be included -- subset the imported dataframe
sub_density_import <- subset(density_import, density_import$Collection_Method == "Extraction")
# Reset levels of Material to include only the material types that were exacation 
sub_levels <- c("Crushed Concrete", "Granite", "Marl")
sub_density_import$Material <- factor(sub_density_import$Material, levels = sub_levels)

#Legal oysters analysis
#Nice plot
box_legal <- ggplot(sub_density_import, aes(x=Material, y = legal_density))+
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#56B4E9",
               color = "black",
               alpha = 0.5,
               outlier.color = "tomato1",
               outlier.size = 2)+
  labs(x = "Material Type", y = expression("Legal Density (per m"^2*")"), title = "Oyster Sanctuary Material Performance")+
  theme(axis.title.x = element_text(size = 16, margin = margin(t = 20)),  # Add top margin to x-axis label
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  # Add right margin to y-axis label
        axis.text.x = element_text(size = 12, margin = margin(t = 10)),    # Add top margin to x-axis text
        axis.text.y = element_text(size = 12, margin = margin(r = 10)),
        plot.title = element_text(size = 20, hjust = 0.5))

box_legal
#plot(legal_density~Material,data=sub_density_import, ylab=expression("Legal Frequency (Oysters per m"^2*")"))

#ANOVA using the lm function
legal_lm <- lm(legal_density~Material, data=sub_density_import)
summary(legal_lm)
confint(legal_lm)
legal_anova <- aov(legal_lm)
summary(legal_anova)
TukeyHSD(legal_anova)
#Only crushed concrete is statistically significant from marl 





#Sub-Legal oysters analysis
#Nice plot
box_sublegal <- ggplot(sub_density_import, aes(x=Material, y = sublegal_density))+
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#56B4E9",
               color = "black",
               alpha = 0.5,
               outlier.color = "tomato1",
               outlier.size = 2)+
  labs(x = "Material Type", y = expression("Sublegal Density (per m"^2*")"), title = "Oyster Sanctuary Material Performance")+
  theme(axis.title.x = element_text(size = 16, margin = margin(t = 20)),  # Add top margin to x-axis label
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  # Add right margin to y-axis label
        axis.text.x = element_text(size = 12, margin = margin(t = 10)),    # Add top margin to x-axis text
        axis.text.y = element_text(size = 12, margin = margin(r = 10)),
        plot.title = element_text(size = 20, hjust = 0.5))

box_sublegal
#plot(sublegal_density~Material,data=sub_density_import, ylab=expression("Sub-Legal Frequency (Oysters per m"^2*")"))


#Spat oysters analysis
#Nice plot
box_spat <- ggplot(sub_density_import, aes(x=Material, y = spat_density))+
  stat_boxplot(geom = "errorbar",
               width = 0.25) +
  geom_boxplot(fill = "#56B4E9",
               color = "black",
               alpha = 0.5,
               outlier.color = "tomato1",
               outlier.size = 2)+
  labs(x = "Material Type", y = expression("Spat Density (per m"^2*")"), title = "Oyster Sanctuary Material Performance")+
  theme(axis.title.x = element_text(size = 16, margin = margin(t = 20)),  # Add top margin to x-axis label
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  # Add right margin to y-axis label
        axis.text.x = element_text(size = 12, margin = margin(t = 10)),    # Add top margin to x-axis text
        axis.text.y = element_text(size = 12, margin = margin(r = 10)),
        plot.title = element_text(size = 20, hjust = 0.5))

box_spat


plot(spat_density~Material,data=sub_density_import, ylab=expression("Spat Frequency (Oysters per m"^2*")"))





















setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R")

#Data Import
OS_monitor <- read.csv("2019-2023_oyster_densities.csv") #Read in ANNUAL Dataset
OS_monitor <- read.csv("2023_density_estimates.csv")

data.frame(OS_monitor)

str(OS_monitor)

head(OS_monitor)
plot(OS_monitor$B_Sal,OS_monitor$legal,xlab = "Salinity",ylab="Legal Oyster Density")

Salinity_data <- OS_monitor[!is.na(OS_monitor$B_Sal), ]



results1 = lme(legal~B_Sal, Salinity_data, random = ~1|OS_Name)
summary(results1)



Salinity_data$B_Sal_spline <- bs(Salinity_data$B_Sal, df = 3, degree = 3)
results3 <- lme(legal ~ B_Sal_spline, Salinity_data, random = ~1|OS_Name)
summary(results3)




colorPack  <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7",'#000000')

m <- ggplot(OS_monitor, aes(x = Material_Age, y = legal_density, color = Material))+
  geom_point(size = 5, alpha = 0.5)+
  scale_color_manual(values = setNames(colorPack, levels(OS_monitor$Material)))+
  geom_smooth(method = "loess" , se = FALSE, aes(color = "Trendline")) +
  labs(x = "Material Age (years)", y = expression("Adult Oyster Density (m"^2*")"), title = "Sanctuary Material Performance")+
  theme(axis.title.x = element_text(size = 16, margin = margin(t = 20)),  # Add top margin to x-axis label
        axis.title.y = element_text(size = 16, margin = margin(r = 20)),  # Add right margin to y-axis label
        axis.text.x = element_text(size = 12, margin = margin(t = 10)),    # Add top margin to x-axis text
        axis.text.y = element_text(size = 12, margin = margin(r = 10)),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.title.align = 0.5,
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "white", color = "black"),
        plot.title = element_text(size = 20, hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.background = element_rect(fill = "#D6F2F4"),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black")) 


plot(m)





















# Function to calculate the average density for each site and material type
material_site_density <- function(x) {
  (x)%>%
    group_by(Year,OS_ID, OS_Name, Material)%>%
    dplyr::summarise(n(), Mean= mean(oyst_density), sd=sd(oyst_density))
}


MSD_total <- material_site_density(dens_total)

MSD_legal <- material_site_density(dens_legal)

MSD_sublegal <- material_site_density(dens_sublegal)

MSD_spat <- material_site_density(dens_spat)


write.csv(MSD_total,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/density breakdown/MSD_total.csv", row.names=FALSE)
write.csv(MSD_legal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/density breakdown/MSD_legal.csv", row.names=FALSE)
write.csv(MSD_sublegal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/density breakdown/MSD_sublegal.csv", row.names=FALSE)
write.csv(MSD_spat,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/density breakdown/MSD_spat.csv", row.names=FALSE)



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



write.csv(avg_mat_dens_all,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/density breakdown/avg_mat_dens_all.csv", row.names=FALSE)
write.csv(avg_mat_dens_legal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/density breakdown/avg_mat_dens_legal.csv", row.names=FALSE)
write.csv(avg_mat_dens_sublegal,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/density breakdown/avg_mat_dens_sublegal.csv", row.names=FALSE)
write.csv(avg_mat_dens_spat,"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R/density breakdown/avg_mat_dens_spat.csv", row.names=FALSE)


#writes dataframes to a .csv file in file pathway

# write.csv([],"S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2022 R/[].csv", row.names=FALSE)






