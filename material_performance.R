setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R")

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

density_import = read.csv("2019-2023_oyster_densities.csv")
head(density_import)
str(density_import)

# Create density_table dataframe
density_table <- density_import %>%
  dplyr::select(OS_ID, OS_Name, Site_ID, total) %>%
  group_by(OS_Name) %>%
  summarise(Mean = mean(total), SD = sd(total)) %>%
  distinct(OS_Name, Mean, SD)



#Compare material performance

#change Material variable to a factor for categorical analysis
density_import$Material <- as.factor(density_import$Material)

#Total density - material analysis
# Statistics - comparing material performance for total oyster density
plot(total~Material,data=density_import, ylab=expression("Total Frequency (Oysters per m"^2*")"))

#Nice plot
box_all <- ggplot(density_import, aes(x=Material, y = total))+
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
total_lm <- lm(total~Material, data=density_import)
summary(total_lm)
confint(total_lm)
total_anova <- aov(total_lm)
summary(total_anova)
TukeyHSD(total_anova)
#crushed concrete and granite were statistically significant from the 4 other materials, but not from each other

#ANCOVA with material and material age
ancova_model <- lm(total ~ Material + Material_Age + Material:Material_Age, data=density_import)

#test linearity assumption w/ residuals vs fitted values plot
#looks like homoscedasticity --> greater spread as x increases
plot(ancova_model, add.smooth = FALSE, which =1)

#test for normality
plot(ancova_model, which =2)

#constant variance assumption
plot(ancova_model, add.smooth = FALSE, which =3) # ---> little bit of a pattern, larger variability with larger fitted values

#print results of ancova
anova(ancova_model)

#There were significant effects of material age (ANCOVA: F=39.3, df = 1,124, p=4.34e-09) and material type (F=10.3, df=5,124; p=2.95e-08) 
#on total oyster density for sanctuary monitoring 2023. The interaction between material type and material age was also significant in the
#2023 dataset (F=6.3; df=5,124; p=3.0e-05). 

#plot --- not the best graph because of the lines, but might look better with more data (2019-2023)
pred.data <- expand.grid(Material_Age = 1:28, Material = c("Basalt", "Consolidated Concrete", "Crushed Concrete", "Granite", "Marl", "Reef Ball", "Shell"))
pred.data <- mutate(pred.data, total = predict(ancova_model, pred.data))


ggplot(pred.data, aes(x= Material_Age, y = total, color = Material))+
  geom_line() + geom_point(data = density_import) +
  xlab("Material Age") + ylab("Total Oyster Density") + xlim(0,30) + ylim(0,5000)







#Testing again without Little Creek or need to include depth! 











