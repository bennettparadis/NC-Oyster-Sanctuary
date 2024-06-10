setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R")

#install.packages('gganimate')
#install.packages('gifski')
#install.packages('av')
#install.packages('cowplot')
#install.packages('gapminder')

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
library(splines)
library(carData)
library(gganimate)
library(gifski)
library(av)
library(cowplot)
library(gapminder)
#library(reshape2)
#library(rcompanion)
#library(FSA)
#library(multcompView)
#library(multcomp)
#library(Rcmdr)

# Annual Data Import
OS_monitor <- read.csv("2019-2023_oyster_densities.csv") #Read in ANNUAL Dataset
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

density_graph_data <- OS_monitor%>%
  select(Year, OS_Name, Material, Material_Age, legal)%>%
  filter(!is.na(legal))


library(RColorBrewer)

colorPack  <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7",'#000000')

colorPack <- c("#0072B2", "#332288", "#117733", "#D55E00", "#AA4499", 
                "#FDDD00","#000000") #"#999933",", "#882255", "#661100", "#6699CC", "#888888")

h <- ggplot(density_graph_data, aes(x = Material_Age, y = legal, color = Material))+
  geom_point(size = 5, alpha = 0.65, stroke = 1)+
  scale_color_manual(values = setNames(colorPack, levels(OS_monitor$Material)))+
  geom_smooth(method = "loess" , se = FALSE, aes(color = "Trendline")) +
  labs(x = "Material Age (years)", y = expression("Legal Oyster Density (m"^2*")"), title = "Sanctuary Material Performance")+
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
        #panel.grid.major = element_blank(),  # Remove major grid lines
        #panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.background = element_rect(fill = "white"), ##D6F2F4
        panel.background = element_rect(fill = "grey90"),
        axis.line = element_line(color = "black")) 


plot(h)





### Population Histogram over time
setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R")
# Annual Data Import
OS_import <- read.csv("All 19-23 R Data.csv") #Read in ANNUAL Dataset
data.frame(OS_import)

OS_import<-subset(OS_import, OS_import$Collection.Method != "Observation")


OS_import<- dplyr::rename(OS_import,OS_ID=OS.ID,SH_mm =SH..mm., OS_Name=OS.Name,Site_ID=Site.., Size_Class=Size.Class)


####specify the sanctuary of interest, select data for just that sanctuary


#site <- "Cedar Island"
#site <- "Crab Hole"
#site <- "Croatan Sound"
#site <- "Deep Bay"
#site <- "Gibbs Shoal"
#site <- "Little Creek"
#site <- "Middle Bay"
site <- "Neuse River"
#site <- "Pea Island"
#site <- "Raccoon Island"
#site <- "Swan Island"
#site <- "West Bay"
#site <- "West Bluff"


site_data <- subset(OS_import, OS_import$OS_Name == site)

#format dataframe
site_data$OS_ID<-as.numeric(gsub("OS-","",site_data$OS_ID)) #remove OS-" from OS_ID
site_data$SH_mm<-as.numeric(site_data$SH_mm)


site_data<-site_data %>% #cut down dataset to only include what is needed, renaming condensed dataset as "data"
  select(OS_ID,OS_Name,Year,Site_ID,SH_mm)


Num_quads<-site_data %>% #Calculate number of quadrats taken for each site (will be used later to standardize sampling) 
  group_by(Year)%>%
  dplyr::mutate(Quad_Count=n_distinct(Site_ID))%>%
  distinct(OS_ID,Quad_Count)


Num_quads$vector <- rownames(Num_quads) # Loops don't like skipped values for iterations (sanctuaries we don't end up sampling), so create a list of 1:n for loop to follow along
site_data<-left_join(site_data,Num_quads,by="Year") #R requires a long data format, so join "Num_quad" with "data"
site_data<-na.omit(site_data)

max_length = round(max(site_data$SH_mm)+10) ## to set the br and ranges, round and divided by 10; add 10 for buffer

br=seq(0, max_length, by=5)

ranges <- seq(5, max_length, by=5)


loop.vector <-1:(nrow(Num_quads)) #create vector for loop to cycle through (set as total # sanctuaries sampled that year)
y.grob<-textGrob(expression("Frequency (Oysters per m"^2*")"),rot=90,gp=gpar(fontsize=24,fontface="plain"))
x.grob<-textGrob("LVL (mm)",gp=gpar(fontsize=24,fontface="plain"))
title=textGrob(paste(site, " Demographics",gp=gpar(fontsize=14,fontface="plain")))



### Import density data ---> this would be for trying to include labels on the plot, otherwise not necessary
#density <- read.csv("19-22_oyster_densities.csv")

#sanctuary_density <- subset(density, density$OS_Name == "Swan Island")

#create a dataframe with the average density of legal oysters for each year; will be used as a label
#avg_density <- sanctuary_density%>%
#group_by(Year)%>%
#mutate(Average_total = round(mean(total)))%>%
#mutate(density = round(mean(legal)))%>%
#rename(year = Year)%>%
#select(year, density)%>%
#distinct(year, .keep_all = TRUE)



hist_data <- data.frame()

for (i in loop.vector) { 
  Sanctuary<-site_data %>%
    filter(vector==i)
  loop_year <- unique(Sanctuary$Year)
  freq <- hist(Sanctuary$SH_mm, breaks=br, include.lowest=TRUE, plot=FALSE)
  Count<-data.frame(year = loop_year, range = ranges, count = (freq$counts/unique(Sanctuary$Quad_Count))*4)
  hist_data <- rbind(hist_data,Count)
}


hist_data$count <- round(hist_data$count)

hist_data <- hist_data%>%
  uncount(count)%>%
  rename(SH = range)


hist_data$year <- factor(hist_data$year)



p <- ggplot(hist_data, aes(x=SH))+
  geom_histogram(binwidth = 5, fill = "orange", color = "black")+
  geom_vline((aes(xintercept=75)), color="black", linetype="dashed", size=.5)+
  geom_vline((aes(xintercept=25)), color="black", linetype="dotdash", size=.5)+
  ylim(0,400)+
  theme_classic()+
  transition_states(
    year,
    transition_length = 1,
    state_length = 1
  ) +
  enter_fade()+
  labs(x="Left Valve Length (mm)", y = expression("Oyster Count (m"^2*")"),title = paste(site, "Oyster Sanctuary {closest_state}")) +
  theme(plot.title = element_text(size=20,hjust = 0.5),
        axis.text = element_text(size=10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.background = element_rect(fill = "#D6F2F4"),
        panel.background = element_rect(fill = "#D6F2F4"),
        axis.line = element_line(color = "black")) + 
  enter_fade() +
  exit_fade()



anim <- animate(
  p +
    transition_states(year, transition_length = 3, state_length = 6) +
    enter_fade() + 
    exit_fade(),
  fps = 15)

#anim

setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R/histogram gifs")
suffix <- "_2019-2023 hist2"
file_extension <- ".gif"

anim_save(paste(site, suffix, file_extension, sep = ""), animate(p, fps =10)) #adjust speed of gif here with fps value

