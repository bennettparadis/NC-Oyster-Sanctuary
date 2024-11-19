library(dplyr)
library(tidyr)
library(grid)
library(ggplot2)
library(gganimate)
library(gifski)
library(RColorBrewer)

### Population Histogram over time
setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R")
# Annual Data Import
OS_import <- read.csv("All 19-24 R Data.csv") #Read in ANNUAL Dataset
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
#site <- "Neuse River"
#site <- "Pea Island"
#site <- "Raccoon Island"
#site <- "Swan Island"
#site <- "West Bay"
site <- "West Bluff"

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

y_limit <- if (unique(Sanctuary$OS_Name) == 'Swan Island') 600 else 400
hist_data$year <- factor(hist_data$year)

dens_units <- expression(per~m^2)

p <- ggplot(hist_data, aes(x=SH))+
  geom_histogram(binwidth = 5, fill = "orange", color = "black")+
  #geom_vline((aes(xintercept=75)), color="black", linetype="dashed", size=.5)+
  #geom_vline((aes(xintercept=25)), color="black", linetype="dotdash", size=.5)+
  ylim(0,y_limit)+
  
  theme_classic()+
  transition_states(
    year,
    transition_length = 1,
    state_length = 1
  ) +
  enter_fade()+
  labs(
    x="Left Valve Length (mm)", 
    y = bquote(Oyster~Density~(per~m^2)),
    title = paste(site, "Oyster Sanctuary {closest_state}")) +
  theme(plot.title = element_text(size=15,hjust = 0.5),
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

setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R/histogram gifs")
suffix <- "_2019-2024 hist"
file_extension <- ".gif"

anim_save(paste(site, suffix, file_extension, sep = ""), animate(p, fps =10)) #adjust speed of gif here with fps value
