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

setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/4. Data/2023 Data")
datum_2023 = read.csv("2023 Data.csv")

data<- dplyr::rename(datum_2023,OS_ID=OS.ID,OS_Name=OS.Name,Site_ID=Site..,Sample = Sample..,SH_mm=SH..mm., Size_Class=Size.Class)
data$OS_ID<-as.numeric(gsub("OS-","",data$OS_ID)) #remove OS-" from rows under OS_ID, change to numeric variables
data$SH_mm<-as.numeric(data$SH_mm)
#data$OS_ID<-as.character(data$OS_ID)



#### Resampling 
#IMPORTANT - ONCE RESAMPLING HAS BEEN COMPLETED, SKIP THESE LINES AND SIMPLY IMPORT '2023 R Data.csv' FOR LENGTH FREQUENCY PLOTS 

#resample to "fill in" rows of subsampled sites - take the 400 measurements from subsamples and create 'full' sample using random sampling with replacement
#create an empty dataframe where the resampled data will go, use names from original dataframe
resampled_data <- data.frame(matrix(ncol = 41, nrow = 0))
colnames(resampled_data) <- colnames(data)

#Split up data into what was subsampled (only 400 oysters measured) and those samples that were "censused" or observed
sub_data <- subset(data, data$Sample.method == 'Subsample')
part_data <- subset(data, data$Sample.method != 'Subsample')

sub_data <- sub_data%>%
  select(-Sample)

#create list of sites where subsampling was used
sites <- unique(sub_data$SID)

#loop through each site using unique SID
for (i in sites) {
  #subset the data for each SID iteration
  sampled_400 <- subset(sub_data, sub_data$SID == i)
  
  #gets the total count for the site
  total_count <- unique(sampled_400$Total.Oyster.Count)
  
  #takes the empty dataframe of resampled data, binds the rows of the 400 known measurements from this site
  resampled_data <- rbind(sampled_400, resampled_data)
  
  #whatever the total count is subtract 400, this is to determine how many resamples need to be done in the nested for loop
  iterations <- total_count - 400
  
  #randomly sample with replacement from the 400 subsample, and add that row to the resampled dataframe
  for ( i in 1:iterations){
    resampled_data <- rbind(resampled_data,sampled_400[sample(1:nrow(sampled_400), 1, replace = TRUE) ,])
  }
  
}

#Organize resampled data frame before joining with part_data 
resampled_data <- resampled_data%>%
  group_by(SID) %>% 
  mutate(Sample = row_number()) %>% #recreates a Sample column that assigns a sample number for each oyster at that site
  ungroup() %>%
  relocate(Sample, .before = SH_mm) %>% #places the Sample column before SH_mm -- like in original dataset
  arrange(OS_ID, Site_ID, Sample) #properly sorts samples


#census + observational data has 10706 measurements
#subsampled data has 10000 measurements
#resampled data has 16002 measurements (6002 generated randomly within loops)

#combine the resampled data with the census data to get a full frame ready to generate length frequency plots
full_LFP_data_2023 <- rbind(resampled_data, part_data)



#save full and resampled dataframe as a .csv file in the 2023 R folder
setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2023 R")
write.csv(full_LFP_data_2023, "2023 R Data.csv")


#ONCE RESAMPLING HAS BEEN DONE ONCE AND THE CSV FILE CREATED, JUST IMPORT THAT FILE TO WORK WITH LENGTH FREQ PLOTS

#### Length Frequency Plots - Loop ####

full_LFP_data_2023 <- read.csv('2023 R Data.csv')

datum<-subset(full_LFP_data_2023, full_LFP_data_2023$Collection.Method != "Observation")
data$OS_ID<-as.numeric(data$OS_ID)

datum <- datum %>%#cut down dataset to only include what is needed, renaming condensed dataset as "data"
  select(OS_ID,OS_Name,Site_ID,SH_mm) %>%
  arrange(OS_ID)

Num_quads<-datum %>% #Calculate number of quadrats taken for each site (will be used later to standardize sampling) 
  group_by(OS_ID)%>%
  dplyr::mutate(count=n_distinct(Site_ID))%>%
  rename(Quad_Count = "count")%>%
  distinct(OS_ID,Quad_Count)

Num_quads$vector <- rownames(Num_quads) # Loops don't like skipped values for iterations (sanctuaries no longer in OSP, such as OS_ID 4, 7, or for sanctuaries we don't end up sampling), so create a list of 1:n to then add as a column for loop to reference & follow along
datum<-left_join(datum,Num_quads,by="OS_ID") #R requires a long data format, so join "Num_quad" with "data"; adds vector column to "data" dataframe
datum<-na.omit(datum) #removes any instances of "na"

br = seq(0,165,by=5) #creates a sequence; start, end, step
ranges <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,165)
#ranges <- c(10,20,30,40,50,60,70,80,90,100,110,120,130,140)
plist <- list()

loop.vector <-1:(nrow(Num_quads)) #create vector for loop to cycle through (set as total # sanctuaries sampled that year)
y.grob<-textGrob(expression("Frequency (Oysters per m"^2*")"),rot=90,gp=gpar(fontsize=24,fontface="plain"))
x.grob<-textGrob("LVL (mm)",gp=gpar(fontsize=24,fontface="plain"))
title=textGrob("2023 Size Frequencies",gp=gpar(fontsize=20,fontface="plain"))


# GENERATE AND SAVE ALL INDIVIDUAL SANCTUARY PLOTS
for (i in loop.vector) { # Loop over loop.vector to get multi-panel plots
  Sanctuary<-datum %>%
    filter(vector==i)
  freq <- hist(Sanctuary$SH_mm, breaks=br, include.lowest=TRUE, plot=FALSE)
  Count<-data.frame(range = ranges, count = (freq$counts/unique(Sanctuary$Quad_Count))*4)
  plist[[i]]<-ggplot(Count, aes(x=range, y=count)) +
    geom_bar(stat="identity",color="black", fill="grey") +
    geom_vline((aes(xintercept=75)), color="black", linetype="dashed", size=.5)+
    geom_vline((aes(xintercept=25)), color="black", linetype="dotdash", size=.5)+
    ylim(0,400)+
    theme_classic()+
    geom_text(data = Num_quads %>%
                filter(vector == i),
              mapping = aes(x=130, y=350, 
                            label=paste("n =",Quad_Count)), 
              hjust=0) +
    theme(plot.title = element_text(hjust=0.5), #toggle these three lines off/on for individual/grid plots
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+
    labs(x="LVH (mm)", y = expression("Frequency (Oysters/m"^2*")"))+  #toggle this line on/off for individual/grid plots
    ggtitle(Sanctuary$OS_Name)+
    theme(plot.title = element_text(size=20,hjust = 0.5))
  plist[i]
  #ggsave(file = paste0("Sanctuary", i, ".tiff"),width = 5, height = 4, dpi = 300, units = "in",) #toggle this line off for grid plot
}


#PLOT A SPECIFIC SANCTUARY
x<-1 #replace "i" with whatever sanctuary (1-13) you want to generate
plist[13]

#PLOT ALL SANCTUARIES IN GRID#

for (i in seq(1,length(plist), 13)) { #loop for plotting all sanctuaries on grid (3 columns) with labels, Change "(plist), x)" to total # sanctuaries sampled with excavated material (Long Shoal does not count!!)
  g=grid.arrange(grobs=plist[i:(i+12)], #change "i+x" to i+ (# sanctuaries sampled -1)
                 ncol=3,bottom=x.grob, left=y.grob, top=title)
  ggsave(file = paste0("2022 Oyster Sanctuary Demographics.tiff"), width = 12, height = 10, dpi = 300, units = "in", g)
}

