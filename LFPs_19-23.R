setwd("S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R")

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
#library(reshape2)
#library(rcompanion)
#library(FSA)
#library(multcompView)
#library(multcomp)
#library(Rcmdr)

# Annual Data Import
OS_import <- read.csv("All 19-23 R Data.csv") #Read in ANNUAL Dataset
data.frame(OS_import)

#remove NA rows
OS_import <- OS_import[complete.cases(OS_import$Year),]

#remove non-extracted rows
OS_import<-subset(OS_import, OS_import$Collection.Method == "Extraction")

data<- dplyr::rename(OS_import,OS_ID=OS.ID,OS_Name=OS.Name,Site_ID=Site..,SH_mm=SH..mm., Size_Class=Size.Class)

####Format dataframe for Length Frequency Plots ####
data$OS_ID<-as.numeric(gsub("OS-","",data$OS_ID)) #remove OS-" from rows under OS_ID, change to numeric variables
data$SH_mm<-as.numeric(data$SH_mm)
data$OS_ID<-as.character(data$OS_ID)
data$Year<-as.character(data$Year)

data<-data %>% #cut down dataset to only include what is needed, renaming condensed dataset as "data"
  select(Year,OS_ID,OS_Name,Site_ID,SH_mm)

#write.csv(data,"2019-2022_LFP_data.csv")


Oyster_Sanctuaries <- c(unique(data$OS_Name))



original <- "S:/8. Oyster Sanctuaries/3. Monitoring and Data/1. Oyster Sanctuary (OS)/5. Analysis/2019-current R/LFPs_19-23"
setwd(original)


## 5mm loop for 2019 to 2022 dataset
## creates a folder for each sanctuary, and within each folder there is a histogram for each year, as well as a comprehensive gird layout showing year after year

for (i in Oyster_Sanctuaries){
  setwd(original)
  folder <- i
  dir.create(folder)
  setwd(folder)
  
  OS_subset <- subset(data,OS_Name == i)
  write.csv(OS_subset, paste0(i, "_2019-2023.csv"), row.names=FALSE)
  
  
  Num_sites<-OS_subset %>% #Calculate number of quadrats taken for each site (will be used later to standardize sampling) 
    group_by(Year)%>%
    dplyr::mutate(count=n_distinct(Site_ID))%>%
    rename(Site_Count = "count")%>%
    distinct(OS_ID,Site_Count)
  
  Num_sites$vector <- rownames(Num_sites) # Loops don't like skipped values for iterations (sanctuaries we don't end up sampling), so create a list of 1:n for loop to follow along
  OS_subset<-left_join(OS_subset,Num_sites,by="Year") #R requires a long data format, so join "Num_quad" with "data"
  OS_subset<-na.omit(OS_subset)
  
  years=c(unique(OS_subset$Year)) #number of years a Sanctuary was sampled -- how many graphs should be made, used in the last code block for the grid plot
  max_length = round(max(OS_subset$SH_mm)+10) ## to set the br and ranges, round and divided by 10; add 10 for buffer
  
  
  #br = seq(0,160,by=10)
  br=seq(0, max_length, by=5)
  
  ranges <- seq(5, max_length, by=5)
  #ranges <- c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,165)
  #ranges <- c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160)
  plist <- list()
  
  loop.vector <-1:(nrow(Num_sites)) #create vector for loop to cycle through (set as total # sanctuaries sampled that year)
  y.grob<-textGrob(expression("Frequency (Oysters per m"^2*")"),rot=90,gp=gpar(fontsize=24,fontface="plain"))
  x.grob<-textGrob("LVL (mm)",gp=gpar(fontsize=24,fontface="plain"))
  title=textGrob("_____ Demographics",gp=gpar(fontsize=14,fontface="plain"))
  
  
  #Figure out the maximum frequency to then set y-limit in plots
  
  #OS_freqs <- OS_subset%>%
    #mutate(SH_binned = cut(SH_mm, breaks=seq(0,max(SH_mm)+10, by = 5)))%>%
    #group_by(Year, SH_binned)%>%
    #summarise(freq = n())
  
  #OS_freqs<-left_join(OS_freqs,Num_sites,by="Year")
  
  #OS_freqs <- OS_freqs%>%
    #mutate(adjusted_freq = freq/Site_Count)
  
  #y_limit <- max(OS_freqs$adjusted_freq)
  y_limit <- 400

    
  # GENERATE AND SAVE ALL INDIVIDUAL SANCTUARY PLOTS
  for (i in loop.vector) { # Loop over loop.vector to get multi-panel plots
    Sanctuary<-OS_subset %>%
      filter(vector==i)
    freq <- hist(Sanctuary$SH_mm, breaks=br, include.lowest=TRUE, plot=FALSE)
    Count<-data.frame(range = ranges, count = (freq$counts/unique(Sanctuary$Site_Count))*4)
    plist[[i]]<-ggplot(Count, aes(x=range, y=count)) +
      geom_bar(stat="identity",color="black", fill="grey") +
      geom_vline((aes(xintercept=75)), color="black", linetype="dashed", size=.5)+
      geom_vline((aes(xintercept=25)), color="black", linetype="dotdash", size=.5)+
      ylim(0,y_limit)+
      theme_classic()+
      geom_text(data = Num_sites %>%
                  filter(vector == i),
                mapping = aes(x=(max_length*0.9), y=((y_limit)/2), 
                              label=paste("n =",Site_Count)),size=5,  
                hjust=0.5) +
      theme(plot.title = element_text(hjust=0.5),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text = element_text(size=14))+
      labs(x="LVH (mm)", y = expression("Oyster Count (m"^2*")"))+
      ggtitle(Sanctuary$OS_Name, subtitle=Sanctuary$Year)+
      theme(plot.title = element_text(size=20,hjust = 0.5), plot.subtitle = element_text(size=15, hjust=0.5))
    plist[i]
    OS <- c(unique(Sanctuary$OS_Name))
    filename <- paste("LFplot_",OS,"_",i, sep="")
    ggsave(file = paste(filename,".tiff", sep=""),width = 5, height = 4, dpi = 300, units = "in",)
  }
  
  
  
  #PLOT ALL YEARS IN GRID#
  # generate individual plots again, but change a few thematic items (titles, subtitles), skip saving them again, then run through grid.arrange loop 
  for (i in loop.vector) { # Loop over loop.vector to get multi-panel plots
    Sanctuary<-OS_subset %>%
      filter(vector==i)
    freq <- hist(Sanctuary$SH_mm, breaks=br, include.lowest=TRUE, plot=FALSE)
    Count<-data.frame(range = ranges, count = (freq$counts/unique(Sanctuary$Site_Count))*4)
    plist[[i]]<-ggplot(Count, aes(x=range, y=count)) +
      geom_bar(stat="identity",color="black", fill="grey") +
      geom_vline((aes(xintercept=75)), color="black", linetype="dashed", size=.5)+
      geom_vline((aes(xintercept=25)), color="black", linetype="dotdash", size=.5)+
      ylim(0,y_limit)+
      theme_classic()+
      geom_text(data = Num_sites %>%
                  filter(vector == i),
                mapping = aes(x=max_length, y=((y_limit)/2), 
                              label=paste("n =",Site_Count)),size=5,  
                hjust=0.5) +
      theme(plot.title = element_text(hjust=0.5),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text = element_text(size=14))+
      labs(x="LVH (mm)", y = expression("Oyster Count (m"^2*")"))+
      ggtitle(paste("(",Sanctuary$Year,")"))+
      theme(plot.title = element_text(size=15,hjust = 0.5), plot.subtitle = element_text(size=15, hjust=0.5))
    plist[i]
  }
  
  
  for (i in seq(1,length(plist), length(years))) { #loop for plotting all sanctuaries on grid (3 columns) with labels, Change "(plist), x)" to total # sanctuaries sampled
    g=grid.arrange(grobs=plist[i:(i+(length(years)-1))], #change "i+x" to i+ (# years -1)
                   ncol=2,bottom=x.grob, left=y.grob, top=textGrob(Sanctuary$OS_Name, gp =gpar(fontsize=20)))
    
    ggsave(file = paste0(OS," Frequencies", ".tiff"), width = 18, height = 18, dpi = 300, units = "in", g)
  }
  
  setwd(original)
  
  
}


