
# Read in data --------------------------------------------------------------------------------

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

# Prep for plot -------------------------------------------------------------------------------

#Get average % STEM by state
library(dplyr)
STEMavg<-salary_potential %>% 
   group_by(state_name) %>% 
   summarise(avgSTEM = mean(stem_percent))

# Plot ----------------------------------------------------------------------------------------

#libraries
library(tidyverse)
library(viridis)

#calculate the angle of the labels
number_of_bar <- nrow(STEMavg)
angle <- 90 - 360 * (1:50-0.5) /number_of_bar     

#flip angle of labels to make them readable
STEMavg$angle<-ifelse(angle < -90, angle+180, angle) 

#make circular barplot
statesSTEM <- ggplot(STEMavg, aes(x=as.factor(state_name), y=avgSTEM, fill = avgSTEM)) + 
   geom_bar(stat="identity") +
   ylim(-5,39) +
   theme_minimal() +
   theme(axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm"),
      legend.position=c(.9,.5),
      legend.text=element_text(color = "black",face="bold",size=10),
      legend.title=element_text(hjust = .5, color = "black",face="bold",size=10)) +
   coord_polar(start = 0) +
   geom_text(data=STEMavg, aes(x=as.factor(state_name), y=avgSTEM + 5, label=state_name), 
             color="black", fontface="bold",size=3, angle=STEMavg$angle, inherit.aes = FALSE ) +
   labs(fill = "% of Students\nin STEM") +
   scale_fill_viridis() 

#Save out plot
png('Week11-CollegeTuition/statesSTEM.png',width=6, height=6, units = 'in',res=150)
statesSTEM
dev.off()
