
# Read in data --------------------------------------------------------------------------------

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

# Plot ----------------------------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(png)

#Get handedness and position for each player
playerPosHandedness<-season_goals %>% 
   group_by(position, hand) %>% 
   distinct(player) %>% 
   select(player) %>% 
   filter(!is.na(hand)) #Remove data from which handedness/position was unavailable

#Read in plot background image
NHLteams <- readPNG("/Users/courtneygerver/Desktop/tidytuesday/Week10-NHLgoals/NHLteams.png")

#Plot percentages
NHLhandedness<-ggplot(playerPosHandedness, aes(x = position, fill = hand)) +  
   background_image(NHLteams) +
   geom_bar(aes(y = (..count..)/sum(..count..)), color = "black") + 
   scale_fill_manual(values=c("white", "gray")) +
   labs(title = "Handedness by Position in Top NHL Players", 
        subtitle = "Data from 164 of the top scorers of all time.",
        y="Percentage", x="Position", fill = "Dominant\nHand") +
   theme(plot.title=element_text(hjust = .5, color = "black",face="bold",size=18),
         plot.subtitle=element_text(hjust = .5, color = "black",face="bold",size=14),
         axis.text.x=element_text(color = "black",face="bold",size=14),
         axis.title.x=element_text(hjust = .5, color = "black",face="bold",size=16),
         axis.text.y=element_text(color = "black",face="bold",size=14),
         axis.title.y=element_text(hjust = .5, color = "black",face="bold",size=16),
         legend.text=element_text(color = "black",face="bold",size=14),
         legend.title=element_text(hjust = .5, color = "black",face="bold",size=16))

#Save out plot
png('Week10-NHLgoals/NHLhandedness.png',width=7.5, height=5, units = 'in',res=150)
NHLhandedness 
dev.off()
