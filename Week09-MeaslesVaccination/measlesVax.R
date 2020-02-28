
# Read in data --------------------------------------------------------------------------------

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

# Clean data ----------------------------------------------------------------------------------

#Only take schools with students entolled
measles <-subset(measles, enroll > 0) 

#Only take schools with percentages for personal and medical reasons 
measles <-subset(measles, (!is.na(measles[,13])) & (!is.na(measles[,14])))

#Subset data frame by specific columns 
measlesSubset <-measles[c(2,13:14)]

#Melt data into long format for plot
library(reshape2)
measlesMelt<-melt(measlesSubset, id.vars=c("state"), 
                 variable.name="exclusion",            
                 value.name="percent")

# Plot ----------------------------------------------------------------------------------------

#load in libraries
library(rcartocolor)
library(tidyverse)

#plot away
ggplot(measlesMelt, aes(x=reorder(state,-percent), y=percent, fill = reorder(exclusion, -percent)))+
   stat_summary(geom = "bar", fun.y = mean, position = "dodge", color = "black", size = .5) +
   theme_bw() +
   labs(title = "Why are students exempt from vaccination?",
        x = "State",
        y = "Average %") + 
   scale_fill_carto_d(palette = "TealGrn", name = "Reason", labels = c("Personal","Medical")) +
   theme(axis.text.x = element_text(face = "bold", color = "black", size = 16),
         axis.text.y = element_text(face = "bold", color = "black", size = 16),
         axis.title.x = element_text(face = "bold", size = 18),
         axis.title.y = element_text(face = "bold", size = 18),
         plot.title = element_text(hjust = 0.5, face = "bold", size = 26),
         legend.text = element_text(face = "bold", size = 18),
         legend.title = element_text(face = "bold", size = 18)) 