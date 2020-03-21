
# Read in data --------------------------------------------------------------------------------

##From GitHub
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')

##From Schrute package
install.packages("schrute")
library(schrute)
schruteDat<-schrute::theoffice

# Organize data -------------------------------------------------------------------------------

library(tidyverse)
writerDat<-schruteDat %>% 
   mutate(season = as.numeric(season),
          episode = as.numeric(episode)) %>% 
   group_by(episode, season) %>% 
   count(writer) 

#Merge two datasets
mergedDat<-merge(x=office_ratings,y=writerDat,by=c("season","episode"),all.x=TRUE)

#Wrap multiple writers
library(stringi)
mergedDat$writer <- stri_replace_all_fixed(mergedDat$writer, ";", "\n")

# Plot ----------------------------------------------------------------------------------------

#Make plot
TheOfficePlot<-ggplot(mergedDat, aes(x = as.factor(season), y = as.factor(episode), fill = imdb_rating, label= writer)) + 
   geom_tile(color = "black")  + 
   scale_fill_viridis_c(option="plasma") +
   geom_text(color="black", size=2.65, fontface=2) +
   theme_classic() +
   labs(title="The Office Ratings", 
        subtitle="Crediting episode rating to writer", 
        x= "Season", y= "Episode", fill = "IMDB Rating") + 
   theme(plot.title=element_text(hjust = .5, color = "black",face="bold",size=18, family = "Arial"),
         plot.subtitle=element_text(hjust = .5, color = "black",face="bold",size=14, family = "Arial"),
         axis.text.x=element_text(color = "black",face="bold",size=14, family = "Arial"),
         axis.title.x=element_text(hjust = .5, color = "black",face="bold",size=16, family = "Arial"),
         axis.text.y=element_text(color = "black",face="bold",size=14, family = "Arial"),
         axis.title.y=element_text(hjust = .5, color = "black",face="bold",size=16, family = "Arial"),
         legend.text=element_text(color = "black",face="bold",size=14, family = "Arial"),
         legend.title=element_text(hjust = .5, color = "black",face="bold",size=16, family = "Arial")) 

#Save out plot
png('Week12-TheOffice/TheOfficePlot.png',width=15, height=10, units = 'in',res=150)
TheOfficePlot 
dev.off()

# Bonus That’s What She Said Count ------------------------------------------------------------

#Extract phrase
thatsWhatSheSaid<-filter(schruteDat, grepl("what she said",text_w_direction))
nrow(thatsWhatSheSaid) #30 times total
