# Read and clean data -------------------------------------------------------------------------

#Read in data
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

#Remove duplicates
library(dplyr)
spotify_songs <- 
   spotify_songs %>%
   distinct(track_name, track_artist, .keep_all = TRUE)

# Prep for plot -------------------------------------------------------------------------------

#Extract songs from the 80s
spotify_songs$year<-as.numeric(substr(spotify_songs$track_album_release_date, start = 1, stop = 4)) #Get year 
throwback80s <-subset(spotify_songs, year >=1980 & year <=1989) 

#Subset intended columns
subset80s <- throwback80s[c(2,12,13,21,24)]

#Melt into useable columns
library(reshape2)
spotifyMelt<-melt(subset80s, id.vars=c("track_name","year"),
       variable.name="audioFeature",
       value.name="value")

# Plot ----------------------------------------------------------------------------------------

library(ggplot2)
songPlot<- ggplot(spotifyMelt, aes(x=year,y = value, colour = audioFeature)) + 
   stat_summary(fun.y="mean", geom="line", size = 2) +
   stat_summary(fun.y="mean", geom="point", size = 3) +
   labs(title = "An 80s Music Retrospective", 
        y="Value", x="Year") +
   theme_dark(base_family="VCR OSD Mono")+ 
   theme(plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black", size = 0.5, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'dotted', colour = "green"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',colour = "green"),
      plot.title=element_text(hjust = .5, color = "white",size=20),
      axis.text.x=element_text(color = "white",face="bold",size=14),
      axis.title.x=element_text(color = "white",face="bold",size=18),
      axis.text.y=element_text(color = "white",face="bold",size=14),
      axis.title.y=element_text(color = "white",face="bold",size=18),
      legend.background = element_rect(fill = "black"),
      legend.text = element_text(color = "white", size = 14),
      legend.title = element_text(color = "white", size = 16)) +
   scale_x_continuous(breaks = pretty(spotifyMelt$year, n = 10)) + 
   scale_color_manual(values=c("#FF0076","#FF6B58","#9239F6"))+
   guides(color=guide_legend(title="Song Feature")) 

#Save out .png
png(filename="songPlot.png", width = 800, height = 600)
plot(songPlot)
dev.off()
