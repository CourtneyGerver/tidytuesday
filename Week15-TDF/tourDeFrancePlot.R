
# Read in data --------------------------------------------------------------------------------

tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')

# Prep data for plot --------------------------------------------------------------------------

#Extract year
library(tidyverse)
tdf_stages$Year<-substr(tdf_stages$Date, start = 1, stop = 4) 
tdf_stages$Year<-as.numeric(tdf_stages$Year)

#Prepare stage data by turning all legs with letters into numbers
library(stringr)
regexp <- "[[:digit:]]+"
tdf_stages$newStage<-str_extract(tdf_stages$Stage, regexp)

#Keep top distance if multiple riders per stage for sake of cleaner plot
tdfTidy <- tdf_stages %>%
   filter(Year >= 1992) %>% 
   group_by(Year,newStage) %>%
   top_n(1, abs(Distance))

# Plot ----------------------------------------------------------------------------------------

#Load in libraries
library(gganimate)
library(gapminder)

#Plot
TDFplot<-ggplot(tdfTidy, aes(x=as.numeric(newStage), y = Distance, fill = as.numeric(newStage),
                             label = paste("Destination:", Destination), nsmall = 2)) + 
   geom_bar(stat='identity') +
   coord_flip(clip = "off", expand = FALSE) + 
   scale_fill_continuous(trans = 'reverse') +
   scale_x_reverse() +
   geom_text(aes(y=0,label = Destination,hjust=0), color = "lightgray") +
   guides(color = FALSE, fill = FALSE) +
   theme_bw() +
   labs(title = "25 Years of Tour de France Distance By Stage",
        subtitle='Year: {closest_state}', x = "Stage", y = "Distance (Km)") +
   theme(plot.title = element_text(hjust = .5, size = 18, face = "bold"),
         plot.subtitle = element_text(hjust = .5, size = 16),
         axis.text.x = element_text(size = 16, face = "bold", color = "black"),
         axis.title.x = element_text(size = 16, face = "bold", color = "black"),
         axis.text.y = element_text(size = 16, face = "bold", color = "black"),
         axis.title.y = element_text(size = 16, face = "bold", color = "black"))+
   transition_states(Year, transition_length = 3, state_length = 1) +
   enter_grow() +
   exit_shrink() +
   ease_aes('sine-in-out')
TDFplot

#Save out gif 
animate(TDFplot, nframes = 100, renderer = gifski_renderer("TDFplot.gif"))
