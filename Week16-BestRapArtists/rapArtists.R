
# Read in data --------------------------------------------------------------------------------

#Load in using the tidy tuesday package
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load(2020, week = 16)
polls <- tuesdata$polls


# Format data for plot ------------------------------------------------------------------------

#Load in libraries
library(dplyr)
library(stringr)

#Make data table
pollRank <- polls %>%     
   filter(year == 1993) %>% 
   group_by(title, year)  %>%
   summarize_each(list(mean = mean, min = min, max = max), rank) %>% 
   mutate(difference = min - max) %>% 
   ungroup() %>% 
   mutate(title = gsub("Fuck", "F***", title)) #Gotta censor!

# Plot data -----------------------------------------------------------------------------------

#Load libraries
pacman::p_load('bbplot', 'ggalt', 'tidyr')

#Make plot
rapPlot<-ggplot(pollRank, aes(x = `min`, xend = `max`, y = reorder(title, mean), group = title)) + 
   geom_dumbbell(colour = "#dddddd",
                 size = 3,
                 colour_x = "#FAAB18",
                 colour_xend = "#1380A1") +
   bbc_style() + 
   labs(title="The golden age for rap, 1993",
        subtitle="Lots of variability within critic rankings",
        caption = "Data: BBC Music, http://www.bbc.com/culture/story/20191007-the-greatest-hip-hop-songs-of-all-time-who-voted") + 
   geom_label(aes(x = 4, y = 'Insane In The Brain', label = "\n\nNo arguments for\nthese songs!"), 
              hjust = 0, 
              vjust = 0.5, 
              lineheight = 0.8,
              colour = "#555555", 
              fill = "transparent", 
              label.size = NA, 
              family="Helvetica", 
              size = 6) 

#Save out plot
png('Week16-BestRapArtists/rapPlot.png',width=15, height=12, units = 'in',res=150)
rapPlot 
dev.off()
