
# Read in data --------------------------------------------------------------------------------

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)

# Prep data for plot --------------------------------------------------------------------------

#load in library
library(tidyverse)

#Extract number of performances
topShows<-grosses %>% 
   select(show, theatre,performances) %>%
   group_by(show,theatre) %>%
   summarise(nPerformances = sum(performances)) %>% #Total performances by show and theatre
   ungroup() %>% 
   group_by(show) %>% 
   mutate(totalByTheatre = sum(nPerformances[match(unique(theatre), theatre)])) %>% #Total performance by show, period
   filter(totalByTheatre > 2020)  #Filter by shows that have over 3,000 performances across theatres

# Plot ----------------------------------------------------------------------------------------

#load in library
library(ggalluvial)

#Plot
broadwayPlot<-ggplot(topShows,
       aes(axis1 = theatre, axis2 = show, y = totalByTheatre)) +
   geom_alluvium(aes(fill = as.factor(show))) +
   geom_stratum() +
   geom_text(stat = "stratum", infer.label = TRUE, size = 4,fontface='bold') +
   scale_x_discrete(limits = c("Theater", "Show"),
                    expand = c(.1, .1)) +
   labs(title = "Broadway Theatres and Shows",
        subtitle = "These productions have completed over 2,020 performances!",
        y = "Relative number of performances") +
   theme_minimal() +
   theme(plot.title = element_text(hjust = .5, size = 20, face = "bold", family = "Helvetica"),
         plot.subtitle = element_text(hjust = .5, size = 16, face = "bold", family = "Helvetica"),
         axis.text.x = element_text(size = 16, face = "bold", color = "black", family = "Helvetica"),
         axis.title.x = element_text(size = 16, face = "bold", color = "black", family = "Helvetica"),
         axis.text.y = element_blank(),
         axis.title.y = element_text(size = 16, face = "bold", color = "black", family = "Helvetica"),
         legend.position = "none")
broadwayPlot

#Save out plot
png('Week18-Broadway/broadwayPlot.png',width=17, height=12, units = 'in',res=150)
broadwayPlot
dev.off()
