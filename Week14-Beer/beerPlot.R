
# Read in data --------------------------------------------------------------------------------

beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# Organize data -------------------------------------------------------------------------------

#load in library
library(tidyverse)

#tally number of barrels produced by states for bottles and cans from 2008 to 2019
bottlesCans <- beer_states %>% 
   group_by(state, type) %>% 
   filter(type %in% c("Bottles and Cans")) %>% 
   tally(barrels)

#tally number of barrels produced by states for kegs and barrels from 2008 to 2019
kegsBarrels <- beer_states %>% 
   group_by(state, type) %>% 
   filter(type %in% c("Kegs and Barrels")) %>% 
   tally(barrels)

#tally number of barrels produced by states on premises from 2008 to 2019
onPremise <- beer_states %>% 
   group_by(state, type) %>% 
   filter(type %in% c("On Premises")) %>% 
   tally(barrels)

# Plot 3 plots --------------------------------------------------------------------------------

#load in information for states data sheet 
library(usmap)

#Plot bottles and cans
a<-plot_usmap(data = bottlesCans, values = "n") + 
   scale_fill_continuous(low = "tan", high = "salmon4", 
                         name = "", 
                         label = scales::comma) +
   ggtitle("Bottles and Cans") +
   theme(plot.title=element_text(hjust = .5, color = "black",face="bold",size=14),
      legend.text=element_text(size=8, face = "bold"), 
      legend.title = element_text(size = 8, face="bold"),
      legend.position = "bottom",
      legend.justification = "center")  + 
   guides(fill = guide_colorbar(barwidth = 15, barheight = 2))  

#Plot kegs and barrels
b<-plot_usmap(data = kegsBarrels, values = "n") + 
   scale_fill_continuous(low = "tan", high = "salmon4", 
                         name = "", 
                         label = scales::comma) +
   ggtitle("Kegs and Barrels") +
   theme(plot.title=element_text(hjust = .5, color = "black",face="bold",size=14),
         legend.text=element_text(size=8, face = "bold"), 
         legend.title = element_text(size = 8, face="bold"),
         legend.position = "bottom",
         legend.justification = "center")  + 
   guides(fill = guide_colorbar(barwidth = 15, barheight = 2))  


#Plot on premise production
c<-plot_usmap(data = onPremise, values = "n") + 
   scale_fill_continuous(low = "tan", high = "salmon4", 
                         name = "", 
                         label = scales::comma) +
   ggtitle("On Premises") +
   theme(plot.title=element_text(hjust = .5, color = "black",face="bold",size=14),
         legend.text=element_text(size=8, face = "bold"), 
         legend.title = element_text(size = 8, face="bold"),
         legend.position = "bottom",
         legend.justification = "center")  + 
   guides(fill = guide_colorbar(barwidth = 15, barheight = 2))  

#Combine into a single plot
library(ggpubr)
figure <- ggarrange(a, b, c,
                    ncol = 3, nrow = 1)

beerPlot <-annotate_figure(figure,
                top = text_grob("\n\n\n\nNumber of Beer Barrels Produced For Intended Use (2008-2019)", color = "Black", face = "bold", size = 18),
                bottom = text_grob("Note: 1 barrel of beer = 31 gallons  \n", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 12))
beerPlot

#Save out plot
png('Week14-Beer/beerPlot.png',width=17, height=12, units = 'in',res=150)
beerPlot
dev.off()
