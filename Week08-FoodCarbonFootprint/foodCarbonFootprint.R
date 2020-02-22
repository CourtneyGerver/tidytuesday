
# Read in data --------------------------------------------------------------------------------

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

# Prep for plot -------------------------------------------------------------------------------

#libraries
library(tidyverse)
library(rnaturalearth)

#What industry emits the most CO2?
food_consumption %>%
   group_by(food_category) %>%
   summarise(mean = mean(co2_emmission), n = n()) 
##Beef, no surprise there

#What country has the most CO2 emissions for beef?
food_consumption %>% 
   filter(food_category == "Beef") %>%  
   arrange(desc(co2_emmission)) 
##Argentina

#Get world variable
world <- ne_countries(scale = "medium", returnclass = "sf") 

#Extract beef for plot
beef <- food_consumption %>%
   filter(food_category == "Beef") %>%
   mutate(country.corr = str_replace(country,"USA","United States"))
worldBeef <- left_join(world,beef,by=c('name'='country.corr'))

#Remove Antartica
worldBeef<-filter(worldBeef,geounit != "Antarctica") 

# Plot ----------------------------------------------------------------------------------------

beefPlot <- ggplot(data = worldBeef) +
   geom_sf(aes(fill=co2_emmission), colour = NA) +
   scale_fill_viridis_c("Emissions \n(Kg CO2/person/yr)", direction = -1) +
   theme_minimal() +
   theme(plot.title=element_text(hjust = .5, color = "black",face="bold",size=13),
         axis.text.x=element_text(hjust = .5, color = "black",face="bold",size=10),
         legend.title=element_text(size=10)) + 
   labs(title = (''~CO[2]~' emissions from beef consumption')) 

#Save out plot
png('beefPlot.png', width=6, height=3, units = 'in',res=150)
beefPlot 
dev.off()
