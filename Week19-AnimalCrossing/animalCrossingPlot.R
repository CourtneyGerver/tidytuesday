
# Read in data --------------------------------------------------------------------------------

user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')

# Prep data for plot --------------------------------------------------------------------------

#load in libraries
library(tidyverse)
library(lubridate)

#Determine day and week
user_reviews<-user_reviews %>% 
   mutate(week = isoweek(date))
user_reviews$day <- weekdays(as.Date(user_reviews$date))

#Count number of reviews per week
datForPlot<-user_reviews %>% 
   group_by(date,day,grade,week) %>% 
   count()

#Assign weeks for plot factor
datForPlot$weekOf<-as.factor(ifelse(datForPlot$week == 12, "3/20-3/22",
                             ifelse(datForPlot$week == 13, "3/23-3/29",
                             ifelse(datForPlot$week == 14, "3/30-4/5",
                             ifelse(datForPlot$week == 15, "4/6-4/12",
                             ifelse(datForPlot$week == 16, "4/13-4/19",
                             ifelse(datForPlot$week == 17, "4/20-4/26","4/27-5/3")))))))

#Assign shorter labels for plot 
datForPlot$dayShort<-as.factor(ifelse(datForPlot$day == "Monday", "M",
                             ifelse(datForPlot$day == "Tuesday", "Tu",
                             ifelse(datForPlot$day == "Wednesday", "W",
                             ifelse(datForPlot$day == "Thursday", "Th",
                             ifelse(datForPlot$day == "Friday", "F",
                             ifelse(datForPlot$day == "Saturday", "Sa","Su")))))))

#Assign day order for plot
datForPlot$dayOrder<-as.factor(ifelse(datForPlot$day == "Monday", 1,
                               ifelse(datForPlot$day == "Tuesday", 2,
                               ifelse(datForPlot$day == "Wednesday", 3,
                               ifelse(datForPlot$day == "Thursday", 4,
                               ifelse(datForPlot$day == "Friday", 5,
                               ifelse(datForPlot$day == "Saturday", 6,7)))))))

#Reorder days with order
datForPlot$dayShort <- reorder(datForPlot$dayShort, as.numeric(datForPlot$dayOrder))

# Plot ----------------------------------------------------------------------------------------

#Load last library
library(colorspace)

#Plot away!
animalCrossingPlot<-ggplot(datForPlot, aes(x = dayShort, y = n))+
   facet_grid(. ~ weekOf) +
   geom_col(aes(fill = as.factor(grade)), width = 0.7, color = "black") +
   labs(title = "Animal Crossing: New Horizons Reviews", 
          subtitle = "People had a lot to say just a few days after its release!",
          x = "Day Reviewed", y = "Number of Reviews", fill = "Game Grade") +
   theme_test()+
   theme(plot.title = element_text(hjust = .5, size = 24, face = "bold", color = "chocolate4", family = "Arial Rounded MT"),
         plot.subtitle = element_text(hjust = .5, size = 16, face = "bold", color = "chocolate4", family = "Arial Rounded MT"),
         axis.text.x = element_text(size = 12, face = "bold", color = "chocolate4", family = "Arial Rounded MT"),
         axis.title.x = element_text(size = 12, face = "bold", color = "chocolate4", family = "Arial Rounded MT"),
         axis.text.y = element_text(size = 12, face = "bold", color = "chocolate4", family = "Arial Rounded MT"),
         axis.title.y = element_text(size = 12, face = "bold", color = "chocolate4", family = "Arial Rounded MT"),
         legend.title = element_text(size = 12, face = "bold", color = "chocolate4", family = "Arial Rounded MT"),
         strip.text.x = element_text(size = 12, face = "bold", color = "chocolate4", family = "Arial Rounded MT"),
         panel.background = element_rect(fill = "lightgreen"),
         plot.background = element_rect(fill = "skyblue"),
         legend.background = element_rect(fill = "skyblue"),
         legend.position = "bottom") +
   scale_fill_discrete_qualitative(palette = "Dark 3") + 
   guides(fill=guide_legend(nrow=1,byrow=TRUE))

#Save out plot
png('Week19-AnimalCrossing/animalCrossingPlot.png',width=17, height=12, units = 'in',res=150)
animalCrossingPlot
dev.off()
