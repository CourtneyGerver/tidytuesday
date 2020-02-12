
# Read in data --------------------------------------------------------------------------------

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')


# Plot ----------------------------------------------------------------------------------------

#Organize months for easier plotting
hotels$arrival_date_month<-as.factor(hotels$arrival_date_month)

hotels$monthOrder<-ifelse(hotels$arrival_date_month == "January", 1,
                          ifelse(hotels$arrival_date_month == "February", 2,
                          ifelse(hotels$arrival_date_month == "March", 3, 
                          ifelse(hotels$arrival_date_month == "April", 4,
                          ifelse(hotels$arrival_date_month == "May", 5, 
                          ifelse(hotels$arrival_date_month == "June", 6, 
                          ifelse(hotels$arrival_date_month == "July", 7,
                          ifelse(hotels$arrival_date_month == "August", 8,
                          ifelse(hotels$arrival_date_month == "September", 9,
                          ifelse(hotels$arrival_date_month == "October", 10,
                          ifelse(hotels$arrival_date_month == "November", 11,
                          ifelse(hotels$arrival_date_month == "December", 12,13))))))))))))


# Plot

library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

ggplot(hotels[hotels$arrival_date_year == 2016,], aes(x=arrival_date_day_of_month, y=reorder(arrival_date_month,monthOrder), 
           fill=reorder(arrival_date_month,monthOrder))) +
   geom_density_ridges(alpha=0.6, stat="binline", bins=31) +
   theme_ridges() +
   theme(plot.title=element_text(hjust = .5, color = "black",face="bold",size=18),
         axis.text.x=element_text(color = "black",face="bold",size=14),
         axis.title.x=element_text(hjust = .5, color = "black",face="bold",size=16),
         axis.text.y=element_text(color = "black",face="bold",size=14),
         axis.title.y=element_text(hjust = .5, color = "black",face="bold",size=16),
         legend.position = "none") +
   labs(title = "What day did people check into hotel rooms\n in 2016?", x="Day of Month", y="Month")

#Save out plot
ggsave("hotelsPlot.png")
  