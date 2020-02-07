
# Read in data --------------------------------------------------------------------------------

standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')

# Plot ----------------------------------------------------------------------------------------

#Extract data from 2019
NFL2019 <-subset(standings, year == 2019) 

#Add positive or negative margin value for plot fill
NFL2019$margin_positivity<-ifelse(NFL2019$margin_of_victory <= 0, "Negative","Positive")

#Diverging Barcharts
library(ggplot2)
ggplot(NFL2019, aes(x=reorder(team_name,margin_of_victory), y=margin_of_victory, label=margin_of_victory)) + 
   geom_bar(stat='identity', aes(fill=margin_positivity), color = "black")  +
   labs(title = "2019 NFL MARGIN OF VICTORY", x="TEAM", y="MARGIN OF VICTORY") +
   coord_flip() + theme_bw() +
   theme(plot.title=element_text(hjust = .5, color = "black",size=18,face="bold", family = "Freshman"),
         axis.text.x=element_text(hjust = 1, color = "black",face="bold",size=12, family = "DIN Alternate Bold"),
         axis.title.x=element_text(color = "black",face="bold",size=14, family = "Freshman"),
         axis.text.y=element_text(color = "black",face="bold",size=12, family = "DIN Alternate Bold"),
         axis.title.y=element_text(color = "black",face="bold",size=14, family = "Freshman"),
         legend.position = "none") + scale_fill_brewer(palette="Set1")

#Save out plot
ggsave("NFLplot.png")
