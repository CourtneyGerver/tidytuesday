# Libraries -----------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(gganimate)

# Read in data --------------------------------------------------------------------------------
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# Clean data ----------------------------------------------------------------------------------

# Remove empty rows
passwords <- passwords[-c(501:507), ]

#Reverse code ranks so data appears from least to most popular
passwords$revRank <- 500-passwords$rank


# Summarise data ------------------------------------------------------------------------------

passwords %>%                      
   group_by(category) %>%                                 
   count()

# Plot ----------------------------------------------------------------------------------------

##Plot
passPlot <- ggplot(passwords, aes(category, as.numeric(revRank), color = category)) +
   geom_point(size = 1) + 
   geom_text(aes(label=password), vjust=-0.2, size = 5) +
   scale_color_viridis_d() +
   labs(x = "Password Category", y = "Password Popularity (Least to Most)") +
   ggtitle("What is the most popular password per category?") +
   theme_classic()+
   theme(plot.title=element_text(hjust = .5, color = "black",size=18,face="bold"),
         axis.text.x=element_text(hjust = 1, angle = 45, color = "black",face="bold",size=12),
         axis.title.x=element_text(color = "black",face="bold",size=14),
         axis.text.y=element_text(color = "black",face="bold",size=12),
         axis.title.y=element_text(color = "black",face="bold",size=14),
         legend.position = "none") 

#Add animation to plot
passPlot <- passPlot + transition_time(revRank) +
   shadow_mark(alpha = 0.4, size = 0.6)

#Save out gif
animate(passPlot, nframes = 150, renderer = gifski_renderer("passRanks.gif", loop = F))
