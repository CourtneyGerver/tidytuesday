
# Read in data --------------------------------------------------------------------------------

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

# Prepare data for plot -----------------------------------------------------------------------

library(tidyverse)
gdpr_violations$price<-as.numeric(gdpr_violations$price)
gdprTop20<-gdpr_violations %>%
   mutate(rank = rank(-price, ties.method="last")) %>% 
   filter(rank <= 20)

# Plot ----------------------------------------------------------------------------------------

text_color = "black"

library(scales)
library(viridis)
GDPRfines<-ggplot(gdprTop20, aes(x=name, y=price, color = price)) + 
   geom_point(aes(size=price)) +
   scale_y_continuous(labels = comma) + 
   scale_size_continuous(label=comma) +
   scale_color_viridis() +
   labs(title = "Top 20 Priciest GDPR Fines",
        subtitle = "2019-2020",
        x = "Country", y = "Fine (€)") +
   theme_minimal(base_family = "Arial", base_size = 18) +
   theme(plot.title = element_text(hjust = .5, size = 24, color = text_color), 
         plot.subtitle = element_text(hjust = .5, size = 20, color = text_color), 
         legend.position = "none",
         axis.title = element_text(color = text_color),
         axis.text.x = element_text(color = text_color, vjust = 5)) 

#Save out plot
png('Week17-GDPRfines/GDPRfines.png',width=15, height=12, units = 'in',res=150)
GDPRfines
dev.off()
