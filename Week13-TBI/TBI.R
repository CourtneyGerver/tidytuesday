
# Read in data --------------------------------------------------------------------------------

tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')


# Describe data -------------------------------------------------------------------------------

#load in library
library(tidyverse)

#How many active military members were diagnosed with TBI?
tbi_military %>%
   filter(component == "Active") %>% 
   summarize(totalDiagnosed = sum(diagnosed, na.rm = TRUE))

#Percentage by severity
tbi_military %>%
   filter(component == "Active") %>% 
   group_by(severity) %>% 
   summarize(diagnosedBySeverity = sum(diagnosed, na.rm = TRUE))

# Plot ----------------------------------------------------------------------------------------

#Read in libraries
library(ggtext)

#Assign colors for plot
background_color <- "black"
text_color <- "lightgray"

mild_color <- "green3"
moderate_color <- "yellow"
penetrating_color <- "orange"
severe_color <- "red"
notClassifiable_color <- "darkgray"

#Specify fill order
tbi_military$severity <- factor(tbi_military$severity, levels=c("Mild","Moderate","Penetrating","Severe","Not Classifiable"))

#Create plot
TBIplot<-ggplot(subset(tbi_military,component %in% c("Active")), aes(x = reorder(service, -diagnosed), y = diagnosed, fill = severity)) + 
   geom_bar(position="stack", stat="identity") +
scale_fill_manual(values = c(mild_color, moderate_color, penetrating_color, severe_color, notClassifiable_color)) +
guides(fill = FALSE) +
labs(title = "From 2006-2014, the United States armed forces recorded nearly 200,000 cases of traumatic \nbrain injury (TBI) in active duty members.",
subtitle = glue::glue("These TBI diagnoses were broken up into the following classifications: <span style='color:{mild_color};font-family:\"Arial\"'>Mild</span>, <span style='color:{moderate_color};font-family:\"Arial\"'>Moderate</span>, <span style='color:{penetrating_color};font-family:\"Arial\"'>Penetrating</span>, and <span style='color:{severe_color};font-family:\"Arial\"'>Severe</span>. <br> About 6% cases were <span style='color:{notClassifiable_color};font-family:\"Arial\"'>Not Classifiable</span>."),
caption = "Data from www.cdc.gov/traumaticbraininjury/data",
x = "Military Branch", y = " ") +
   theme_minimal(base_family = "Arial", base_size = 18) +
   theme(plot.title = element_text( size = 24, color = text_color), 
      plot.subtitle = element_markdown(color = text_color, lineheight = 1),
      plot.caption = element_text(color = text_color, size = 14),
      plot.background = element_rect(fill = background_color, color = background_color),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(color = text_color),
      axis.text.x = element_text(color = text_color, vjust = 5)) 
TBIplot

#Save out plot
png('Week13-TBI/TBIplot.png',width=15, height=12, units = 'in',res=150)
TBIplot 
dev.off()
