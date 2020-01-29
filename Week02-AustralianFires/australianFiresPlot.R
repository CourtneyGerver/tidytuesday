# Read in data --------------------------------------------------------------------------------
rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

# Extract common cities -----------------------------------------------------------------------
#Find common cities across datasets
unique(rainfall[,2])
unique(temperature[,1])

#Convert cities to lowercase for easier data management
rainfall$city_name <- tolower(rainfall$city_name)        
temperature$city_name <- tolower(temperature$city_name)  

#find common city names
commonCities<-intersect(rainfall$city_name, temperature$city_name)     

#Subset data frame by specific rows
rainfallSubset <- rainfall[rainfall$city_name %in% commonCities, ]
temperatureSubset <- temperature[temperature$city_name %in% commonCities, ]

# Average temp and rainfall per month and year ------------------------------------------------

#Extract just year from temperature dataset
temperatureSubset$month<-format(as.Date(temperatureSubset$date, format="%Y/%m/%d"),"%m")
temperatureSubset$month<-as.character(temperatureSubset$month)

#Extract just year from temperature dataset
temperatureSubset$year<-format(as.Date(temperatureSubset$date, format="%Y/%m/%d"),"%Y")

#find common years
commonYears<-intersect(rainfallSubset$year, temperatureSubset$year) 

#Subset data frame by specific rows
rainfallSubset <- rainfallSubset[rainfallSubset$year %in% commonYears, ]
temperatureSubset <- temperatureSubset[temperatureSubset$year %in% commonYears, ]

#Get average temp per month and rainfall
library(dplyr)
avgRainfall<-rainfallSubset %>%                      
   group_by(year, city_name) %>%                                 
   summarise(averageRainfall = mean(rainfall, na.rm=T)) %>%                                      
   mutate_if(is.numeric, ~round(., 3))

avgTemperature<-temperatureSubset %>%                      
   group_by(year, city_name) %>%                                 
   summarise(averageTemperature = mean(temperature, na.rm=T)) %>%                                      
   mutate_if(is.numeric, ~round(., 3))

#Merge data frames
names<-c("year","city_name") 
averages<-merge(avgTemperature, avgRainfall,by = c(names), all = TRUE)
averages<-averages[complete.cases(averages[ , 4]),]

# Plot ----------------------------------------------------------------------------------------
library(ggplot2)
library(gganimate)
library(gapminder)
library(gifski)
theme_set(theme_bw())

averages[averages == ""] <- NA
averages[rowSums(is.na(averages)) != ncol(averages), ] 

averages %>%                      
   group_by(city_name) %>%                                 
   summarise(minYear = min(year, na.rm=T))
averages<-subset(averages, averages[ , 1] > 1964)

##Average temperature
p <- ggplot(averages,
   aes(as.numeric(year), averageTemperature, group = city_name, color = factor(city_name))) +
   geom_line() +
   #scale_color_viridis_d() +
   labs(x = "Year", y = "Temperature (°C)") +
   ggtitle("Temperature in Australian Cities Over Time") +
   theme(plot.title=element_text(hjust = .5, color = "black",size=16,face="bold")) +
   scale_color_manual(values = c("salmon", "darkgoldenrod1","plum3","deepskyblue","darkseagreen2"), 
                      labels=c("Brisbane","Canberra","Melbourne","Perth","Sydney"), name = "City Name")+
   scale_x_continuous(breaks = c(1965,1975,1985,1995,2005,2015)) 
p

f<-p +  
   geom_point() +
   transition_reveal(year)

#Save out gif
animate(f, nframes = 55, renderer = gifski_renderer("aussieTempsOverTime.gif"))

##Average rainfall
q <- ggplot(averages,
            aes(as.numeric(year), averageRainfall, group = city_name, color = factor(city_name))) +
   geom_line() +
   #scale_color_viridis_d() +
   labs(x = "Year", y = "Rainfall (inches)") +
   ggtitle("Rainfall in Australian Cities Over Time") +
   theme(plot.title=element_text(hjust = .5, color = "black",size=16,face="bold")) +
   scale_color_manual(values = c("salmon", "darkgoldenrod1","plum3","deepskyblue","darkseagreen2"), 
                      labels=c("Brisbane","Canberra","Melbourne","Perth","Sydney"), name = "City Name")+
   scale_x_continuous(breaks = c(1965,1975,1985,1995,2005,2015)) 
q

g<-q +  
   geom_point() +
   transition_reveal(year)

#Save out gif
animate(g, nframes = 55, renderer = gifski_renderer("aussieRainfallOverTime.gif"))
