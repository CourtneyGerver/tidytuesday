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

p <- ggplot(averages,
   aes(year, averageTemperature, group = city_name, color = factor(city_name))) +
   geom_line() +
   scale_color_viridis_d() +
   labs(x = "year", y = "Temperature") +
   theme(legend.position = "top")
p

f<-p + transition_reveal(year)
animate(f, nframes = 100, renderer = gifski_renderer("gganim.gif"))
