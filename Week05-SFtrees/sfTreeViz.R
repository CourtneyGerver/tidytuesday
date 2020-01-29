# Read and clean data -------------------------------------------------------------------------

#Read in data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

# explore data --------------------------------------------------------------------------------

#Extract specie name
library(stringr)
species<-as.data.frame(as.character(word(sf_trees$species, 1, 2)))
names(species)[1] <- "specie"
sf_trees<-cbind(sf_trees,species)                              #Bind specie name with datase
sf_trees$specie<-gsub("[[:punct:]]", " ", sf_trees$specie)     #Remove punctuation

#Extract trees planted last year
sf_trees$year<-as.numeric(substr(sf_trees$date, start = 1, stop = 4)) #Get year
trees2010s <-subset(sf_trees, year >=2015 & year <=2019) 

# Plot ----------------------------------------------------------------------------------------

#Get specie planted by year
library(dplyr)
count<-trees2010s %>% 
   group_by(specie,year) %>% 
   count()

#Remove empty rows
count <- count[-c(509:513,544:548), ]

#Generate treemap
library(treemap)
treemap<-treemap(count,
        index=c("year","specie"),
        vSize="n",
        type="index",
        title="Trees Planted in San Francisco\n 2015-2019",
        fontsize.title = 14,
        fontsize.labels=c(15,11), 
        fontcolor.labels=c("black","brown"),    
        fontface.labels=c(2,2),                 
        overlap.labels=1,                      
        inflate.labels=F,
        palette="Greens",            
        align.labels=list(
           c("right", "bottom"), 
           c("center", "center")))

        