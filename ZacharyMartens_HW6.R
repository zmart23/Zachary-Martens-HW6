library(tidyverse) #load requisite libraries
library(terra) #load requisite libraries
library(dplyr) #load requisite libraries
library(tidyr) #load requisite libraries
library(stringr) #load requisite libraries

raw_tree = read.csv("RDS-2016-0005/Data/TS3_Raw_tree_data.csv") #read in raw tree data set
View(raw_tree) #view raw tree data set

##Question 1##

raw_tree[c('city', 'state')] <- str_split_fixed(raw_tree$City, ',', 2) #split "city, state" column into separate city and state columns

raw_tree_count = raw_tree %>% count(state) #count the number of observations from each state in the data set

ggplot(raw_tree_count, aes(x=state, y=n)) + 
  geom_bar(stat = "identity") #plot number of observations from each state

##Question 2##

raw_tree_filt <- raw_tree%>% filter (str_detect(state, "NC|SC")) #filter raw tree data set to only include observations from North Carolina and South Carolina
raw_tree_filt_count = raw_tree_filt %>% count(city) #identify which cities in North Carolina and South Carolina have data collected for them in the data set by counting how many observations from each individual city there are in the data set
unique(raw_tree_filt$city) #anoter method to identify which cities in North Carolina and South Carolina have data collected for them in the data set


##Question 3##
raw_tree_filt[c('genus', 'species')] <- str_split_fixed(raw_tree_filt$ScientificName, ' ', 2) #split "genus species" column in filtered North Carolina and South Carolina raw tree data set into separate genus and species columns
raw_tree_filt$AvgCdia..m. = as.integer(raw_tree_filt$AvgCdia..m.) #convert AvgCdia..m. column (crown diameter) to integer rather than string/characters (if it was not already in integer classification)
raw_tree_filt_genus_avg = aggregate(AvgCdia..m. ~ genus, raw_tree_filt, mean) #compile a table of genus names with their associated average crown diameters

##Extra Credit 1##
raw_tree_filt$Age = as.integer(raw_tree_filt$Age) #converts Age column to integer rather than string/character (if it was not already in the integer classification)
raw_tree_filt_genus_avg_age = aggregate(Age ~ genus, raw_tree_filt, mean) #compile a table of genus names with their associated average ages
raw_tree_filt_merged = merge(raw_tree_filt_genus_avg, raw_tree_filt_genus_avg_age, by='genus', all.x=TRUE) #merge tables to create a table with genera, average crown diameters, and average ages

##Extra Credit 2##
ggplot() + geom_point(data=raw_tree_filt_merged, aes(x=genus, y=AvgCdia..m., size=Age)) #create a plot that shows the differences in average age and average crown diameter between genera of trees


##Extra Credit 3##
raw_tree_filt_count_species = raw_tree_filt %>% count(species) #count the number of observations for each species in the data set