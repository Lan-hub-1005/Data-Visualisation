
# ------------------------- < load packages > ------------------------


install.packages('tidyverse')
install.packages('reshape2')
install.packages('sqldf')
install.packages('tmap')
install.packages('countrycode')

library(tidyverse)
library(reshape2)
library(sqldf)
library(tmap)
library(sf)
library(countrycode)





# ------------------------- < Import Dataset > ------------------------

# import data

url_healthdata <- 'https://raw.githubusercontent.com/Lan-hub-1005/Data-Visualisation/main/health.csv'
data <- read.csv(url_healthdata)
View(data)





# ------------------------- < Data Cleaning and Preparation > ------------------------

# Explore the dataset

names(data)[names(data) == "Flag.Codes"] <- "Flag_Codes"
ncol(data)
nrow(data)
dim(data)

# How many countries included in the dataset?
#     ----- (result: 52 countries in total)

sqldf("select distinct(LOCATION) from data")
length(unique(data$LOCATION))

# What indicators are included in the data? 
#     ----- (result: HEALTHEXP; only 1 indicator in total)

sqldf("select distinct(INDICATOR) from data")
length(unique(data$INDICATOR))

# Which subjects are included in the data?
#     ----- (result: TOT, OOPEXP, COMPULSORY, VOLUNTARY; 4 subjects in total)

sqldf("select distinct(SUBJECT) from data")
length(unique(data$SUBJECT))

# What measures are included in the data?
#     ----- (result: PC_GDP, USD_CAP, PC_HEALTHXP; 3 meausures in total)

sqldf("select distinct(MEASURE) from data")
length(unique(data$MEASURE))

# What frequencies are included in the data?
#     ----- (result: A; only 1 frequency in total)

sqldf("select distinct(FREQUENCY) from data")
length(unique(data$FREQUENCY))

# Which years are included in the data?
#      ----- (result: from 1970 to 2019; 50 years in total)

sqldf("select distinct(TIME) from data")
length(unique(data$TIME))

# What flag codes are included in the data?
#     ----- (result: D, E, B, P, NA; 5 in total)

sqldf("select distinct(Flag_Codes) from data")
length(unique(data$Flag_Codes))


# Drop the columns that are not quite useful for later data analysis.

# drop Flag_Codes, FREQUENCY and INDICATOR

health_spending <- select(data,-c('Flag_Codes','FREQUENCY','INDICATOR'))

View(health_spending)
dim(health_spending)

names <- c(1:3)
health_spending[,names] <- lapply(health_spending[,names],factor)

# Check if there is any missing value in the dataframe

summary(health_spending)
sum(is.na(health_spending))






# ------------------------- < Data Analysis > ------------------------

# Question 1

# 1.What is healthcare spending trend over the years for Ireland?

ireland_healthcare <- filter(health_spending,LOCATION=='IRL' & MEASURE=='USD_CAP')

# View(ireland_healthcare)
dim(ireland_healthcare)
summary(ireland_healthcare)

ggplot(ireland_healthcare, aes(x = TIME, y = Value, color = SUBJECT)) +
        geom_line(size = 1) +
        geom_point(size = 2)+
        scale_x_continuous(breaks = seq(1970, 2020, by=5))+
        scale_y_continuous(breaks = seq(0, 6000, by=500))+
        labs(title = 'Health Care Spending Trend of Ireland Over Time', y='Value in USD per Capita')



   


     
# Question 2

# 2.Which countries have the highest healthcare spending in 2019? Which type of expenditure contributes the most for health spending last year (2019)?

highest_healthcare_countries <- filter(health_spending, TIME==2019 & MEASURE=='PC_GDP')

highest_healthcare_countries <- filter(highest_healthcare_countries, SUBJECT!='TOT')

total_value <- sqldf("select LOCATION,
                         sum(Value) as Total_Value 
                         from highest_healthcare_countries
                         group by LOCATION
                         order by Total_Value desc")

combined_data <- merge(highest_healthcare_countries, total_value, by = c('LOCATION'), all = T)

length(unique(combined_data$LOCATION))

ggplot(combined_data, aes(x = reorder(LOCATION,-Total_Value), y = Value, fill = SUBJECT)) +
        geom_bar(stat = 'identity', color='black', alpha=0.8) +
        theme(axis.text.x = element_text(angle = 90))+
        scale_y_continuous(breaks = seq(0, 17.5, by=2.5))+
        labs(title = 'Health Spending Ranking among Countries in 2019', x='Countries', y='Value in Percentage of GDP')







# Question 3

# 3. What is healthcare spending trend in Europe over the years?

data(World)

gall_world <- st_transform(World, crs = "+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

url_worldshp <- 'https://github.com/Lan-hub-1005/Data-Visualisation/blob/main/world.shp?raw=true'
download.file(url_worldshp,'world.shp')
url_worldshx <- 'https://github.com/Lan-hub-1005/Data-Visualisation/blob/main/world.shx?raw=true'
download.file(url_worldshx,'world.shx')
url_worldprj <- 'https://github.com/Lan-hub-1005/Data-Visualisation/raw/main/world.prj'
download.file(url_worldprj,'world.prj')
url_worlddbf <- 'https://github.com/Lan-hub-1005/Data-Visualisation/blob/main/world.dbf?raw=true'
download.file(url_worlddbf,'world.dbf')
url_worldcpg <- 'https://github.com/Lan-hub-1005/Data-Visualisation/raw/main/world.cpg'
download.file(url_worldcpg,'world.cpg')

my_map <- st_read('world.shp', stringsAsFactors=FALSE)


# Only Europe Data

europe_countries <- c('Austria','Belgium','Bulgaria','Croatia','Cyprus','Czechia','Denmark','Estonia',
                      'Finland','France','Germany','Greece','Hungary','Ireland','Italy','Latvia','Lithuania',
                      'Luxembourg','Malta','Netherlands','Poland','Portugal','Romania','Slovakia','Slovenia',
                      'Spain','Sweden','United Kingdom')

# Use Total Value MEASURE

countries_over_time <- health_spending %>%
  filter(MEASURE=='USD_CAP') %>%
  filter(TIME>=1990) %>%
  filter(SUBJECT!='TOT')

total_value_countries_over_time <- sqldf("select LOCATION, TIME,
                         sum(Value) as Total_Value 
                         from countries_over_time
                         group by LOCATION,TIME
                         order by TIME")

countries_combined_data <- total_value_countries_over_time %>%
  mutate(CNTRY_NAME=countrycode(LOCATION,
                                origin = 'iso3c',destination = 'country.name'))
map_and_data_question3 <- inner_join(my_map,countries_combined_data)

map_and_data_europe_question3 <- filter(map_and_data_question3,`CNTRY_NAME` %in% europe_countries)

length(unique(map_and_data_europe_question3$`CNTRY_NAME`))

# Plot the dynamic graph 

europe <- tm_shape(map_and_data_europe_question3)+
  tm_borders()+
  tm_fill(col = 'CNTRY_NAME')+
  tm_bubbles(col = 'black',border.col = 'white',size = 'Total_Value',alpha=0.6)+
  tm_layout(title='Health Spending Trend in Europe')+
  tm_facets(along = 'TIME',free.coords = FALSE)

tmap_animation(europe,filename = 'europe.gif',delay = 40)











# ---------------------------- Previous Versions tried for Question 3 -----------------------------------------------------

# ----- Version 1 -----
country_name_combined_data <- combined_data %>%
  mutate(CNTRY_NAME=countrycode(LOCATION,origin = 'iso3c',destination = 'country.name'))

map_and_data <- inner_join(my_map,country_name_combined_data)

tm_shape(gall_world) +
  tm_polygons()+
  tm_shape(map_and_data)+
  tm_fill(col = 'CNTRY_NAME')+
  tmap_options(max.categories = 41)+
  tm_symbols(col = 'black',border.col = 'white',size = 'Total_Value')+
  tm_layout(title='Health Spending in 2019')

# Warning Message we got from this:
# Warning message:
# Number of levels of the variable "CNTRY_NAME" is 41, 
# which is larger than max.categories (which is 30), so levels are combined. 
# Set tmap_options(max.categories = 41) in the layer function to show all levels. 




# ------- Version 2 --------
map_and_data_europe <- filter(map_and_data,`CNTRY_NAME` %in% europe_countries)

length(unique(map_and_data_europe$`CNTRY_NAME`))

tm_shape(map_and_data_europe)+
  tm_borders()+
  tm_fill(col = 'CNTRY_NAME')+
  tm_bubbles(col = 'black',border.col = 'white',size = 'Total_Value',alpha=0.6)+
  tm_layout(title='Health Spending in 2019')





# ------- Version 3 ------
countries_over_time <- health_spending %>%
  filter(MEASURE=='PC_GDP') %>%
  filter(TIME>=1990) %>%
  filter(SUBJECT!='TOT')

total_value_countries_over_time <- sqldf("select LOCATION, TIME,
                         sum(Value) as Total_Value 
                         from countries_over_time
                         group by LOCATION,TIME
                         order by TIME")

countries_combined_data <- total_value_countries_over_time %>%
  mutate(CNTRY_NAME=countrycode(LOCATION,
                                origin = 'iso3c',destination = 'country.name'))

map_and_data_question3 <- inner_join(my_map,countries_combined_data)

map_and_data_europe_question3 <- filter(map_and_data_question3,`CNTRY_NAME` %in% europe_countries)

length(unique(map_and_data_europe$`CNTRY_NAME`))

tm_shape(map_and_data_europe_question3)+
  tm_borders()+
  tm_fill(col = 'CNTRY_NAME')+
  tm_bubbles(col = 'black',border.col = 'white',size = 'Total_Value',alpha=0.6)+
  tm_layout(title='Health Spending Trend in Europe')+
  tm_facets(by = 'TIME',nrow=10, free.coords = FALSE)
