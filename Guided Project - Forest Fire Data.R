
library(readr)
library(dplyr)
library(ggplot2)
library(purrr)

forest = read.csv("C:/Users/micha/Downloads/forestfires.csv") # This is my home directory to retrieve file, your's will differ depending on what you set as your home directory
attach(forest)
View(forest)



# QUESTION 1: Asking which months are forest fires most common?
# Part A) Need to re-order the months to make sure it is not in alphabetical order 
forest$month <- factor(forest$month, levels =c('jan','feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'), labels = c(1,2,3,4,5,6,7,8,9,10,11,12))
forest$month <- factor(forest$month, levels =c(1,2,3,4,5,6,7,8,9,10,11,12), labels = c('jan','feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))

# Part B) Create a table to tell which months had the most fires
fires_by_month = forest %>% group_by(month) %>% summarize(total_fires = n())
fires_by_month

# Part C) Make the Bar chart 
ggplot(data = fires_by_month, aes(x = month, y = total_fires, fill = month)) + geom_bar(stat = 'identity', show.legend = FALSE) + labs(title = 'Number of Forest Fires Based on Calendar Months', x = "Months", y = "Number of Forest Fires") + theme(panel.background = element_rect('white'))

# Based on the chart, it appears as though most forest fires occur during the late summer months (i.e. August and September)



# QUESTION 2: Asking which days are forest fires most common?
# Part A) Need to re-order the days to make sure it is not in alphabetical order 
forest$day <- factor(forest$day, levels =c('mon','tue', 'wed', 'thu', 'fri', 'sat', 'sun'), labels = c(1,2,3,4,5,6,7))
forest$day <- factor(forest$day, levels =c(1,2,3,4,5,6,7), labels = c('mon','tue', 'wed', 'thu', 'fri', 'sat', 'sun'))

# Part B) Create a table to tell which days had the most fires
fires_by_day = forest %>% group_by(day) %>% summarize(total_fires = n())
fires_by_day

# Part C) Make the Bar chart 
ggplot(data = fires_by_day, aes(x = day, y = total_fires, fill = day)) + geom_bar(stat = 'identity', show.legend = FALSE) + labs(title = 'Number of Forest Fires Based on Calendar Days', x = "Days", y = "Number of Forest Fires") + theme(panel.background = element_rect('white'))

# Based on the chart, it appears as though most forest fires occur during the weekend (i.e. Friday to Sunday)



# Question 3: What are the influence of other variables onto number of forrest fires based on months and days
# PART A) Create a function to make boxplots
create_boxplot <- function(x, y) {
  ggplot(data = forest) +
    aes_string(x = x, y = y) +
    geom_boxplot() +
    theme(panel.background =element_rect(fill = 'white'))
}

# PART B) create variables for boxplots
x_var_month = names(forest)[3]
x_var_day = names(forest)[4]
y_var = names(forest)[5:12]

# PART C) boxplots 
month_box = map2(x_var_month, y_var, create_boxplot)
month_box

day_box = map2(x_var_day, y_var, create_boxplot)
day_box

# Looking across the days, it seems that the median values are consistent across variables. 
# Additionally, the sie of the boxes are also consistent. 

# However, as it pertains to the months, there are some notable differences. 
# As it relates to temperature, the summer months appears to be high and seems to coincide with the number of forest fires. 
# This also coincides with the drought code where the dry conditions were also high during the summer months. 



# Question #4: What is the influence of other variables on forest fire areas
create_scatter = function(x, y) {
  ggplot(data = forest) +
    aes_string(x = x, y = y) +
    geom_point(alpha = 0.3) + 
    theme(panel.background = element_rect('white'))
}

x_var_scatter = names(forest)[5:12]
y_var_scatter = names(forest)[13]

scatter = map2(x_var_scatter, y_var_scatter, create_scatter)
scatter

# Looking at the distribution, we see that most of the values lie near the bottom of the chart. 
# THis would make sense since most of the burned forest areas are less than 100 ha. 

ggplot(data = forest, aes(x = area)) + geom_histogram(bins = 100)



# Examining only rows with high value areas (i.e. 10 Ha)

month_plus_10 <- month[area > 10]
day_plus_10 = day[area > 10]
FFMC_plus_10 = FFMC[area > 10]
DMC_plus_10 = DMC[area > 10]
DC_plus_10 = DC[area > 10]
ISI_plus_10 = ISI[area > 10]
temp_plus_10 = temp[area > 10]
RH_plus_10 = RH[area > 10]
Wind_plus_10 = wind[area > 10]
rain_plus_10 = rain[area > 10]
area_plus_10 = area[area > 10]

forest_plus_10 = data.frame(
  month_plus_10, 
  day_plus_10, 
  FFMC_plus_10, 
  DMC_plus_10, 
  DC_plus_10, 
  ISI_plus_10, 
  temp_plus_10, 
  RH_plus_10, 
  Wind_plus_10, 
  rain_plus_10, 
  area_plus_10)


create_scatter_plus10 = function(x, y) {
  ggplot(data = forest_plus_10) +
    aes_string(x = x, y = y) +
    geom_point(alpha = 0.3) + 
    theme(panel.background = element_rect('white'))
}

x_var_scatter = names(forest_plus_10)[3:10]
y_var_scatter = names(forest_plus_10)[11]

scatter_plus10 = map2(x_var_scatter, y_var_scatter, create_scatter_plus10)
scatter_plus10



# Exploring only 0 Ha areas

month_zero <- month[area == 0]
day_zero = day[area == 0]
FFMC_zero = FFMC[area == 0]
DMC_zero = DMC[area == 0]
DC_zero = DC[area == 0]
ISI_zero = ISI[area == 0]
temp_zero = temp[area == 0]
RH_zero = RH[area == 0]
Wind_zero = wind[area == 0]
rain_zero = rain[area == 0]
area_zero = area[area == 0]

forest_zero = data.frame(
  month_zero, 
  day_zero, 
  FFMC_zero, 
  DMC_zero, 
  DC_zero, 
  ISI_zero, 
  temp_zero, 
  RH_zero, 
  Wind_zero, 
  rain_zero, 
  area_zero)


create_scatter_zero = function(x, y) {
  ggplot(data = forest_zero) +
    aes_string(x = x, y = y) +
    geom_point(alpha = 0.3) + 
    theme(panel.background = element_rect('white'))
}

x_var_scatter = names(forest_zero)[3:10]
y_var_scatter = names(forest_zero)[11]

scatter_zero = map2(x_var_scatter, y_var_scatter, create_scatter_zero)
scatter_zero