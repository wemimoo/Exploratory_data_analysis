# Exploratory_data_analysis

---
title: "Forestfire project"


#*INTRODUCTION*

This project explores the relationship of several factors with forest fires.

```{r}
library(tidyverse)
forestfire <- read_csv("forestfires.csv")
dim(forestfire) 
##The dataset contains 517 rows of 13 variables
glimpse(forestfire)

##check for missing values for each variable
#for (i in colnames(forestfire)) {
#  print("The number of missing values")
#  print(i)
#  print(sum(is.na(forestfire[i])))
#}

### No variable has missing values



```
#*Description of variables*
X: X-axis spatial coordinate within the Montesinho park map: 1 to 9
Y: Y-axis spatial coordinate within the Montesinho park map: 2 to 9
month: Month of the year: 'jan' to 'dec'
day: Day of the week: 'mon' to 'sun'
FFMC: Fine Fuel Moisture Code index from the FWI system: 18.7 to 96.20
DMC: Duff Moisture Code index from the FWI system: 1.1 to 291.3
DC: Drought Code index from the FWI system: 7.9 to 860.6
ISI: Initial Spread Index from the FWI system: 0.0 to 56.10
temp: Temperature in Celsius degrees: 2.2 to 33.30
RH: Relative humidity in percentage: 15.0 to 100
wind: Wind speed in km/h: 0.40 to 9.40
rain: Outside rain in mm/m2 : 0.0 to 6.4
area: The burned area of the forest (in ha): 0.00 to 1090.84


```{r}
###Further exploration of the dataset
##month variable
forestfire %>% pull(month) %>% unique
## this variable should be a factor variable 
forestfire <- forestfire %>% 
  mutate(factor_month = factor(month,
                               levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")))
class(forestfire$factor_month)

##check the day variable
forestfire %>% pull(day) %>% unique
###convert to factor
forestfire <- forestfire %>%
  mutate(factor_day = factor(day,
                             levels = c("sun", "mon", "tue", "wed", "thu", "fri", "sat")))
class(forestfire$factor_day)

```

##*Which months do forest fires happen the most?
##*Which days of the week do forest fires happen the most?
```{r}
###count the number of fire by month
countby_month <- forestfire %>% 
  group_by(factor_month) %>%
  summarise(fires_by_month = n())

###use graphs to visualise the number of fires by month
countby_month %>%
  ggplot(aes(x= factor_month, y = fires_by_month)) +
  geom_col() +
  labs(title = "Forest fire by month",
       x = "month", 
       y = "number of fires") +
  theme(panel.background = element_rect(fill ="white"))

```

###**Most forest fires occur in August and September**

```{r}
## Count forest forest by the day of the week
countby_day <- forestfire %>%
  group_by(factor_day) %>%
  summarise(fires_by_weekday = n())

###use graphs to visualise the number of forest fires by day of the week
countby_day %>%
  ggplot(aes(x = factor_day, y = fires_by_weekday)) +
  geom_col() +
  labs(title = "Forest fire by Weekday",
       x = "month", 
       y = "number of fires") +
  theme(panel.background = element_rect(fill ="white"))
```
###**Forest fires are more frequent on Weekends than Weekdays.**


##*Explore temporal trend in the dataset*
The relationship between the other variables and months of the year.

```{r}
##Since I am looking at several columns, it is better to change to the long format

forestfire2 <- forestfire %>%
  pivot_longer(c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain"),
               names_to = "explain_var",
               values_to = "values")
head(forestfire2, 10)

###using this new variable, explore the relationship between each variable and month

forestfire2 %>% 
  ggplot(aes(x = factor_month, y = values)) + 
  geom_col() +
  facet_wrap(vars(explain_var), scale = "free_y") +
  theme(panel.background = element_blank()) +
  labs(x = "Month",
       y = "Values of columns",
       title = "Relationships between other variables and months")

```
###**Levels of ISI, DMC, FFMC, ISI, RH, TEMP AND WIND are highest in August and September. There is more rain in August compared with other months**


##*Relationship between the variables in the data and the frequency of forest fires.
Area will be used as a proxy for severity of forest fire here.

```{r}
#This code plots the relationship between area and each of the following #variables: FFMC, DMC,DC, ISI, temp, RH, wind, rain.
head(forestfire2)

forestfire2 %>% 
  ggplot(aes(x = values, y = area)) +
  geom_point() +
  facet_wrap(vars(explain_var), scales = "free_x") +
  theme(panel.background = element_blank()) +
  labs(
    title = "Relationship between other variables and area",
    x = "value of column", 
    y = "Area burned (hectare"
  )


```
###**There are a few outliers in this dataset** 
No clear pattern of association with area is evident for any of these variables. This may be because of the presence of outliers in the areas dataset. 

##View distribution of area
```{r}
forestfire2 %>% 
  ggplot(aes(x = area)) +
  geom_histogram() +
  theme(panel.background = element_blank())
```

##Restrict the analysis fires with area < 250

```{r}
forestfire2 %>% 
  filter(area <= 250) %>%
  ggplot(aes(x = values, y = area)) +
  geom_point() +
  facet_wrap(vars(explain_var), scale = "free_x") +
  theme(panel.background = element_blank()) +
  labs(
    title = "Relationship between other variables and area (area < 250)",
    x = "value of column", 
    y = "Area burned (hectare"
  )
```
