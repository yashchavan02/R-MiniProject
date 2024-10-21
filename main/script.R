# Zomato data analysis using R

# <- install and load packages ->

install.packages("dplyr")
library(dplyr)

# <- load & read data ->

path = "public/ZomatoDataset.csv"
data = read.csv(path)

# <- explore data ->

View(data) 
head(data)
tail(data)
glimpse(data)

# <- categorical data exploration -> 

unique(data $ Festival)
unique(data $ Weather_conditions)
unique(data $ Vehicle_condition)
unique(data $ Type_of_order)
unique(data $ Type_of_vehicle)
unique(data $ multiple_deliveries)
unique(data $ City)

# <- data cleaning ->

# drop unwanted columns 

data = data %>% select(-Delivery_person_ID)
data = data %>% select(-Restaurant_latitude)
data = data %>% select(-Restaurant_longitude)
data = data %>% select(-Delivery_location_latitude)
data = data %>% select(-Delivery_location_longitude)

glimpse(data)

# drop rows where data is NaN

data[data == "NaN"] = NA
data = data %>% filter(!is.na(Festival) & !is.na(Weather_conditions) & !is.na(City))

unique(data $ Festival)
unique(data $ Weather_conditions)
unique(data $ City)

glimpse(data)


