# Zomato data (2022) analysis using R 

# <- install and load packages ->

install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)

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
data = data %>% filter(!is.na(Festival) & !is.na(Weather_conditions) & !is.na(City) & !is.na(multiple_deliveries))

unique(data $ Festival)
unique(data $ Weather_conditions)
unique(data $ City)
unique(data $ multiple_deliveries)

glimpse(data)

# separate the Order date column

data = separate(data,col=Order_Date,int=c("Day","Month","Year"),sep="-")

# drop unnecessary columns

data = data %>% select(-Day)
data = data %>% select(-Year)

glimpse(data)

# calculate time required to accept customer order

data = data %>%
  mutate(Time_Orderd = as.POSIXct(Time_Orderd, format = "%H:%M"),
         Time_Order_picked = as.POSIXct(Time_Order_picked, format = "%H:%M"),
         Time = as.numeric(difftime(Time_Order_picked, Time_Orderd, units = "mins")))

# drop unnecessary columns

data = data %>% select(-Time_Orderd)
data = data %>% select(-Time_Order_picked)

glimpse(data)
