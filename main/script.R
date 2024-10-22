# Zomato data (2022) analysis using R 

# <- install and load packages ->

install.packages("dplyr")
install.packages("tidyr")

library(tidyr)
library(dplyr)
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
data = data %>% filter(!is.na(Festival) & !is.na(Weather_conditions) & !is.na(City) & !is.na(multiple_deliveries) & !is.na(Delivery_person_Age) & !is.na(Delivery_person_Ratings) & !is.na(Road_traffic_density) )
data = data %>% filter(!is.na(Vehicle_condition) & !is.na(Type_of_vehicle) & !is.na(Type_of_order) & !is.na(Time_taken..min.))
unique(data $ Festival)
unique(data $ Weather_conditions)
unique(data $ City)
unique(data $ multiple_deliveries)

glimpse(data)

# separate the Order date column

data = separate(data,col=Order_Date,int=c("Day","Month","Year"),sep="-")
data = data %>% filter(!is.na(Month))

# drop unnecessary columns

data = data %>% select(-Day)
data = data %>% select(-Year)

glimpse(data)

# calculate time required to accept customer order

data = data %>%
  mutate(Time_Orderd = as.POSIXct(Time_Orderd, format = "%H:%M"),
         Time_Order_picked = as.POSIXct(Time_Order_picked, format = "%H:%M"),
         Time = as.numeric(difftime(Time_Order_picked, Time_Orderd, units = "mins")))

data = data %>% filter(!is.na(Time))

# drop unnecessary columns

data = data %>% select(-Time_Orderd)
data = data %>% select(-Time_Order_picked)

glimpse(data)

# Removing outliers using dplyr package

remove_outliers = function(x) {
  lower_bound = quantile(x, 0.25) - 1.5 * IQR(x)
  upper_bound = quantile(x, 0.75) + 1.5 * IQR(x)
  x[x < lower_bound | x > upper_bound] = NA  # Replace outlier with NA
  return(x)
}

data_no_outliers = data
data_no_outliers[sapply(data_no_outliers, is.numeric)] = lapply(data_no_outliers[sapply(data_no_outliers, is.numeric)], remove_outliers)

View(data)

glimpse(data)
