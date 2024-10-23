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
names(data)

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

names(data)


# separate the Order date column

data = separate(data,col=Order_Date,int=c("Day","Month","Year"),sep="-")

# drop unnecessary columns

data = data %>% select(-Day)
data = data %>% select(-Year)

names(data)

# calculate time required to accept customer order

data = data %>%
  mutate(Time_Orderd = as.POSIXct(Time_Orderd, format = "%H:%M"),
         Time_Order_picked = as.POSIXct(Time_Order_picked, format = "%H:%M"),
         Time = as.numeric(difftime(Time_Order_picked, Time_Orderd, units = "mins")))

# drop unnecessary columns

data = data %>% select(-Time_Orderd)
data = data %>% select(-Time_Order_picked)

names(data)

# drop rows where data is NaN

data[data == "NaN"] = NA
data = data %>% filter(!is.na(Festival) & !is.na(Weather_conditions) & !is.na(City) & !is.na(multiple_deliveries) & !is.na(Delivery_person_Age) & !is.na(Delivery_person_Ratings) & !is.na(Road_traffic_density) )
data = data %>% filter(!is.na(Month) & !is.na(Vehicle_condition) & !is.na(Type_of_vehicle) & !is.na(Type_of_order) & !is.na(Time_taken..min.) & !is.na(Time))
unique(data $ Festival)
unique(data $ Weather_conditions)
unique(data $ City)
unique(data $ multiple_deliveries)

names(data)

#Removing outliears using dplyr package
remove_outliers = function(x) {
  lower_bound = quantile(x, 0.25) - 1.5 * IQR(x)
  upper_bound = quantile(x, 0.75) + 1.5 * IQR(x)
  x[x < lower_bound | x > upper_bound] = NA  # Replace outliers with NA
  return(x)
}

data_no_outliers = data
data_no_outliers[sapply(data_no_outliers, is.numeric)] = lapply(data_no_outliers[sapply(data_no_outliers, is.numeric)], remove_outliers)

View(data)

names(data)

#plot Graph using ggplot
install.packages("ggplot2")
library(ggplot2)
names(data)

#Person Ratings by their Age
ggplot(data, aes(Delivery_person_Age, Delivery_person_Ratings))+ 
  geom_point(size=5) +
  geom_line(color='red')

ggplot(data, aes(x = City, y = Time_taken..min.)) + geom_point()
  
  names(data)

#type of order by person count
ggplot(data,aes(Type_of_order)) +
  geom_histogram(stat="count")

#count for multiple deliveries
ggplot(data,aes(multiple_deliveries)) +
  geom_histogram(stat="count")

#number of orders in festival period
ggplot(data,aes(Festival)) +
  geom_bar(stat="count")

ggplot(data, aes(Weather_conditions))+geom_bar()
ggplot(data, aes(Road_traffic_density))+geom_bar(fill = "blue")

#y = "Time_taken..min.")