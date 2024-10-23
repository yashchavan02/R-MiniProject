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

#problem statement 1
ggplot(data, aes(Road_traffic_density, Time_taken..min.)) + 
  geom_col(fill='red') 
labs(title="Time taken in min by Delivery Person's Age", x="Road traffic density", y="Time taken in min")

ggplot(data, aes(Road_traffic_density, Time_taken..min.)) + 
  geom_boxplot(fill='lightblue') + 
  labs(title="Box Plot of Road Traffic Density by Time Taken (min)", x="Road Traffic Density", y="Time Taken (min)")

#problem statement 2
ggplot(data, aes(Weather_conditions, Time_taken..min.)) + 
  geom_point(size=4, color='blue') + 
  labs(title="Scatter Plot ", x="Weather conditions", y="Time taken in min")

ggplot(data, aes(Weather_conditions))+geom_bar(fill = "green")

#problem statem 3
# Convert the Time_taken..min. column to numeric if it isn't already
data$Time = as.numeric(data$Time)

# Create the bar plot with y-axis limits from 0 to 20 minutes
ggplot(data, aes(Type_of_order, Time)) + 
  geom_boxplot(fill='blue') + 
  labs(title="Box Plot of Type of Order vs Average Time Taken", x="Type of Order", y="Time Taken to accept order") 

ggplot(data, aes(x = Type_of_order)) +
  geom_pieplot(binwidth = 5, fill = "yellow", color = "white") +
  labs(title = "Histogram of Type_of_order", x = "Type_of_order")

#problem statement 4
ggplot(data, aes(Delivery_person_Ratings, Time_taken..min.)) + 
  geom_line(color='red') + 
  labs(title="Box Plot of Delivery person Ratings by Time Taken (min)", x="Delivery person Ratings", y="Time Taken (min)")

#problem statement 5
ggplot(data, aes(Delivery_person_Age, Delivery_person_Ratings))+ 
  geom_point(size=5) +
  geom_line(color='red')