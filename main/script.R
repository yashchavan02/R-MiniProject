# <- Install and load packages ->

install.packages(c("dplyr", "tidyr", "ggplot2")) # Install multiple packages at once
library(dplyr)
library(tidyr)
library(ggplot2)

# <- Load & Read Data ->

path = "public/ZomatoDataset.csv" 
data = read.csv(path)

# <- Explore Data ->

View(data)
head(data)
tail(data)
names(data)

# <- Categorical Data Exploration -> 

# List unique values for categorical columns
unique(data$Festival)
unique(data$Weather_conditions)
unique(data$Vehicle_condition)
unique(data$Type_of_order)
unique(data$Type_of_vehicle)
unique(data$multiple_deliveries)
unique(data$City)

# <- Data Cleaning ->

# Drop unwanted columns 
data = data %>%
  select(-c(Delivery_person_ID, Restaurant_latitude, Restaurant_longitude, 
            Delivery_location_latitude, Delivery_location_longitude))

# Separate the Order Date column into Day, Month, Year
data = separate(data, col = Order_Date, into = c("Day", "Month", "Year"), sep = "-")

# Drop unnecessary columns
data = data %>% select(-Day, -Year)

# Calculate time required to accept customer order
data = data %>%
  mutate(Time_Orderd = as.POSIXct(Time_Orderd, format = "%H:%M"),
         Time_Order_picked = as.POSIXct(Time_Order_picked, format = "%H:%M"),
         Time = as.numeric(difftime(Time_Order_picked, Time_Orderd, units = "mins"))) %>%
  select(-Time_Orderd, -Time_Order_picked)

# Handle NaN values
data[data == "NaN"] = NA
data = data %>%
  filter(!is.na(Festival) & !is.na(Weather_conditions) & !is.na(City) & 
           !is.na(multiple_deliveries) & !is.na(Delivery_person_Age) & 
           !is.na(Delivery_person_Ratings) & !is.na(Road_traffic_density) & 
           !is.na(Month) & !is.na(Vehicle_condition) & 
           !is.na(Type_of_vehicle) & !is.na(Type_of_order) & 
           !is.na(Time_taken..min.) & !is.na(Time))

# Remove outliers using the dplyr package
remove_outliers = function(x) {
  lower_bound = quantile(x, 0.25) - 1.5 * IQR(x)
  upper_bound = quantile(x, 0.75) + 1.5 * IQR(x)
  x[x < lower_bound | x > upper_bound] = NA  # Replace outliers with NA
  return(x)
}

data_no_outliers = data
data_no_outliers[sapply(data_no_outliers, is.numeric)] = lapply(data_no_outliers[sapply(data_no_outliers, is.numeric)], remove_outliers)

# <- Visualizations ->

# Problem Statement 1
# Bar Plot
ggplot(data_no_outliers, aes(Road_traffic_density, Time_taken..min.)) + 
  geom_col(fill = 'red') + 
  labs(title = "Time Taken by Road Traffic Density", 
       x = "Road Traffic Density", 
       y = "Time Taken (min)") +
  
  
  # Box Plot
  ggplot(data_no_outliers, aes(Road_traffic_density, Time_taken..min.)) + 
  geom_boxplot(fill = 'lightblue') + 
  labs(title = "Box Plot of Road Traffic Density by Time Taken (min)", 
       x = "Road Traffic Density", 
       y = "Time Taken (min)") +
  
  
  #problem statement 2
  ggplot(data, aes(Weather_conditions, Time_taken..min.)) + 
  geom_point(size=4, color='blue') + 
  labs(title="Scatter Plot ", x="Weather conditions", y="Time taken in min")

ggplot(data, aes(Weather_conditions))+geom_bar(fill = "green")

#problem statement 3
# Convert the Time_taken..min. column to numeric if it isn't already
data$Time = as.numeric(data$Time)

# Create the bar plot with y-axis limits from 0 to 20 minutes
ggplot(data, aes(Type_of_order, Time)) + 
  geom_boxplot(fill='blue') + 
  labs(title="Box Plot of Type of Order vs Average Time Taken", x="Type of Order", y="Time Taken to accept order") 

ggplot(data, aes(x = Type_of_order)) +
  geom_bar(binwidth = 5, fill = "yellow", color = "white") +
  labs(title = "Histogram of Type_of_order", x = "Type_of_order")

#problem statement 4
ggplot(data, aes(Delivery_person_Ratings, Time_taken..min.)) + 
  geom_line(color='red') + 
  labs(title="Box Plot of Delivery person Ratings by Time Taken (min)", x="Delivery person Ratings", y="Time Taken (min)")

#problem statement 5
ggplot(data, aes(Delivery_person_Age, Delivery_person_Ratings))+ 
  geom_point(size=5) +
  geom_line(color='red')


ggplot(data, aes(x = Month)) +
  geom_bar(binwidth = 5, fill = "yellow", color = "white") +
  labs(title = "Histogram of Type_of_order", x = "Type_of_order")


#objective 2
festival_traffic_summary = data %>%
  group_by(Festival, Road_traffic_density) %>%
  summarise(avg_time_taken = mean(Time_taken..min., na.rm = TRUE),
            avg_rating = mean(Delivery_person_Ratings, na.rm = TRUE)) %>%
  ungroup()

# View the summary
print(festival_traffic_summary)

# Filter data for festival and nonfestival times only
festival_data = data %>%
  filter(Festival == "Yes")
nonfestival_data = data %>%
  filter(Festival == "No")
# Correlation between delivery time and rating during festivals and regular
cor(festival_data$Time_taken..min., festival_data$Delivery_person_Ratings, use = "complete.obs")
cor(nonfestival_data$Time_taken..min., nonfestival_data$Delivery_person_Ratings, use = "complete.obs")

#Objective 3
# necessary packages dplyr
# Select the relevant columns
model_data = data %>%
  filter(!is.na(Time_taken..min.), !is.na(Delivery_person_Ratings)) %>%  
  select(Time_taken..min., Delivery_person_Ratings, Road_traffic_density, Festival, Weather_conditions)

# Convert categorical variables to factors
model_data$Road_traffic_density = as.factor(model_data$Road_traffic_density)
model_data$Festival = as.factor(model_data$Festival)
model_data$Weather_conditions = as.factor(model_data$Weather_conditions)

# Install and load the required package
install.packages("caTools")
library(caTools)

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # For reproducibility
split = sample.split(model_data$Time_taken..min., SplitRatio = 0.7)
train_data = subset(model_data, split == TRUE)
test_data = subset(model_data, split == FALSE)

# Fit the linear regression model
model = lm(Time_taken..min. ~ Delivery_person_Ratings + Road_traffic_density + Festival + Weather_conditions, data = train_data)

# Summary of the model to check coefficients and significance levels
summary(model)

# Predict delivery time on the test set
predictions = predict(model, newdata = test_data)

# Compare predicted and actual values
results = data.frame(Actual = test_data$Time_taken..min., Predicted = predictions)

# Print the first few rows
head(results)
