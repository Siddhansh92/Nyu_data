#UNDERSTANDING UBER DATA

#Check str
str(Uber_Request_Data)

#Get the summary
summary(Uber_Request_Data)

#Loading library - Tidyverse
library(tidyverse)

#Saving Uber's data in a new variable for analysis:
Uber_data <- Uber_Request_Data

#------------------------------------------------#

#DATA CLEANING

#Coverting dates to the right format
Uber_data$`Request timestamp` <- as.POSIXlt(Uber$`Request timestamp`, format = "%m/%d/%Y-%H:%M:%S")
Uber_data$`Drop timestamp` <- as.POSIXlt(Uber$`Drop timestamp`, format = "%m/%d/%Y-%H:%M:%S")

#DERIVING NEW VARIABLES FOR ANALYSIS
#Deriving the hour column for the analysing the demand for uber for each hour of the day
#Converting the new hour variable to numeric form for computing the values
Uber_data$Request_Hour <- format(Uber_data$`Request timestamp`, "%H")
Uber_data$Request_Hour = as.numeric(Uber_data$Request_Hour)

Uber_data$Date <- as.Date(Uber_data$`Request timestamp`)
#Get days data as a seperate column

#Renaming columns in Uber_data for ensuring ggplots work

#IDENTIFICATION OF THE PROBLEM
# Plot the number of cabs requested in a particular hour for all 05 days

# Pickup points will be displayed in two colors
count_hourly_requests <- ggplot(Uber_data, aes(x = factor(Request_Hour), fill = factor(Pickup_point)))
#Add title and lables to the plot and save it as a object
Plot_by_hour <- count_hourly_requests + geom_bar(stat='count', position = "dodge") + ggtitle("Hourly Demand for Uber Cabs") + labs(x="Time in Hours", y="Number of Cabs Requested") + labs(fill="Pickup_point")

#View Plot_1
Plot_by_hour

# Generate a sequence of numbers from 0 to 23 

# Save it as a vector
Request_Hour <- c(0:23)

# Naming of time slots
Time_Slot2 <- c("Early_Morning","Morning","Afternoon","Evening","Late_Night")

# Creating a vector which represents the number of times the slots must repeat
# The number of elements in this vector should correspond to 24
Times <- c(4,6,7,5,2)
Time_Slot <- rep(Time_Slot2, Times)

# Creating a new dataframe with the sequence of number generated for the time slots
Uber_extension <- data.frame(Time_Slot, Request_Hour)

# Merging the main uber request dataframe with the newly create extension df 
Uber_data <- merge(Uber_data, Uber_extension, by="Request_Hour", all.x=TRUE)

#Reorganising the Uber data
Uber_data <- Uber_data[ , c(2,6,3,5,4,7,8,9,1)]

#Subset the Uber data by Trips completed
Trips_Completed <- subset(Uber_data, Uber_data$Status == "Trip Completed")

# Plot 2 - A bar chart with time slots on x-axis and request frequency on y-axis
Plot_Timeslot <- ggplot(Trips_Completed, aes(x = Time_Slot))

Plot2_Timeslot <- Plot_Timeslot + geom_bar(stat="count", col="red", fill="grey") 
+ ggtitle("Trips completed by Time Slots") 
+ labs(x ="Time Slots", y="Trips Completed") 
+ geom_text(stat = 'count', aes(label=..count..), vjust = -1) 
+ guides(fill = FALSE) + scale_x_discrete(limits=c("Morning","Evening","Afternoon","Late_Night", "Early_Morning"))


# Plot 3 - A bar chart with time slots on x-axis and request frequency on y-axis
# Showing the status of requests in different colors

Timeslot_Request <- ggplot(Uber_data, aes(x = factor(Time_Slot), fill = factor(Status)))
Plot3_Timeslot_Request <- Timeslot_Request + geom_bar(stat = "count", position = "stack", col="black") + ggtitle("Trips At Different Time Slots") + scale_x_discrete(limits=c("Evening","Morning","Afternoon","Late_Night","Early_Morning")) + labs(x ="Time Slots", y = "Number of Requests") + labs(fill = "Trip Status") + scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))

#--------------------------------#

#PROBLEM 1 - Numerous service requests got cancelled during the Morning time slot

#Subset the Morning time slot for analysis
Problem1 <- subset(Uber_data, Uber_data$Time_Slot == "Morning")

#Plot the bargraph with status of request in x-axis and count in y-axis for Morning rush time slot
#Show the request from different pickup points in different colors

Problem1_Count <- ggplot(Problem1, aes(x = factor(Status), fill = factor(Pickup_point)))

Problem1_Plot <- Problem1_Count + geom_bar(stat ="count", position = "stack") + ggtitle("Morning Cab Status") + labs(x = "Status", y = "Total count") + labs(fill= "Pickup_Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))

#Number of trips cancelled for the Morning rush time slot
total_trips_cancelled <- length(which(Problem1$Status=="Cancelled"))

#Number of trips cancelled from airport for Morning rush
airport_trips_cancelled <- length(which((Problem1$Pickup_point == "Airport") & (Problem1$Status == "Cancelled")))

# Number of trips cancelled from city for Morning rush
city_trips_cancelled <- length(which((Problem1$Pickup_point == "City") & (Problem1$Status == "Cancelled")))

# Percentage of trips cancelled from city out of total trips cancelled during morning rush
per_trip_cancelled_city <- (city_trips_cancelled/total_trips_cancelled * 100)

# Percentage of trips cancelled from airport out of total trips cancelled during Morning rush
per_trip_cancelled_airport <- (airport_trips_cancelled/total_trips_cancelled * 100)

# Number of trips requested from city to airport during morning rush
demand_request_city <- length(which(Problem1$Pickup_point == "City"))

# Number of trips completed from city to airport during morning rush
demand_city_trip_completed <- length(which((Problem1$Pickup_point == "City")& (Problem1$Status == "Trip Completed")))

#--------------------------------#
#PROBLEM 2 - Subset the data for Evenings from dataframe for analysis

Problem2 <- subset(subset(Uber_data, Uber_data$Time_Slot == "Evening"))

# Plotting the bar graph with status of requests on x-axis and count in y-axis for evening time slot
# To show the request from different pickup points in different colors
Problem2_count <- ggplot(Problem2, aes(x = factor(Status), fill = factor(Pickup_point)))

Problem2_Plot <- Problem2_count + geom_bar(stat = "count", position = "stack") + ggtitle("Evening Rush Cabs Status") + labs(x="Status", y="Total count") + labs(fill="Pickup Point") + scale_x_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))

#View Plot5
Problem2_Plot


# No of service requests with no cars available for evening time slot
total_nocars <- length(which(Problem2$Status == "No Cars Available"))

# No of  service requests with no cars available from airport during evening rush
airport_nocars <- length(which((Problem2$Pickup_point == "Airport") & (Problem2$Status == "No Cars Available")))

# No of service requests with no cars availablefrom city during evening
city_nocars <- length(which((Problem2$Pickup_point == "City") & (Problem2$Status == "No Cars Available")))

# Percentage of no cars available status from city out of total no cars available during evening
percent_city_nocars <- (city_nocars/total_nocars * 100)

# Percentage of no cars available status from airport out of total no cars available during evening
percent_airport_nocars <- (airport_nocars/total_nocars * 100)

#No of service requests from airport to city during the evening
demand_nocars_request_airport <- length(which(Problem2$Pickup_point == "Airport"))

#No of trips completed from airport to city during the evening
demand_nocars_request_airport_completed <- length(which((Problem2$Pickup_point == "Airport") & (Problem2$Status == "Trip Completed")))


#------------------------------------------------#
#------------------------------------------------#

#PRACTISE R QUERIES FOR ANALYSIS
#verifying the request hour data
table(Uber_data$Request_Hour)
