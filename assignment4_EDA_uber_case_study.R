#####################################
# UBER CASE STUDY                   #
# SUBMITTED BY : RUPA KADAMBI       #
# APPLICATION ID : APFE18801124     #
#####################################

###########################################################################################################################
#                                             PRELIMINARY STEPS - DATA IMPORTS                                            #
###########################################################################################################################

# SET LIBRARY FOR PROJECT #

setwd("C:/Users/rupak/Documents/Training/EDA/uber_case_study")

library(lubridate)
library(ggplot2)
library(dplyr)

#IMPORT DATA INTO R #

uber_data <-  read.csv(file="Uber Request Data.csv", head = TRUE, sep = ",")
View(uber_data)
str(uber_data)

###########################################################################################################################
#                                             PRELIMINARY STEPS - DATA CLEANING                                           #
###########################################################################################################################

class(uber_data$Request.timestamp)

#Firstly make the date separator consistent to be converted

uber_data$Request.timestamp <- gsub("/", "-", uber_data$Request.timestamp)
uber_data$Drop.timestamp <- gsub("/", "-", uber_data$Drop.timestamp)

#Convert character dates to datetime variables

uber_data$requested_at <- as.POSIXct(uber_data$Request.timestamp, format = "%d-%m-%Y %H:%M")
uber_data$dropped_at <- as.POSIXct(uber_data$Drop.timestamp, format = "%d-%m-%Y %H:%M")

#Create a new variable to indicate time of day

attach(uber_data)

uber_data$time_slot[hour(requested_at)>= 0 & hour(requested_at)  < 4 ]  <- "LATE NIGHT"
uber_data$time_slot[hour(requested_at)>= 4 & hour(requested_at)  < 9 ]  <- "EARLY MORNING"
uber_data$time_slot[hour(requested_at)>= 9 & hour(requested_at)  < 12]  <- "LATE MORNING"
uber_data$time_slot[hour(requested_at)>= 12 & hour(requested_at) < 16]  <- "AFTERNOON"
uber_data$time_slot[hour(requested_at)>= 16 & hour(requested_at) < 20]  <- "EVENING"
uber_data$time_slot[hour(requested_at)>= 20 & hour(requested_at) <= 23]  <- "NIGHT"

detach(uber_data)

#Identify weekday and hour

uber_data$weekday <- strftime(uber_data$requested_at, '%A') 

uber_data$request_hr <- hour(uber_data$requested_at)

uber_data$request_day <- as.factor(date(uber_data$requested_at))

#Determine demand and supply for further analysis

uber_data$demand <- 1
uber_data$supply[uber_data$Status == "Trip Completed"] <- 1
uber_data$supply[uber_data$Status != "Trip Completed"] <-0

 

###########################################################################################################################
#                                         DATA ANALYSIS - UNIVARIATE ANALYSIS &                                           #
###########################################################################################################################

#DAY ANALSIS OF DEMAND. USE BAR GRAPH TO SEE TREND IN DEMAND

ggplot(uber_data,aes(x= weekday, y = demand)) + geom_bar(stat = "identity") +  labs(title = "Frequency by day")

ggplot(uber_data,aes(x= weekday, y = demand, fill = Pickup.point)) + geom_bar(stat = "identity") +
labs(title = "Demand by pick up point each day" )

#OBSERVATION : 
#------------------------------------------------------------------------------------------------------------------------
#There is no significant variation in demand over the days, the demand is essentially the same for both pick up points
#------------------------------------------------------------------------------------------------------------------------

#TIME SLOT ANALYSIS - PLOT BAR TO DISPLAY THE DIFFERENCE IN VALUES OVER TIME SLOTS

ggplot(uber_data,aes(x= time_slot, y = demand, fill = Pickup.point)) + geom_bar(stat="identity") +
  labs(x = "TIME SLOT", y = "NBR OF BOOKINGS", title = "DEMAND BASED ON TIME SLOT" ) +
  theme_update(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_brewer(palette = "Set1")

#OBSERVATION : 
#------------------------------------------------------------------------------------------------------------------------
#The demand based on time slot indicates that the demand at the Airport is significantly higher during Evening and Night
#hours. Similarly, for bookings from the City, the demand is high during early morning and late morning hours
#------------------------------------------------------------------------------------------------------------------------

#HOURLY ANALYSIS OF DEMAND BY PICK-UP POINT - HISTOGRAM USED TO VISUALISE CONTINUOUS VARIABLE

airport_data <- subset(uber_data, Pickup.point == "Airport")
city_data <- subset(uber_data, Pickup.point == "City")

theme_update(axis.text.x = element_text(angle = 0, hjust = 1))

qplot(airport_data$request_hr, geom = "histogram", binwidth = 0.5, main = "DEMAND BY HOUR AT AIRPORT", xlab = "HOUR OF DAY",
      ylab = "NBR OF REQUESTS", fill = I("pink"), col = I("red"), xlim = c(0,24))

qplot(city_data$request_hr, geom = "histogram", binwidth = 0.5, main = "DEMAND BY HOUR IN THE CITY", xlab = "HOUR OF DAY",
      ylab = "NBR OF REQUESTS", fill = I("green"), col = I("blue"), xlim = c(0,24))

#OBSERVATION : 
#------------------------------------------------------------------------------------------------------------------------
#Further review of the demand by hour indicates that the demand for rides is highest between 5AM to 9AM in the city and 
#between 5 PM to 10 PM at the Airport
#------------------------------------------------------------------------------------------------------------------------

# ANALYSIS OF STATUS BY PICK UP LOCATION AND TIME SLOT - COMPARE ALL THE STATUS USING BAR AND FACET WRAP

stat_plot <- ggplot(uber_data, aes(x=Status, fill = factor(time_slot), group = factor(time_slot))) +
geom_bar(position = position_dodge())

stat_plot + facet_wrap(time_slot~Pickup.point) + 
labs(x = "BOOKING STATUS", y = "NBR OF BOOKINGS", title = "STATUS BASED ON TIME SLOT & PICK UP POINT" ) +
geom_text(stat="count", aes(label = ..count..), position=position_dodge(width = 0.75), size=3, vjust = 0) +
theme_update(axis.text.x = element_text(angle = 45, hjust = 1))  + scale_fill_brewer(palette = "Set1") 


#OBSERVATION : 
#------------------------------------------------------------------------------------------------------------------------
#It can be observed that the no. of cancelled and No cars available status, i.e unfulfilled bookings occurs
#mostly during the peak demand time slots 
#------------------------------------------------------------------------------------------------------------------------



#write.csv(uber_data,"uber_data_analysis.csv", row.names = FALSE)

