#########################################
# TIME SERIES - CASE STUDY              #
# SUBMITTED BY : RUPA KADAMBI           #
#                SAURAV DAS             #
#                SOURABH DAS            #
#########################################

##############################################################################################################
#                                             PRELIMINARY STEPS - DATA IMPORTS                               #
##############################################################################################################

# SET LIBRARY FOR PROJECT #

lapply(.packages(all.available = TRUE), function(x)library(x,character.only = TRUE))
setwd("C:/Users/rupak/Documents/Training/time_series/case_study")

#IMPORT DATA INTO R #

global_superstore <-  read.csv(file="Global Superstore.csv", head = TRUE, sep = ",")


##############################################################################################################
#                                    PRELIMINARY STEPS - DATA PREPARATION & EDA                              #             
##############################################################################################################

#Verify Record count

nrow(data.table::fread("Global Superstore.csv"))
nrow(global_superstore)

#View file and review structure and data attributes

View(global_superstore)
str(global_superstore)

#DataExplorer::create_report(global_superstore)

#--------------------------------CHECK FOR MISSING AND DUPLICATE VALUES---------------------------------------#


missing_vars <- plot_missing(global_superstore)

#Only missing values are in postal_code field which is not important for this case study

dupes <- sum(duplicated(global_superstore))


#CREATE A SUBSET OF THE MASTER DATA, LIMITING IT TO THE REQUIRED VARIABLES AND ALSO CREATING A NEW ONE
#FOR SEGMENT

global_superstore$customer_segment <- as.factor(toupper(paste(global_superstore$Market, 
                                                  global_superstore$Segment, sep = " ")))
                                              
keepvars <- c("Order.Date", "customer_segment", "Sales", "Quantity", "Profit")

filtered_data <- global_superstore[keepvars]

#Convert order date to date format

filtered_data$order_month <- as.factor(as.yearmon(as.Date(filtered_data$Order.Date, format = "%d-%m-%Y")))

#---------------------------------------DATA VISUALIZATION-----------------------------------------------------#


summary(filtered_data)
table(filtered_data$customer_segment)

#AGGREGATE BY SALES, QUANTITY & PROFIT BY SEGMENT

detach(package:plyr)
aggregate_data <- filtered_data %>%
                  select(order_month,customer_segment,Sales, Quantity, Profit) %>%
                  group_by(customer_segment, order_month) %>%
                  summarise(tot_sales = sum(Sales), tot_qty = sum(Quantity), tot_profit = sum(Profit)) %>%
                  arrange(customer_segment, order_month)

profit_summary <- aggregate_data %>% 
                  group_by(customer_segment) %>%
                  summarise(cov_profit = sd(tot_profit)/mean(tot_profit), segment_profit = sum(tot_profit))
      

ggplot(profit_summary, aes(customer_segment, y = segment_profit, fill = customer_segment)) +
  geom_bar(stat = "sum") +
  labs(x = "CUSTOMER SEGMENT/MARKET", y = "TOTAL PROFIT", title = "TOTAL PROFIT BY CONSUMER SEGMENT & MARKET" ) 
  theme_update(axis.text.x = element_text(angle = 90, hjust = 0)) 

ggplot(profit_summary, aes(customer_segment, y = cov_profit)) +
    geom_point() +
    labs(x = "CUSTOMER SEGMENT/MARKET", y = "CO-EFFICIENT OF VARIATION", 
         title = "PROFIT COV BY CONSUMER SEGMENT & MARKET" ) 
  theme_update(axis.text.x = element_text(angle = 90, hjust = 0)) 


theme_update(axis.text.x = element_text(angle = 0, hjust = 0))  
                   
#IT CAN BE OBSERVED THAT THE HIGHEST PROFIT AND THE LOWEST COV VALUES ARE SEEN FOR THE APAC CONSUMER AND 
#EU CONSUMER. THESE TWO SEGMENTS WILL BE USED DOWNSTREAM FOR MODELLING AND SALES PREDICTION

library(dplyr)

#SUBSET THE DATA FOR THE TOP TWO CONSUMER SEGMENT-MARKET COMBINATIONS

apac_consumer_data <-  aggregate_data %>%
                      filter(customer_segment %in% c("APAC CONSUMER")) %>% 
                      arrange(order_month, tot_sales)

eu_consumer_data <-  aggregate_data %>%
                    filter(customer_segment %in% c("EU CONSUMER")) %>% 
                    arrange(order_month, tot_sales)

#RETAIN THE RELEVANT VARIABLES AND EXCLUDE LAST 6 MONTHS' DATA

apac_model_for_sales <- subset(apac_consumer_data[,c(2:3)])
apac_model_for_demand <- subset(apac_consumer_data[,c(2,4)])

eu_model_for_sales <- subset(eu_consumer_data[,c(2:3)])
eu_model_for_demand <- subset(eu_consumer_data[,c(2,4)])

apac_sales_tot_timeser <- ts(apac_consumer_data$tot_sales)
apac_sales_timeser <- ts(apac_model_for_sales$tot_sales)
apac_sales_outdata <- apac_consumer_data$tot_sales[43:48]

apac_demand_tot_timeser <- ts(apac_consumer_data$tot_qty)
apac_demand_timeser <- ts(apac_model_for_demand$tot_qty)
apac_demand_outdata <- apac_consumer_data$tot_qty[43:48]

eu_sales_tot_timeser <- ts(eu_consumer_data$tot_sales)
eu_sales_timeser <- ts(eu_model_for_sales$tot_sales)
eu_sales_outdata <- eu_consumer_data$tot_sales[43:48]

eu_demand_tot_timeser <- ts(eu_consumer_data$tot_qty)
eu_demand_timeser <- ts(eu_model_for_demand$tot_qty)
eu_demand_outdata <- eu_consumer_data$tot_qty[43:48]
  
  
##############################################################################################################
#                                    DATA MODELLING - CLASSIC DECOMPOSITION                                  #             
##############################################################################################################

#CREATE TIME SERIES OBJECTS FOR THE 2 MARKET SEGMENTS FOR SALES AND QUANTITY

apac_sales_total_ts <- ts(apac_model_for_sales$tot_sales)
in_apac_sales <- apac_model_for_sales[1:42,]
apac_sales_ts_trn <- ts(in_apac_sales$tot_sales)
plot(apac_sales_ts_trn)

apac_qty_total_ts <- ts(apac_model_for_demand$tot_qty)
in_apac_qty <- apac_model_for_demand[1:42,]
apac_qty_ts_trn <- ts(in_apac_qty$tot_qty)
plot(apac_qty_ts_trn)

eu_sales_total_ts <- ts(eu_model_for_sales$tot_sales)
in_eu_sales <- eu_model_for_sales[1:42,]
eu_sales_ts_trn <- ts(in_eu_sales$tot_sales)
plot(eu_sales_ts_trn)

eu_qty_total_ts <- ts(eu_model_for_demand$tot_qty)
in_eu_qty <- eu_model_for_demand[1:42,]
eu_qty_ts_trn <- ts(in_eu_qty$tot_qty)
plot(eu_qty_ts_trn)

#SMOOTHEN THE SERIES USING MOVING AVERAGE SMOOTHING, CHOOSING WIDTH OF 1

width <- 1

smooth_apac_sales_ts <- stats::filter(apac_sales_ts_trn, filter=rep(1/(2*width+1),(2*width+1)), method='convolution', sides = 2)
smooth_apac_qty_ts <- stats::filter(apac_qty_ts_trn, filter=rep(1/(2*width+1),(2*width+1)), method='convolution', sides = 2)
smooth_eu_sales_ts <- stats::filter(eu_sales_ts_trn, filter=rep(1/(2*width+1),(2*width+1)), method='convolution', sides = 2)
smooth_eu_qty_ts <- stats::filter(eu_qty_ts_trn, filter=rep(1/(2*width+1),(2*width+1)), method='convolution', sides = 2)

#CHECK EFFECT OF SMOOTHENING ON ORIGINAL TIME SERIES PLOT

plot(apac_sales_ts_trn)
lines(smooth_apac_sales_ts, col = "blue", lwd = 2)
title ("SALES OVER TIME FOR APAC CONSUMER")

plot(apac_qty_ts_trn)
lines(smooth_apac_qty_ts, col = "blue", lwd = 2)
title ("DEMAND OVER TIME FOR APAC CONSUMER")

plot(eu_sales_ts_trn)
lines(smooth_eu_sales_ts, col = "blue", lwd = 2)
title ("SALES OVER TIME FOR EU CONSUMER")

plot(eu_sales_ts_trn)
lines(smooth_eu_sales_ts, col = "blue", lwd = 2)
title ("DEMAND OVER TIME FOR EU CONSUMER")

#Smoothing right end of the time series

n1 <- length(apac_sales_ts_trn)
diff <- smooth_apac_sales_ts[n1-width] - smooth_apac_sales_ts[n1-width-1]
for (i in seq(n1-width+1, n1)) {
  smooth_apac_sales_ts[i] <- smooth_apac_sales_ts[i-1] + diff
}

n2 <- length(apac_qty_ts_trn)
diff <- smooth_apac_qty_ts[n2-width] - smooth_apac_qty_ts[n2-width-1]
for (i in seq(n2-width+1, n2)) {
  smooth_apac_qty_ts[i] <- smooth_apac_qty_ts[i-1] + diff
}

n3 <- length(eu_sales_ts_trn)
diff <- smooth_eu_sales_ts[n3-width] - smooth_eu_sales_ts[n3-width-1]
for (i in seq(n3-width+1, n3)) {
  smooth_eu_sales_ts[i] <- smooth_eu_sales_ts[i-1] + diff
}

n4 <- length(eu_qty_ts_trn)
diff <- smooth_eu_qty_ts[n4-width] - smooth_eu_qty_ts[n4-width-1]
for (i in seq(n4-width+1, n4)) {
  smooth_eu_qty_ts[i] <- smooth_eu_qty_ts[i-1] + diff
}

#-----------------------------------PLOT OBSERVATIONS AND MODEL SELECTION------------------------------------#

#SMOOTHENING WITH A WIDTH OF 1 IS EFFECTIVE IS EVENING OUT SOME OF THE IRREGULARITIES. IT CAN NOW BE OBSERVED
#THAT THERE IS AN UPWARD TREND IN DATA, ALONG WITH SEASONALITY, WITH ALMOST EQUAL AMPLITUDE.
#A SINUSOILDAL FIL WITH ADDITIVE POLYNOMIAL MAY WORK IN THIS CASE.

#CREATE DATA FRAMES FROM TIME SERIES FOR MODELLING. CREATE MONTH VARIABLE TO INDICATE THE DATA MONTH
#RENAME VARIABLES FOR EASE OF USE DOWNSTREAM

sm_apac_sales <- seq(1:length(apac_sales_ts_trn))
sm_apac_qty <- seq(1:length(apac_qty_ts_trn))
sm_eu_sales <- seq(1:length(eu_sales_ts_trn))
sm_eu_qty <- seq(1:length(eu_qty_ts_trn))


smoothed_apac_sales <- as.data.frame(cbind(apac_sales_ts_trn, as.vector(sm_apac_sales)))
colnames(smoothed_apac_sales) <- c('Sales', 'Month')
str(smoothed_apac_sales)
smoothed_apac_qty <- as.data.frame(cbind(apac_qty_ts_trn, as.vector(sm_apac_qty)))
colnames(smoothed_apac_qty) <- c('Sales', 'Month')
smoothed_eu_sales <- as.data.frame(cbind(eu_sales_ts_trn, as.vector(sm_eu_sales)))
colnames(smoothed_eu_sales) <- c('Sales', 'Month')
smoothed_eu_qty <- as.data.frame(cbind(eu_qty_ts_trn, as.vector(sm_eu_qty)))
colnames(smoothed_eu_qty) <- c('Sales', 'Month')

#-------------------------------------------MODEL BUILDING-----------------------------------------------------#

#BUILD REGRESSION MODELS FOR EACH OF THE MARKET-SEGMENTS

create_model_df <- function(seg,width, cnst)
{
  lm(seg$Sales ~ (abs(sin(cnst*Month) * poly(Month, width))) + 
       (abs(cos(cnst*Month) * poly(Month, width))) +
       (Month), data = seg)
}

#--------------------------------------APAC CONSUMER SALES MODEL ---------------------------------------------#

apac_sales_tot_timeser <- ts(apac_consumer_data$tot_sales)
apac_sales_timeser <- ts(apac_model_for_sales$tot_sales)
apac_sales_outdata <- apac_consumer_data$tot_sales[43:48]


apac_sales_lm <- create_model_df(smoothed_apac_sales, 1, 0.5)
global_pred_apac_sales1 <- predict(apac_sales_lm, Month = sm_apac_sales )
apac_sales_lm <- create_model_df(smoothed_apac_sales, 3, 0.5)
global_pred_apac_sales3 <- predict(apac_sales_lm, Month = sm_apac_sales )
apac_sales_lm <- create_model_df(smoothed_apac_sales, 5, 0.5)
global_pred_apac_sales5 <- predict(apac_sales_lm, Month = sm_apac_sales )


plot(apac_sales_ts_trn, type = "l")
title ("Model fitting for APAC Consumer segment")
lines(smooth_apac_sales_ts, col = "blue", lwd = 2)
lines(global_pred_apac_sales1, col='red', lwd=2)
lines(global_pred_apac_sales3, col='orange', lwd=2)
lines(global_pred_apac_sales5, col='purple', lwd=2)

#FROM THE PLOT ABOVE, IT APPEARS THAT A DEGREE OF 5 POLYNOMIAL WORKS BEST. USE THIS VALUE TO MODEL FURTHER

local_pred_apac_sales <- ts(in_apac_sales[,2])- global_pred_apac_sales5
plot(local_pred_apac_sales, col = "red", type = "l")

#MODEL AS ARMA SERIES

acf(local_pred_apac_sales)
acf(local_pred_apac_sales, type = "partial")
armafit_apac_sales <- auto.arima(local_pred_apac_sales)
tsdiag(armafit_apac_sales)

#--------------------------------------------MODEL EVALUATION & TESTING---------------------------------------------------#


#CHECK RESIDUAL AND CONFIRM IT IS WHITE NOISE

residual_apac_sales <- local_pred_apac_sales - fitted(armafit_apac_sales)
adf.test(residual_apac_sales, alternative = "stationary")
kpss.test(residual_apac_sales)

#ADF INDICATES A P VALUE OR 0.01, WHICH WOULD REJECT THE NULL HYPOTHESIS. KPSS TEST ALSO INDICATES A P VALUE 0.01
#WHICH IS LESS THAN 0.05, SUPPORTING THE NULL HYPOTHESIS THAT THE SERIES IS STATIONARY

#PREDICT THE VALUE OF SALES FOR THE NEXT 6 MONTHS AND USE MAPE EVALUATION

out_apac_sales <- apac_model_for_sales[43:48,]
colnames(out_apac_sales) <- c('Month', 'Sales')
timevals_out <- as.data.frame(seq(43:48))
colnames(timevals_out) <- c('Month')
global_pred_apac_sales <- predict(apac_sales_lm, timevals_out)


#COMPARE PREDICTED SALES VALUE WITH ACTUAL VALUES USING MAPE
MAPE_apac_sales <- accuracy(global_pred_apac_sales,apac_demand_outdata)[5]
MAPE_apac_sales

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

orig_apac_sales <- apac_model_for_sales
apac_sales_classic <- c(ts(apac_model_for_sales),ts(global_pred_apac_sales))
plot(apac_sales_tot_timeser, col = "black")
lines(apac_sales_classic, col = "black")

#--------------------------------------EU CONSUMER SALES MODEL ---------------------------------------------#

eu_sales_lm <- create_model_df(smoothed_eu_sales, 1, 0.5)
global_pred_eu_sales1 <- predict(eu_sales_lm, Month = sm_eu_sales )
eu_sales_lm <- create_model_df(smoothed_eu_sales, 3, 0.5)
global_pred_eu_sales3 <- predict(eu_sales_lm, Month = sm_eu_sales )
eu_sales_lm <- create_model_df(smoothed_eu_sales, 5, 0.5)
global_pred_eu_sales5 <- predict(eu_sales_lm, Month = sm_eu_sales )


plot(eu_sales_ts_trn, type = "l")
title ("Model fitting for EU Consumer segment")
lines(smooth_eu_sales_ts, col = "blue", lwd = 2)
lines(global_pred_eu_sales1, col='red', lwd=2)
lines(global_pred_eu_sales3, col='orange', lwd=2)
lines(global_pred_eu_sales5, col='purple', lwd=2)

#FROM THE PLOT ABOVE, IT APPEARS THAT A DEGREE OF 5 POLYNOMIAL WORKS BEST. USE THIS VALUE TO MODEL FURTHER

local_pred_eu_sales <- ts(in_eu_sales[,2])- global_pred_eu_sales5
plot(local_pred_eu_sales, col = "red", type = "l")
title("LOCAL SALES TREND FOR EU CONSUMER")

#MODEL AS ARMA SERIES

acf(local_pred_eu_sales)
acf(local_pred_eu_sales, type = "partial")
armafit_eu_sales <- auto.arima(local_pred_eu_sales)
tsdiag(armafit_eu_sales)

#--------------------------------------------MODEL EVALUATION & TESTING---------------------------------------------------#


#CHECK RESIDUAL AND CONFIRM IT IS WHITE NOISE

residual_eu_sales <- local_pred_eu_sales - fitted(armafit_eu_sales)
adf.test(residual_eu_sales, alternative = "stationary")
kpss.test(residual_eu_sales)

#ADF INDICATES A P VALUE OR 0.01, WHICH WOULD REJECT THE NULL HYPOTHESIS. KPSS TEST ALSO INDICATES A P VALUE 0.01
#WHICH IS LESS THAN 0.05, SUPPORTING THE NULL HYPOTHESIS THAT THE SERIES IS STATIONARY

#PREDICT THE VALUE OF SALES FOR THE NEXT 6 MONTHS AND USE MAPE EVALUATION

out_eu_sales <- eu_model_for_sales[43:48,]
colnames(out_eu_sales) <- c('Month', 'Sales')
timevals_out <- as.data.frame(seq(43:48))
colnames(timevals_out) <- c('Month')
global_pred_eu_sales <- predict(eu_sales_lm, timevals_out)


#COMPARE PREDICTED SALES VALUE WITH ACTUAL VALUES USING MAPE
MAPE_eu_sales <- accuracy(global_pred_eu_sales,eu_demand_outdata)[5]
MAPE_eu_sales

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

orig_eu_sales <- eu_model_for_sales
eu_sales_classic <- c(ts(eu_model_for_sales),ts(global_pred_eu_sales))
plot(eu_sales_tot_timeser, col = "black")
lines(eu_sales_classic, col = "black")


#--------------------------------------EU DEMAND MODEL ---------------------------------------------#

eu_qty_lm <- create_model_df(smoothed_eu_qty, 1, 0.5)
global_pred_eu_qty1 <- predict(eu_qty_lm, Month = sm_eu_qty )
eu_qty_lm <- create_model_df(smoothed_eu_qty, 3, 0.5)
global_pred_eu_qty3 <- predict(eu_qty_lm, Month = sm_eu_qty )
eu_qty_lm <- create_model_df(smoothed_eu_qty, 5, 0.5)
global_pred_eu_qty5 <- predict(eu_qty_lm, Month = sm_eu_qty )


plot(eu_qty_ts_trn, type = "l")
title ("Model fitting for EU Consumer segment")
lines(smooth_eu_qty_ts, col = "blue", lwd = 2)
lines(global_pred_eu_qty1, col='red', lwd=2)
lines(global_pred_eu_qty3, col='orange', lwd=2)
lines(global_pred_eu_qty5, col='purple', lwd=2)

#FROM THE PLOT ABOVE, IT APPEARS THAT A DEGREE OF 5 POLYNOMIAL WORKS BEST. USE THIS VALUE TO MODEL FURTHER

local_pred_eu_qty <- ts(in_eu_qty[,2])- global_pred_eu_qty5
plot(local_pred_eu_qty, col = "red", type = "l")
title("LOCAL SALES TREND FOR EU CONSUMER")

#MODEL AS ARMA SERIES

acf(local_pred_eu_qty)
acf(local_pred_eu_qty, type = "partial")
armafit_eu_qty <- auto.arima(local_pred_eu_qty)
tsdiag(armafit_eu_qty)

#--------------------------------------------MODEL EVALUATION & TESTING---------------------------------------------------#


#CHECK RESIDUAL AND CONFIRM IT IS WHITE NOISE

residual_eu_qty <- local_pred_eu_qty - fitted(armafit_eu_qty)
adf.test(residual_eu_qty, alternative = "stationary")
kpss.test(residual_eu_qty)

#ADF INDICATES A P VALUE OR 0.01, WHICH WOULD REJECT THE NULL HYPOTHESIS. KPSS TEST ALSO INDICATES A P VALUE 0.01
#WHICH IS LESS THAN 0.05, SUPPORTING THE NULL HYPOTHESIS THAT THE SERIES IS STATIONARY

#PREDICT THE VALUE OF SALES FOR THE NEXT 6 MONTHS AND USE MAPE EVALUATION

out_eu_qty <- eu_model_for_demand[43:48,]
colnames(out_eu_qty) <- c('Month', 'Sales')
timevals_out <- as.data.frame(seq(43:48))
colnames(timevals_out) <- c('Month')
global_pred_eu_qty <- predict(eu_qty_lm, timevals_out)


#COMPARE PREDICTED SALES VALUE WITH ACTUAL VALUES USING MAPE
MAPE_eu_qty <- accuracy(global_pred_eu_qty,eu_demand_outdata)[5]
MAPE_eu_qty

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

orig_eu_qty <- eu_model_for_demand
eu_qty_classic <- c(ts(eu_model_for_demand),ts(global_pred_eu_qty))
plot(eu_demand_tot_timeser, col = "black")
lines(eu_qty_classic, col = "red")


#--------------------------------------APAC DEMAND MODEL ---------------------------------------------#

apac_qty_lm <- create_model_df(smoothed_apac_qty, 1, 0.5)
global_pred_apac_qty1 <- predict(apac_qty_lm, Month = sm_apac_qty )
apac_qty_lm <- create_model_df(smoothed_apac_qty, 3, 0.5)
global_pred_apac_qty3 <- predict(apac_qty_lm, Month = sm_apac_qty )
apac_qty_lm <- create_model_df(smoothed_apac_qty, 5, 0.5)
global_pred_apac_qty5 <- predict(apac_qty_lm, Month = sm_apac_qty )


plot(apac_qty_ts_trn, type = "l")

lines(smooth_apac_qty_ts, col = "blue", lwd = 2)
lines(global_pred_apac_qty1, col='red', lwd=2)
lines(global_pred_apac_qty3, col='orange', lwd=2)
lines(global_pred_apac_qty5, col='purple', lwd=2)

#FROM THE PLOT ABOVE, IT APPEARS THAT A DEGREE OF 5 POLYNOMIAL WORKS BEST. USE THIS VALUE TO MODEL FURTHER

local_pred_apac_qty <- ts(in_apac_qty[,2])- global_pred_apac_qty5
plot(local_pred_apac_qty, col = "red", type = "l")


#MODEL AS ARMA SERIES

acf(local_pred_apac_qty)
acf(local_pred_apac_qty, type = "partial")
armafit_apac_qty <- auto.arima(local_pred_apac_qty)
tsdiag(armafit_apac_qty)

#--------------------------------------------MODEL EVALUATION & TESTING---------------------------------------------------#


#CHECK RESIDUAL AND CONFIRM IT IS WHITE NOISE

residual_apac_qty <- local_pred_apac_qty - fitted(armafit_apac_qty)
adf.test(residual_apac_qty, alternative = "stationary")
kpss.test(residual_apac_qty)

#ADF INDICATES A P VALUE OR 0.01, WHICH WOULD REJECT THE NULL HYPOTHESIS. KPSS TEST ALSO INDICATES A P VALUE 0.01
#WHICH IS LESS THAN 0.05, SUPPORTING THE NULL HYPOTHESIS THAT THE SERIES IS STATIONARY

#PREDICT THE VALUE OF SALES FOR THE NEXT 6 MONTHS AND USE MAPE EVALUATION

out_apac_qty <- apac_model_for_demand[43:48,]
colnames(out_apac_qty) <- c('Month', 'Sales')
timevals_out <- as.data.frame(seq(43:48))
colnames(timevals_out) <- c('Month')
global_pred_apac_qty <- predict(apac_qty_lm, timevals_out)


#COMPARE PREDICTED SALES VALUE WITH ACTUAL VALUES USING MAPE
MAPE_apac_qty <- accuracy(global_pred_apac_qty,apac_demand_outdata)[5]
MAPE_apac_qty

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

orig_apac_qty <- apac_model_for_demand
apac_qty_classic <- c(ts(apac_model_for_demand),ts(global_pred_apac_qty))
plot(apac_demand_tot_timeser, col = "black")
lines(apac_qty_classic, col = "red")

##############################################################################################################
#                                    DATA MODELLING - AUTO ARIMA                                             #             
##############################################################################################################

# View the filtered 42 months data for the selected segments
View(apac_model_for_sales)
View(apac_model_for_demand)
View(eu_model_for_sales)
View(eu_model_for_demand)

# Create the timeseries data of sales and demand for the selected segments to build the auto ARIMA model

apac_sales_tot_timeser <- ts(apac_consumer_data$tot_sales)
apac_sales_timeser <- ts(apac_model_for_sales$tot_sales)
apac_sales_outdata <- apac_consumer_data$tot_sales[43:48]

apac_demand_tot_timeser <- ts(apac_consumer_data$tot_qty)
apac_demand_timeser <- ts(apac_model_for_demand$tot_qty)
apac_demand_outdata <- apac_consumer_data$tot_qty[43:48]

eu_sales_tot_timeser <- ts(eu_consumer_data$tot_sales)
eu_sales_timeser <- ts(eu_model_for_sales$tot_sales)
eu_sales_outdata <- eu_consumer_data$tot_sales[43:48]

eu_demand_tot_timeser <- ts(eu_consumer_data$tot_qty)
eu_demand_timeser <- ts(eu_model_for_demand$tot_qty)
eu_demand_outdata <- eu_consumer_data$tot_qty[43:48]

# Create the model for APAC Sales using the first 42 months data

autoarima <- auto.arima(apac_sales_timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

# Check if the residual series is white noise
resi_auto_arima <- apac_sales_timeser - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary") # p value of 0.01 indicates stationarity in data
kpss.test(resi_auto_arima) # p value of 0.1 indicates stationarity of data

# Evaluate the model using MAPE after making prediction for the last 6 months
fcast_auto_arima <- predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,apac_sales_outdata)[5]
MAPE_auto_arima # MAPE value of 27.689% for APAC Sales model

# Plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(apac_sales_tot_timeser, col = "black")
lines(auto_arima_pred, col = "red")
title("PREDICTED VS ORIGINAL FOR APAC CONSUMER- SALES")
# Auto ARIMA model for APAC Sales did not give a good fit between the predicted values and total timeseries


# Create the model for APAC Demand using the first 42 months data

autoarima2 <- auto.arima(apac_demand_timeser)
autoarima2
tsdiag(autoarima2)
plot(autoarima2$x, col="black")
lines(fitted(autoarima2), col="red")

# Check if the residual series is white noise
resi_auto_arima2 <- apac_demand_timeser - fitted(autoarima2)
adf.test(resi_auto_arima2,alternative = "stationary") # p value of 0.01 indicates stationarity in data
kpss.test(resi_auto_arima2) # p value of 0.1 indicates stationarity of data

# Evaluate the model using MAPE after making prediction for the last 6 months
fcast_auto_arima2 <- predict(autoarima2, n.ahead = 6)
MAPE_auto_arima2 <- accuracy(fcast_auto_arima2$pred,apac_demand_outdata)[5]
MAPE_auto_arima2 # MAPE value of 26.244% for APAC Demand model

# Plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred2 <- c(fitted(autoarima2),ts(fcast_auto_arima2$pred))
plot(apac_demand_tot_timeser, col = "black")
lines(auto_arima_pred2, col = "red")
title("PREDICTED VS ORIGINAL FOR APAC CONSUMER DEMAND")
# Auto ARIMA model for APAC Demand visually shows a good fit between the predicted values and total timeseries data


# Create the model for EU Sales using the first 42 months data

autoarima3 <- auto.arima(eu_sales_timeser)
autoarima3
tsdiag(autoarima3)
plot(autoarima3$x, col="black")
lines(fitted(autoarima3), col="red")

# Check if the residual series is white noise
resi_auto_arima3 <- eu_sales_timeser - fitted(autoarima3)
adf.test(resi_auto_arima3,alternative = "stationary") # p value of 0.01 indicates stationarity in data
kpss.test(resi_auto_arima3) # p value of 0.1 indicates stationarity of data

# Evaluate the model using MAPE after making prediction for the last 6 months
fcast_auto_arima3 <- predict(autoarima3, n.ahead = 6)
MAPE_auto_arima3 <- accuracy(fcast_auto_arima3$pred,eu_sales_outdata)[5]
MAPE_auto_arima3 # MAPE value of 28.923% for EU Sales model

# Plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred3 <- c(fitted(autoarima3),ts(fcast_auto_arima3$pred))
plot(eu_sales_tot_timeser, col = "black")
lines(auto_arima_pred3, col = "red")
title("PREDICTED VS ORIGINAL SALES FOR EU CONSUMER")
# Auto ARIMA model for EU Sales did not show a good fit between the predicted values and total timeseries data


# Create the model for EU Demand using the first 42 months data

autoarima4 <- auto.arima(eu_demand_timeser)
autoarima4
tsdiag(autoarima4)
plot(autoarima4$x, col="black")
lines(fitted(autoarima4), col="red")

# Check if the residual series is white noise
resi_auto_arima4 <- eu_demand_timeser - fitted(autoarima4)
adf.test(resi_auto_arima4,alternative = "stationary") # p value of 0.045 indicates marginal stationarity in data
kpss.test(resi_auto_arima4) # p value of 0.1 indicates stationarity of data

# Evaluate the model using MAPE after making prediction for the last 6 months
fcast_auto_arima4 <- predict(autoarima4, n.ahead = 6)
MAPE_auto_arima4 <- accuracy(fcast_auto_arima4$pred,eu_demand_outdata)[5]
MAPE_auto_arima4 # MAPE value of 30.133% for EU Demand model

# Plot the predictions along with original values, to get a visual feel of the fit
auto_arima_pred4 <- c(fitted(autoarima4),ts(fcast_auto_arima4$pred))
plot(eu_demand_tot_timeser, col = "black")
lines(auto_arima_pred4, col = "red")
title("PREDICTED VS ACTUAL DEMAND FOR EU CONSUMER")
# Auto ARIMA model for EU Demand did not show a good fit between the predicted values and total timeseries data


##############################################################################################################
#                                         CONCLUSION                                                         #
##############################################################################################################

#Classical models have better fit and forecast than the auto-arima model. Further fine-tuning of the model parameters
#may improve accuracy much more and can be used to predict sales with more reliability
 

