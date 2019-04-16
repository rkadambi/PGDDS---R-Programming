#####################################
# LINEAR REGRESSION                 #
# SUBMITTED BY : RUPA KADAMBI       #
# ROLL NO : DDA1810283              #
#####################################

##############################################################################################################
#                                             PRELIMINARY STEPS - DATA IMPORTS                               #
##############################################################################################################

# SET LIBRARY FOR PROJECT #

library(lubridate)
library(ggplot2)
library(dplyr)
library(MASS)
library(car)
library(stringr)
library(DataExplorer)
library(purrr)
library(tidyr)
library(reshape2)

setwd("C:/Users/rupak/Documents/Training/Course3/assignment")

#IMPORT DATA INTO R #

carprice_data <-  read.csv(file="CarPrice_Assignment.csv", head = TRUE, sep = ",")


##############################################################################################################
#                                             PRELIMINARY STEPS - DATA CLEANING                              #             #
##############################################################################################################

View(carprice_data)
str(carprice_data)
summary(carprice_data)

#CHECK FOR MISSING AND DUPLICATE VALUES

missing_data <- plot_missing(carprice_data)

duplicates <- sum(duplicated(carprice_data))

#FIX DUPLICATE OR MISSPELT CAR NAMES

carprice_data$CarName <-  gsub("toyouta", "toyota", carprice_data$CarName)
carprice_data$CarName <-  gsub("vw", "volkswagen", carprice_data$CarName)
carprice_data$CarName <-  gsub("vokswagen", "volkswagen", carprice_data$CarName)
carprice_data$CarName <-  gsub("maxda", "mazda", carprice_data$CarName)
carprice_data$CarName <-  gsub("porcshce", "porsche", carprice_data$CarName)


###############################################################################################################
#                                        EXPLORATORY DATA ANALYSIS                                            #
###############################################################################################################

#SPLIT THE CAR BRAND NAME FROM THE COMBINED COLUMN

carprice_data$car_brand <- toupper(word(carprice_data$CarName,1))
table(carprice_data$car_brand)

##DUMMY VARIABLE CREATION##

#CONVERT 2 LEVEL FACTOR VARIABLES INTO NUMERIC VARIABLES WITH BINARY VALUES#

levels(carprice_data$fueltype)<-c(1,0)
carprice_data$fueltype <- as.numeric(levels(carprice_data$fueltype))[carprice_data$fueltype]

levels(carprice_data$aspiration)<-c(1,0)
carprice_data$aspiration <- as.numeric(levels(carprice_data$aspiration))[carprice_data$aspiration]

levels(carprice_data$doornumber)<-c(1,0)
carprice_data$doornumber <- as.numeric(levels(carprice_data$doornumber))[carprice_data$doornumber]

levels(carprice_data$enginelocation)<-c(1,0)
carprice_data$enginelocation <- as.numeric(levels(carprice_data$enginelocation))[carprice_data$enginelocation]

str(carprice_data)

#CONVERT 2+ LEVEL FACTOR VARIABLES INTO NUMERIC VARIABLES WITH BINARY VALUES

carbody_1 <- data.frame(model.matrix( ~carbody, data = carprice_data))
carbody_1 <- carbody_1[,-1]

drivewheel_1 <- data.frame(model.matrix( ~drivewheel, data = carprice_data))
drivewheel_1 <- drivewheel_1[,-1]

enginetype_1 <- data.frame(model.matrix( ~enginetype, data = carprice_data))
enginetype_1 <- enginetype_1[,-1]

cyl_num_1 <- data.frame(model.matrix( ~cylindernumber, data = carprice_data))
cyl_num_1 <- cyl_num_1[,-1]

fuelsystem_1 <- data.frame(model.matrix( ~fuelsystem, data = carprice_data))
fuelsystem_1 <- fuelsystem_1[,-1]

carbrand_1 <- data.frame(model.matrix( ~car_brand, data = carprice_data))
carbrand_1 <- carbrand_1[,-1]


#COMBINE ALL DUMMY VARIABLES WITH THE ORIGINAL DATAFRAME

subset_carprice <- subset(carprice_data, 
                   select = -c(carbody, drivewheel, enginetype, cylindernumber, fuelsystem, car_brand))

carprice_combined <- cbind(subset_carprice, carbody_1, drivewheel_1, enginetype_1, cyl_num_1, fuelsystem_1, carbrand_1)



#CONDUCT OUTLIER ANALYSIS ON NUMERIC VARIABLES

boxplot(carprice_combined$wheelbase)
boxplot(carprice_combined$carlength)
boxplot(carprice_combined$carwidth)
boxplot(carprice_combined$carheight)
boxplot(carprice_combined$curbweight)
boxplot(carprice_combined$enginesize)
boxplot(carprice_combined$boreratio)
boxplot(carprice_combined$stroke)
boxplot(carprice_combined$compressionratio)
boxplot(carprice_combined$horsepower)
boxplot(carprice_combined$peakrpm)
boxplot(carprice_combined$citympg)
boxplot(carprice_combined$highwaympg)
boxplot(carprice_combined$price)

###############################################################################################################
#                                 LINEAR REGRESSION MODEL BUILDING                                            #
###############################################################################################################

# CREATE TRAINING AND TEST DATASETS
 
carprice_model <- subset(carprice_combined, select = -c(CarName, car_ID))

set.seed(100)

# GENERATE ROW INDICES FOR 70% OF RECORDS
trainindices= sample(1:nrow(carprice_model), 0.7*nrow(carprice_model))

#GENERATE TRAINING DATA
traincar = carprice_model[trainindices,]

testcar = carprice_model[-trainindices,]

#BUILD FIRST MODEL WITH ALL VARIABLES 

model_1 <-lm(price~.,data=traincar)
summary(model_1)

#USE STEPAIC TO CONDUCT MULTIPLE ITERATIONS OF FORWARD AND BACKWARD SELECTION AND OBTAIN THE FORMULA FOR 
#CONSECUTIVE MODELS

step <- stepAIC(model_1, direction="both")
step

model_2 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + boreratio + stroke + horsepower + 
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + fuelsystemmpfi + car_brandBMW + 
                car_brandBUICK + car_brandDODGE + car_brandHONDA + car_brandJAGUAR + 
                car_brandMAZDA + car_brandMITSUBISHI + car_brandNISSAN + 
                car_brandPLYMOUTH + car_brandRENAULT + car_brandSAAB + car_brandTOYOTA + 
                car_brandVOLKSWAGEN, data = traincar)
summary(model_2)

#DETERMINE VIF AND OBSERVE CORRELATION VALUES

vif(model_2)

#OBSERVER HIGH VIF VALUE VARIABLES (>5) AND NOTE THE CORRESPONDING P VALUES FOR THE VARIABLES. DROP
#THE ONES THAT ARE NOT SIGNIFICANT( p> 0.05) AND BUILD THE NEXT MODEL

model_3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + boreratio + stroke  + 
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + car_brandBMW + 
                car_brandBUICK + car_brandDODGE + car_brandHONDA + car_brandJAGUAR + 
                car_brandMAZDA + car_brandMITSUBISHI + car_brandNISSAN + 
                car_brandPLYMOUTH + car_brandRENAULT + car_brandSAAB + car_brandTOYOTA + 
                car_brandVOLKSWAGEN, data = traincar)
summary(model_3)

#DETERMINE VIF AND OBSERVE CORRELATION VALUES

vif(model_3)

#REPEAT ITERATIONS OF ABOVE VARIABLE EXCLUSION AND MODEL BUILDING

model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + boreratio + stroke  + 
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + car_brandBMW + 
                car_brandBUICK + car_brandDODGE + car_brandHONDA + car_brandJAGUAR + 
                car_brandMAZDA + car_brandMITSUBISHI + car_brandNISSAN + 
                car_brandPLYMOUTH + car_brandRENAULT + car_brandSAAB + car_brandTOYOTA + 
                car_brandVOLKSWAGEN, data = traincar)
summary(model_4)
vif(model_4)

#THE REMAINING VARIABLES WITH HIGH VIF VALUES HAVE LOW P VALUES. HOWEVER, THE VALUES OF CARBODY TYPE IS LESS
#SIGNIFANT THAN THE OTHER VARIABLES. SO DROP THESE AND CONTINUE

model_5 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + boreratio + stroke  + 
                peakrpm + carbodyhardtop + carbodyhatchback + drivewheelrwd + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + fuelsystem2bbl + car_brandBMW + 
                car_brandBUICK + car_brandDODGE + car_brandHONDA + car_brandJAGUAR + 
                car_brandMAZDA + car_brandMITSUBISHI + car_brandNISSAN + 
                car_brandPLYMOUTH + car_brandRENAULT + car_brandSAAB + car_brandTOYOTA + 
                car_brandVOLKSWAGEN, data = traincar)
summary(model_5)
vif(model_5)

# REMOVE VARIABLES WITH HIGH P VALUES THAT ARE NOT SIGNIFICANT

model_6 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + 
                peakrpm  + 
                enginetypeohcf + enginetyperotor + car_brandBMW + 
                car_brandBUICK + car_brandDODGE + car_brandHONDA + car_brandJAGUAR + 
                car_brandMAZDA + car_brandMITSUBISHI + car_brandNISSAN + 
                car_brandPLYMOUTH + car_brandRENAULT  + car_brandTOYOTA + 
                car_brandVOLKSWAGEN, data = traincar)
summary(model_6)
vif(model_6)

model_7 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + 
                peakrpm  + 
                enginetypeohcf + enginetyperotor + car_brandBMW + 
                car_brandBUICK + car_brandDODGE + car_brandHONDA + 
                car_brandMAZDA + car_brandMITSUBISHI + car_brandNISSAN + 
                car_brandPLYMOUTH + car_brandRENAULT  + car_brandTOYOTA ,data = traincar)
summary(model_7)
vif(model_7)

model_8 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + 
                peakrpm  + enginetyperotor + car_brandBMW + 
                car_brandBUICK + car_brandDODGE + car_brandHONDA + 
                car_brandMAZDA + car_brandMITSUBISHI + car_brandNISSAN + 
                car_brandPLYMOUTH + car_brandRENAULT  + car_brandTOYOTA ,data = traincar)
summary(model_8)
vif(model_8)

model_8 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + 
                peakrpm  + enginetyperotor + car_brandBMW + 
                car_brandBUICK + car_brandDODGE + car_brandHONDA + 
                car_brandMAZDA + car_brandMITSUBISHI + car_brandNISSAN + 
                car_brandPLYMOUTH + car_brandRENAULT  + car_brandTOYOTA ,data = traincar)
summary(model_8)
vif(model_8)

model_9 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + 
                peakrpm  + enginetyperotor + car_brandBMW + 
                car_brandBUICK + car_brandDODGE + car_brandMITSUBISHI + car_brandNISSAN + 
                car_brandPLYMOUTH + car_brandRENAULT  + car_brandTOYOTA ,data = traincar)
summary(model_9)
vif(model_9)

model_10 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + 
                peakrpm  + enginetyperotor + car_brandBMW + 
                car_brandBUICK + car_brandMITSUBISHI ,data = traincar)
summary(model_10)
vif(model_10)

model_11 <- lm(formula = price ~ aspiration + enginelocation + carwidth + enginesize + 
                 peakrpm  + enginetyperotor + car_brandBMW + 
                 car_brandBUICK ,data = traincar)
summary(model_11)
vif(model_11)

model_12 <- lm(formula = price ~ enginelocation + carwidth + enginesize + 
                 peakrpm  + enginetyperotor + car_brandBMW + 
                 car_brandBUICK ,data = traincar)
summary(model_12)
vif(model_12)

model_13 <- lm(formula = price ~ enginelocation + carwidth + enginesize + 
                 peakrpm + car_brandBMW + 
                 car_brandBUICK ,data = traincar)
summary(model_13)
vif(model_13)

#FINALLY ACCEPTED THE MODEL ABOVE WITH LOW VIF VALUES AND LOW P VALUES, AFTER MULTIPLE ITERATIONS ABOVE

#PREDICT THE RESULTS IN TEST

predict_price <- predict(model_13, testcar[,-1])
testcar$est_price <- predict_price

#CHECK CORRELATION IN TEST DATASET

r<- cor(testcar$price, testcar$est_price)
rsquared <- cor(testcar$price, testcar$est_price)^2
rsquared

#THE RSQUARE VALUES ARE HIGH IN BOTH TRAINING AND TEST DATA. PLOT THE TEST AND ACTUAL PRICE

testplot <- testcar

testplot$id_no <-rownames(testplot)

testplot %>%
  gather(key, value, price, est_price) %>%
  ggplot(aes(x=id_no, y = value, group = 1, colour = key)) + geom_line()


############################################################################################################
#                                             MODELLING SUMMARY                                            #
############################################################################################################

#The model built above have low VIF values (less than 5) and high statistic significance (p value < 0.05)
#The RSQUARE AND ADJUSTED RSQUARE values are very close, so there is no presence of redundant variables
#The RSQUARE values in the test data is also high and plotting the test vs actual price of the cars is close
#in most cases. Hence the model seems to be a reliable one that reflects the correlation between variables. 
#The independent variables that affect the price of the cars can be seen in the final model and the 
#same can be communicated to the marketing and Business team to take appropriate decision.