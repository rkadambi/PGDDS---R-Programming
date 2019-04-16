#########################################
# HR ATTRITION ANALYTICS _ CASE STUDY   #
# SUBMITTED BY : RUPA KADAMBI           #
#                SOURABH                #
#                SAURAV DAS             #
#                ABHISHEK KUMAR         #
#########################################

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
library(tidyverse)
library(dummies)
library(caret)
library(ROCR)
library(gains)

setwd("C:/Users/rupak/Documents/Training/Course3/HR_analytics")

#IMPORT DATA INTO R #

emp_survey_data <-  read.csv(file="employee_survey_data.csv", head = TRUE, sep = ",")
general_data <-  read.csv(file="general_data.csv", head = TRUE, sep = ",")
intime_data <-  read.csv(file="in_time.csv", head = TRUE, sep = ",")
mgr_survey_data <-  read.csv(file="manager_survey_data.csv", head = TRUE, sep = ",")
outtime_data <-  read.csv(file="out_time.csv", head = TRUE, sep = ",")


##############################################################################################################
#                                    PRELIMINARY STEPS - DATA PREPARATION & EDA                              #             
##############################################################################################################

# -------------------------CLEAN AND MERGE IN-TIME AND OUT-TIME DATASETS-------------------------------------#

View(intime_data)
str(intime_data)
View(outtime_data)
str(outtime_data)

#THE COLUMN NAME IS MISSING FOR FIRST COLUMN. ASSIGN THE SAME FOR BOTH IN-TIME AND OUT-TIME DATA

colnames(intime_data)[1] <- c("EmployeeID")
colnames(outtime_data)[1] <- c("EmployeeID")

#THE COLUMNS WITH ALL MISSING DATA INDICATES GENERAL HOLIDAYS, CHECK FOR THE SAME AND EXCLUDE FROM DATA

allmisscols_in <- apply(intime_data, 2, function(x)all(is.na(x)))
gen_holidays_in <- c(names(allmisscols_in[allmisscols_in>0]))

allmisscols_out <- apply(outtime_data, 2, function(x)all(is.na(x)))
gen_holidays_out <- names(allmisscols_out[allmisscols_out>0])

identical(gen_holidays_out, gen_holidays_in)

intime_workdays <- intime_data[, -which(names(intime_data) %in% gen_holidays_in)]
outtime_workdays <- outtime_data[, -which(names(outtime_data) %in% gen_holidays_out)]

#CONVERT ALL DATE COLUMNS FOR FURTHER ANALYSIS

intime_dates <- intime_workdays %>% mutate_at(vars(starts_with("X2015")), funs(ymd_hms))
outtime_dates <- outtime_workdays %>% mutate_at(vars(starts_with("X2015")), funs(ymd_hms))


str(intime_dates)
str(outtime_dates)

#MERGE THE IN-TIME AND OUT-TIME DATA AND CALCULATE THE DIFFERENCE IN TIME AFTER CONVERTING TO LONG FORMAT. 

intime_long <- melt(intime_dates, id.vars = c("EmployeeID"), value.name = "in_datetime", variable.name = "Workdate")
outtime_long <- melt(outtime_dates, id.vars = c("EmployeeID"), value.name = "out_datetime", variable.name = "Workdate")

employee_timesheets <- merge(intime_long, outtime_long, by=c("EmployeeID", "Workdate"))

#CALCULATE THE HOURLY TIME FOR EACH EMPLOYEE BY DAY

employee_timesheets$workhours <- employee_timesheets$out_datetime - employee_timesheets$in_datetime
employee_timesheets$workweek <- week(as.Date(gsub("X", "", employee_timesheets$Workdate), format= "%Y.%m.%d"))



#FLAG RECORDS WITH MISSING INTIME AND OUTTIME TO BE VACATION OR TIME-OFF

employee_timesheets$time_off <- ifelse(is.na(employee_timesheets$in_datetime) &
                                                    is.na(employee_timesheets$out_datetime),1,0)

summary(employee_timesheets)

#CHECK FOR MISSING AND DUPLICATE VALUES

missing_data <- plot_missing(employee_timesheets)

duplicates <- sum(duplicated(employee_timesheets))

#AGGREGATE TIMESHEET VALUES FOR EMPLOYEES

aggr_timesheet <-   employee_timesheets %>% 
                    group_by(EmployeeID) %>%
                    summarise(total_timeoff = sum(time_off)) %>%
                    arrange(EmployeeID)  

weekly_time_rollup <-   employee_timesheets %>% 
                    group_by(EmployeeID, workweek) %>%
                    summarise(total_weekly_hrs = sum(workhours, na.rm = TRUE)) %>%
                    arrange(EmployeeID)  

weekly_hrs <- weekly_time_rollup %>% 
                  group_by(EmployeeID) %>%
                  summarise(avg_weekly_hrs = mean(total_weekly_hrs, na.rm = TRUE)) %>%
                  arrange(EmployeeID)

employee_aggr_timesheets <- merge(aggr_timesheet, weekly_hrs, by=c("EmployeeID"))

# OUTLIER ANALYSIS ON TIME-OFF AND AVERAGE WEEKLY HOURS

boxplot(employee_aggr_timesheets$total_timeoff)

rnd_weekly_hrs <- as.integer(employee_aggr_timesheets$avg_weekly_hrs)
boxplot(rnd_weekly_hrs)



# -------------------------CLEAN AND MERGE SURVEY AND GENERAL DATASETS WITH TIME------------------------------#

 
combined_data <- list(general_data, mgr_survey_data,emp_survey_data, employee_aggr_timesheets) %>%
                 reduce(left_join, by = "EmployeeID")

#CHECK FOR MISSING AND DUPLICATES IN DATA

missing_data <- plot_missing(combined_data)

#EXCLUDE RECORDS WITH MISSING VALUES. THE RECORDS ARE LESS THAN 1% OF TOTAL OBSERVATIONS

combined_data <- na.omit(combined_data)
missing_data <- plot_missing(combined_data)

duplicates <- sum(duplicated(combined_data))

#CHECK IF EMPLOYMENT YEARS ARE VALID

check_empl_yrs <- subset(combined_data, combined_data$TotalWorkingYears < YearsAtCompany)

combined_data <- combined_data[!(combined_data$Age==18 & combined_data$Over18=="Y"),]
combined_data <- combined_data[!(combined_data$Age==18 & (combined_data$Education==3 | combined_data$Education==4 | combined_data$Education==5)),]
combined_data <- combined_data[!(combined_data$TotalWorkingYears==0 & combined_data$PercentSalaryHike > 0),]
combined_data <- combined_data[!(combined_data$TotalWorkingYears==40 & combined_data$Age==58 & combined_data$Education==3),]
combined_data <- combined_data[!(combined_data$JobInvolvement==1 & (combined_data$PerformanceRating==4 | combined_data$PerformanceRating==3)),]

#BOXPLOT NUMERIC VARIABLES AND EXCLUDE OUTLIERS APPROPRIATELY

boxplot(as.integer(combined_data$MonthlyIncome))
summary(combined_data$MonthlyIncome)
quantile(as.integer(combined_data$MonthlyIncome),0.95)

boxplot(combined_data$TotalWorkingYears)
summary(combined_data$TotalWorkingYears)
quantile(combined_data$TotalWorkingYears,0.90)

###############################################################################################################
#                                            MODEL PREP STEPS                                                 #
###############################################################################################################

#CREATION OF DUMMY VARIABLES 

category_vars <- combined_data[,c(3,4,6,7,10,11,12,13,16,19,25,26,27,28,29)]

#CONVERT THE CATEGORIC VARIABLES INTO FACTOR TO TRANSPOSE CORRECTLY

category_vars_f <- data.frame(sapply(category_vars, function(x) factor(x)))

#CREATE DUMMY VARIABLES TO BE USED FOR MODELLING

levels(combined_data$Attrition)<-c(1,0)
combined_data$Attrition <- as.numeric(levels(combined_data$Attrition))[combined_data$Attrition]

category_vars_trans <- dummy.data.frame(category_vars_f, sep = "_")

combined_data_dummy<- cbind(combined_data[,-c(3,4,6,7,8,10,11,12,13,16,18,19,25,26,27,28,29)],category_vars_trans)

#SCALE ALL CONTINUOUS VARIABLES PRIOR TO MODELLING

scale_vars <- dput(names(combined_data_dummy[,c(1,3,5:14)]))
scale_vars

master_data <- combined_data_dummy %>% mutate_if(names(.) %in% scale_vars, scale)

###############################################################################################################
#                                            MODEL BUILDING                                                   #
###############################################################################################################

# CREATE TRAINING AND TEST DATASETS

set.seed(100)

# GENERATE ROW INDICES FOR 70% OF RECORDS
trainindices= sample(1:nrow(master_data), 0.7*nrow(master_data))

#GENERATE TRAINING DATA
train_hr = master_data[trainindices,]

test_hr = master_data[-trainindices,]

#BUILD LOGISTIC REGRESSION MODEL TO PREDICT ATTRITION

model_1 = glm(Attrition ~., data = train_hr [,-4], family = "binomial")
summary(model_1)

#USE STEPAIC TO CONDUCT MULTIPLE ITERATIONS OF FORWARD AND BACKWARD SELECTION AND OBTAIN THE FORMULA FOR 
#CONSECUTIVE MODELS

step <- stepAIC(model_1, direction = "both")
step

model_2 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 total_timeoff + avg_weekly_hrs + `BusinessTravel_Non-Travel` + 
                 BusinessTravel_Travel_Frequently + `Department_Human Resources` + 
                 Education_1 + Education_2 + Education_3 + Education_4 + `EducationField_Life Sciences` + 
                 EducationField_Medical + JobLevel_2 + `JobRole_Healthcare Representative` + 
                 `JobRole_Manufacturing Director` + `JobRole_Research Director` + 
                 `JobRole_Sales Executive` + MaritalStatus_Divorced + MaritalStatus_Married + 
                 StockOptionLevel_0 + StockOptionLevel_1 + StockOptionLevel_2 + 
                 JobInvolvement_3 + EnvironmentSatisfaction_1 + EnvironmentSatisfaction_2 + 
                 EnvironmentSatisfaction_3 + JobSatisfaction_1 + JobSatisfaction_2 + 
                 JobSatisfaction_3 + WorkLifeBalance_1 + WorkLifeBalance_3, 
               family = "binomial", data = train_hr[, -4])
summary(model_2)
vif(model_2)

#OBSERVE HIGH VIF VALUE VARIABLES (>5) AND NOTE THE CORRESPONDING P VALUES FOR THE VARIABLES. DROP
#THE ONES THAT ARE NOT SIGNIFICANT( p> 0.05) AND BUILD THE NEXT MODEL

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_weekly_hrs + `BusinessTravel_Non-Travel` + 
                 BusinessTravel_Travel_Frequently + `Department_Human Resources` + 
                 JobLevel_2 + 
                 `JobRole_Manufacturing Director` + `JobRole_Research Director` + 
                 `JobRole_Sales Executive` + MaritalStatus_Divorced + MaritalStatus_Married + 
                 JobInvolvement_3 + EnvironmentSatisfaction_1 + EnvironmentSatisfaction_2 + 
                 JobSatisfaction_1 + JobSatisfaction_2 + 
                 JobSatisfaction_3 + WorkLifeBalance_1 + WorkLifeBalance_3, 
               family = "binomial", data = train_hr[, -4])
summary(model_3)
vif(model_3)

#REPEAT ITERATIONS OF ABOVE VARIABLE EXCLUSION AND MODEL BUILDING, KEEPING ONLY SIGNIFICANT VARS WITH LOW VIF

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_weekly_hrs + 
                 BusinessTravel_Travel_Frequently + `Department_Human Resources` + 
                 JobLevel_2 + 
                 MaritalStatus_Divorced + MaritalStatus_Married + 
                 EnvironmentSatisfaction_1 + EnvironmentSatisfaction_2 + 
                 JobSatisfaction_1 + WorkLifeBalance_1 , 
               family = "binomial", data = train_hr[, -4])
summary(model_4)
vif(model_4)

#EXCLUDE LESS SIGNIFICANT VARIABLES

model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 avg_weekly_hrs + 
                 BusinessTravel_Travel_Frequently + `Department_Human Resources` + 
                  MaritalStatus_Divorced + MaritalStatus_Married + 
                 EnvironmentSatisfaction_1 + 
                 JobSatisfaction_1 + WorkLifeBalance_1 , 
               family = "binomial", data = train_hr[, -4])
summary(model_5)
vif(model_5)


#ACCEPTED THE MODEL ABOVE WITH LOW VIF VALUES AND LOW P VALUES, AFTER MULTIPLE ITERATIONS ABOVE. RUN THE MODEL 
#ON THE TEST DATA AND OBSERVE RESULTS. USE TYPE = RESPONSE TO GET PROBABILITIES

predict_attrition <- predict(model_5, type = "response" , newdata = test_hr[,-4])
test_hr$prob_attrition <- predict_attrition

#USE PROBABILITY CUT-OFF OF 70% AND OBSERVE RESULTS

###############################################################################################################
#                                            MODEL EVALUATION                                                 #
###############################################################################################################


predicted_attrition <- factor(ifelse(test_hr$prob_attrition >= 0.7, "Yes", "No"))
actual_attrition <- factor(ifelse(test_hr$Attrition==1, "Yes", "No"))

model_accuracy <- as.data.frame(table(actual_attrition, predicted_attrition))

ggplot(data = model_accuracy, 
       mapping = aes(x = predicted_attrition , y = actual_attrition)) +
       geom_tile(aes(fill = Freq)) +
       geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
       scale_fill_gradient(low = "yellow", high = "red", trans = "log")

confusionMatrix(predicted_attrition, actual_attrition, positive = "Yes") 


#WHILE THE SENSITIVITY AND ACCURACY ARE HIGH, SPECIFICITY IS LOW.DETERMINE OPTIMAL CUT-OFF VALUE

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_hr$prob_attrition >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, actual_attrition)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# CREATE CUT-OFF VALUES FROM 0.01 to 0.85 FOR PLOTTING AND INITIALISING A MATRIX OF 100X3 
# NOTE : CODE SECTION BELOW IS SIMILAR TO THE ONE USED IN TELECOM CHURN LECTURE

# SUMMARY OF TEST PROBABILITY

summary(predict_attrition)

s = seq(.01,.85,length=1000)
OUTn = matrix(0,1000,3)

for(i in 1:1000)
{
  OUTn[i,] = perform_fn(s[i])
} 

warnings()

plot(s, OUTn[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUTn[,2],col="darkgreen",lwd=2)
lines(s,OUTn[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUTn[,1]-OUTn[,2])<0.01)]
cutoff <- mean(cutoff)

#CHOOSE A CUTOFF VALUE OF 0.838, BASED ON THE ANALYSIS ABOVE & VIEW RESULTS

test_cutoff_attr <- factor(ifelse(predict_attrition >=0.838, "Yes", "No"))

hr_attr_cm <- confusionMatrix(test_cutoff_attr, actual_attrition, positive = "Yes")
df_results <- as.data.frame(as.matrix(hr_attr_cm, what = "classes"))
colnames(df_results)[1] <- "VALUE"
 

df_results

# A VALUE OF 0.838 FOR PROBABILITY RESULTS IN THE BEST VALUES FOR ACCURACY, SENSITIVITY AND SPECIFICITY

###############################################################################################################
#                                        KS- STATISTIC & LIFT & GAIN CHARTS                                   #
###############################################################################################################
#NOTE : CODE BELOW IS ADAPTED FROM TELECOM SOLUTION FILE USED IN LECTURE

#------------------------------------------KS STATISTIC CALCULATION-------------------------------------------#

test_cutoff_attr <- ifelse(test_cutoff_attr=="Yes",1,0)
test_actual_attr <- ifelse(actual_attrition=="Yes",1,0)


#APPLY ON TEST DATA

pred_object_test<- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

#PLOT THE ROC CURVE FOR THE TRAINING DATA

plot(performance_measures_test, col = "red", main = "ROC CURVE FOR TRAIN DATA")
abline(0,1, lty = 8, col = "blue")

#------------------------------------LIFT & GAIN CHARTS & TABLE GENERATION------------------------------------#

#CONVERT PREDICTED AND ACTUAL ATTRITION VECTORS INTO NUMBERS

predicted_int <- round(test_hr$prob_attrition, digits = 2)
actual_int <- as.integer(test_hr$Attrition)


lift <- function(labels , pred_val,groups=10) {
  
  helper = data.frame(cbind(labels , pred_val))
  helper[,"bucket"] = ntile(-helper[,"pred_val"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)),
           non_attr = total-totalresp,
           cum_non_attr = cumsum(non_attr),
           ks_cal = Gain-(cum_non_attr/sum(non_attr)*100) )
  return(gaintable)
}

Attr_decile = lift(actual_int, predicted_int, groups = 10)

max_KS <- max(Attr_decile$ks_cal)
max_KS


#THE MAXIMUM KS VALUE OBSERVED IN THE ABOVE CALCULATION IS SIMILAR TO THE KS VALUE OBTAINED ABOVE, WHICH IS 
#GREATER THAN 40%, SO MODEL CAN BE ACCEPTED


