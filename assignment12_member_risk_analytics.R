#########################################
# HEALTHCARE - RISK ANALYTICS           #
# SUBMITTED BY : RUPA KADAMBI           #
#                ROLL NO : DDA1810283   #
#########################################

##############################################################################################################
#                                             PRELIMINARY STEPS - DATA IMPORTS                               #
##############################################################################################################

# SET LIBRARY FOR PROJECT #

lapply(.packages(all.available = TRUE), function(x)library(x,character.only = TRUE))

setwd("C:/Users/rupak/Documents/Training/healthcare/assignment")

#IMPORT DATA INTO R #

diabetic_data <-  read.csv(file="diabetic_data.csv", head = TRUE, sep = ",")


##############################################################################################################
#                                    PRELIMINARY STEPS - DATA PREPARATION & EDA                              #             
##############################################################################################################


View(diabetic_data)


#MISSING VALUES ARE BEING CODED AS ?. READ IN THE FILE, SETTING MISSING VALUES AS NA

diabetic_clean <- readr::read_csv(file = "diabetic_data.csv", na = "?")


#REVIEW STRUCTURE OF DATA

str(diabetic_clean)

#FIX COLUMN NAMES WITH HYPHEN IN THEM

names(diabetic_clean)[names(diabetic_clean) == 'glipizide-metformin'] <- "glipizide_metformin"
names(diabetic_clean)[names(diabetic_clean) == 'glimepiride-pioglitazone'] <- "glimepiride_pioglitazone"
names(diabetic_clean)[names(diabetic_clean) == 'metformin-rosiglitazone'] <- "metformin_rosiglitazone"
names(diabetic_clean)[names(diabetic_clean) == 'metformin-pioglitazone'] <- "metformin_pioglitazone"
names(diabetic_clean)[names(diabetic_clean) == 'glyburide-metformin'] <- "glyburide_metformin"

 

diabetic_clean$patient_nbr <- as.character(diabetic_clean$patient_nbr)
diabetic_clean$encounter_id <- as.character(diabetic_clean$encounter_id)

#CONVERT CATEGORICAL VARS TO FACTORS 

factor_cols <- c("admission_type_id", "discharge_disposition_id", "admission_source_id", "time_in_hospital",
                 "gender", "age", "diag_1", "diag_2", "diag_3", "max_glu_serum",
                  "A1Cresult", "insulin",  "change", "diabetesMed", "readmitted")


diabetic_clean[factor_cols] <- lapply(diabetic_clean[factor_cols], as.factor)

sapply(diabetic_clean, class)

#CHECK MISSING VALUE DISTRIBUTION

missing_vals <- plot_missing(diabetic_clean)

#WEIGHT, MEDICAL SPECIALTY AND PAYER CODE ARE EITHER ALL NULL VALUES OR MOSTLY NULL AND CAN BE EXCLUDED FROM FURTHER 
#ANALYSIS. ADDITIONALLY, ENCOUNTER ID CAN BE EXCLUDED AS IT HAS NO BEARING ON DETERMINING HIGH RISK MEMBERS

#----------------------------------------REVIEW OF CATEGORICAL VARIABLES-----------------------------------------------#


ggplot(diabetic_clean, aes(race)) + geom_bar(fill = "#0073C2FF")  

#UPDATE VALUE OF RACE TO UNKNOWN WHEN MISSING

diabetic_clean$race[is.na(diabetic_clean$race)] <- "Unknown"
diabetic_clean$race <- as.factor(diabetic_clean$race)

ggplot(diabetic_clean, aes(gender)) + geom_bar(fill = "#0073C2FF") 
ggplot(diabetic_clean, aes(age)) + geom_bar(fill = "#0073C2FF")  
ggplot(diabetic_clean, aes(admission_type_id)) + geom_bar(fill = "#0073C2FF")  
ggplot(diabetic_clean, aes(time_in_hospital)) + geom_bar()


table(diabetic_clean$discharge_disposition_id) 
table(diabetic_clean$admission_source_id) 

#Medication variables can be dropped as diabetes medication has been included that indicates if the medication is diabetes
#related. Also keeping Insulin, per lectures



#----------------------------------------REVIEW OF NUMERIC VARIABLES-----------------------------------------------#

diabetic_clean %>%
   keep(is.numeric) %>%
   gather() %>%
   ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram(color = "black", fill = "yellow")

#-------------------------------------CREATE ADDITIONAL VARIABLES--------------------------------------------------#

ggplot(diabetic_clean, aes(readmitted)) + geom_bar()

diabetic_clean$readmitted <- gsub("<30", "YES", diabetic_clean$readmitted)
diabetic_clean$readmitted <- gsub(">30", "YES", diabetic_clean$readmitted)
diabetic_clean$readmitted <- as.factor(diabetic_clean$readmitted)

#CREATE VARIABLE COMORBIDITY

diabetic_clean$diag1_num <- as.numeric(substr(diabetic_clean$diag_1,1,3))
diabetic_clean$diag2_num <- as.numeric(substr(diabetic_clean$diag_2,1,3))
diabetic_clean$diag3_num <- as.numeric(substr(diabetic_clean$diag_3,1,3))
diabetic_clean$diab_flag <- with(diabetic_clean, ifelse(diag1_num == 250, "Y", ifelse(diag2_num == 250,"Y", 
                                                 ifelse(diag3_num ==250, "Y",NA))))
 
table(diabetic_clean$diab_flag)

diabetic_clean$circ_flag <- with(diabetic_clean, ifelse(diag1_num >= 390 & diag1_num <= 459 ,  "Y", 
                                                        ifelse(diag2_num >= 390 & diag2_num <= 459, "Y",
                                                        ifelse(diag3_num >= 390 & diag3_num <= 459, "Y",NA))))

table(diabetic_clean$circ_flag)
diabetic_clean$diab_flag[is.na(diabetic_clean$diab_flag)] <- "N"
diabetic_clean$circ_flag[is.na(diabetic_clean$circ_flag)] <- "N"
diabetic_clean$diab_flag <- as.factor(diabetic_clean$diab_flag)
diabetic_clean$circ_flag <- as.factor(diabetic_clean$circ_flag)



attach(diabetic_clean)

diabetic_clean$comorbid[diab_flag != "Y" & circ_flag != "Y" ]  <- "0"
diabetic_clean$comorbid[diab_flag == "Y" & circ_flag != "Y" ]  <- "1"
diabetic_clean$comorbid[diab_flag != "Y" & circ_flag == "Y" ]  <- "2"
diabetic_clean$comorbid[diab_flag == "Y" & circ_flag == "Y" ]  <- "3"

detach(diabetic_clean)

diabetic_clean$comorbid <- as.factor(diabetic_clean$comorbid)

ggplot(diabetic_clean, aes(comorbid)) + geom_bar(fill = "#0073C2FF")

str(diabetic_clean)

#CONVERT 2 LEVEL FACTOR VARIABLES INTO NUMERIC VARIABLES WITH BINARY VALUES & DROP UNWANTED VARIABLES#

diab_leveled <- subset(diabetic_clean, select = -c(weight, payer_code, medical_specialty, metformin, repaglinide, 
                                                   nateglinide, chlorpropamide, glimepiride, glipizide, glyburide, pioglitazone,
                                                   rosiglitazone, acarbose, miglitol, tolazamide, glyburide_metformin, glipizide_metformin,
                                                   glimepiride_pioglitazone, metformin_rosiglitazone, metformin_pioglitazone, examide,
                                                   citoglipton, acetohexamide, troglitazone, tolbutamide, diag_1, diag1_num, diag_2, diag2_num, diag_3, diag3_num))

levels(diab_leveled$change)<-c(1,0)
diab_leveled$change <- as.numeric(levels(diab_leveled$change))[diab_leveled$change]

levels(diab_leveled$diabetesMed)<-c(1,0)
diab_leveled$diabetesMed <- as.numeric(levels(diab_leveled$diabetesMed))[diab_leveled$diabetesMed]

levels(diab_leveled$readmitted)<-c(1,0)
diab_leveled$readmitted <- as.numeric(levels(diab_leveled$readmitted))[diab_leveled$readmitted]

levels(diab_leveled$diab_flag)<-c(1,0)
diab_leveled$diab_flag <- as.numeric(levels(diab_leveled$diab_flag))[diab_leveled$diab_flag]

levels(diab_leveled$circ_flag)<-c(1,0)
diab_leveled$circ_flag <- as.numeric(levels(diab_leveled$circ_flag))[diab_leveled$circ_flag]


str(diab_leveled)

#CONVERT 2+ LEVEL FACTOR VARIABLES INTO NUMERIC VARIABLES WITH BINARY VALUES


num_race <- data.frame(model.matrix( ~race, data = diab_leveled))
num_race <- num_race[,-1]

num_gender <- data.frame(model.matrix( ~gender, data = diab_leveled))
num_gender <- num_gender[,-1]

num_age <- data.frame(model.matrix( ~age, data = diab_leveled))
num_age <- num_age[,-1]

num_admission_type_id <- data.frame(model.matrix( ~admission_type_id, data = diab_leveled))
num_admission_type_id <- num_admission_type_id[,-1]

num_discharge_disposition_id <- data.frame(model.matrix( ~discharge_disposition_id, data = diab_leveled))
num_discharge_disposition_id <- num_discharge_disposition_id[,-1]

num_admission_source_id<- data.frame(model.matrix( ~admission_source_id, data = diab_leveled))
num_admission_source_id <- num_admission_source_id[,-1]

num_time_in_hospital <- data.frame(model.matrix( ~time_in_hospital, data = diab_leveled))
num_time_in_hospital <- num_time_in_hospital[,-1]

num_max_glu_serum <- data.frame(model.matrix( ~max_glu_serum, data = diab_leveled))
num_max_glu_serum <- num_max_glu_serum[,-1]

num_A1Cresult <- data.frame(model.matrix( ~A1Cresult, data = diab_leveled))
num_A1Cresult <- num_A1Cresult[,-1]

num_insulin <- data.frame(model.matrix( ~insulin, data = diab_leveled))
num_insulin <- num_insulin[,-1]

num_comorbid <- data.frame(model.matrix( ~comorbid, data = diab_leveled))
num_comorbid <- num_comorbid[,-1]

diab_limited<- subset(diab_leveled, select = -c(race,gender ,age ,admission_type_id ,discharge_disposition_id ,admission_source_id,
                                                time_in_hospital, max_glu_serum ,A1Cresult, insulin, comorbid))
diab_combined <- cbind(diab_limited, do.call(cbind, lapply(ls(pattern = "num_"), get)))

str(diab_combined)

#SCALE ALL NUMERIC VARIABLES PRIOR TO MODELLING

scale_vars <- dput(names(diab_combined[,c(3:9)]))
scale_vars

model_data <- diab_combined %>% mutate_if(names(.) %in% scale_vars, scale) 


###############################################################################################################
#                                            MODEL BUILDING                                                   #
###############################################################################################################

#----------------------------------------LOGISTIC REGRESSION-------------------------------------------------#
# CREATE TRAINING AND TEST DATASETS

set.seed(100)

# GENERATE ROW INDICES FOR 70% OF RECORDS
trainindices= sample(1:nrow(model_data), 0.7*nrow(model_data))

#GENERATE TRAINING DATA
train_risk = model_data[trainindices,]
test_risk = model_data[-trainindices,]

#BUILD LOGISTIC REGRESSION MODEL TO PREDICT RE-ADMISSION

model_1 = glm(readmitted ~., data = train_risk [,-c(1,2)], family = "binomial")
summary(model_1)

#USE STEPAIC TO CONDUCT MULTIPLE ITERATIONS OF FORWARD AND BACKWARD SELECTION AND OBTAIN THE FORMULA FOR 
#CONSECUTIVE MODELS

step <- stepAIC(model_1, direction = "both")
step

model_2 <- glm(formula = readmitted ~ num_lab_procedures + num_procedures + number_outpatient + 
                 number_emergency + number_inpatient + number_diagnoses + 
                 change + diabetesMed + diab_flag + circ_flag + A1CresultNone + 
                 A1CresultNorm + admission_source_id2 + admission_source_id4 + 
                 admission_source_id5 + admission_source_id6 + admission_source_id7 + 
                 admission_source_id9 + admission_source_id17 + admission_source_id20 + 
                 admission_type_id2 + admission_type_id5 + admission_type_id6 + 
                 admission_type_id7 + admission_type_id8 + age.10.20. + age.20.30. + 
                 age.30.40. + age.40.50. + age.50.60. + age.60.70. + age.70.80. + 
                 age.80.90. + age.90.100. + comorbid1 + discharge_disposition_id3 + 
                 discharge_disposition_id5 + discharge_disposition_id6 + discharge_disposition_id11 + 
                 discharge_disposition_id13 + discharge_disposition_id14 + 
                 discharge_disposition_id15 + discharge_disposition_id18 + 
                 discharge_disposition_id19 + discharge_disposition_id22 + 
                 discharge_disposition_id23 + discharge_disposition_id25 + 
                 discharge_disposition_id28 + genderMale + insulinNo + insulinSteady + 
                 insulinUp + max_glu_serum.300 + raceAsian + raceHispanic + 
                 raceOther + raceUnknown + time_in_hospital2 + time_in_hospital3 + 
                 time_in_hospital4 + time_in_hospital5 + time_in_hospital6 + 
                 time_in_hospital7 + time_in_hospital8 + time_in_hospital9 + 
                 time_in_hospital10 + time_in_hospital11 + time_in_hospital12 + 
                 time_in_hospital13 + time_in_hospital14 , data = train_risk [,-c(1,2)], family = "binomial")

summary(model_2)
vif(model_2)

#OBSERVE HIGH VIF VALUE VARIABLES (>5) AND NOTE THE CORRESPONDING P VALUES FOR THE VARIABLES. DROP
#THE ONES THAT ARE NOT SIGNIFICANT( p> 0.05) AND BUILD THE NEXT MODEL

model_3 <- glm(formula = readmitted ~ num_procedures + number_outpatient + 
                 number_emergency + number_inpatient + number_diagnoses + 
                 change + diabetesMed + diab_flag + circ_flag + A1CresultNone + 
                 A1CresultNorm + admission_source_id2 + admission_source_id4 + 
                 admission_source_id5 + admission_source_id6 + admission_source_id7 + 
                 admission_source_id9 + admission_source_id17 + admission_source_id20 + 
                 admission_type_id2 + admission_type_id5 + admission_type_id6 + 
                 admission_type_id8 + age.10.20. + age.20.30. + 
                 age.30.40. + age.40.50. + age.50.60. + age.60.70. + age.70.80. + 
                 age.80.90. + age.90.100. + comorbid1 + discharge_disposition_id3 + 
                 discharge_disposition_id5 + discharge_disposition_id6 + 
                 discharge_disposition_id13 + discharge_disposition_id14 + 
                 discharge_disposition_id18 + discharge_disposition_id22 + 
                 discharge_disposition_id23 + discharge_disposition_id25 + 
                 discharge_disposition_id28 + genderMale + insulinNo + insulinSteady + 
                 insulinUp + max_glu_serum.300 + raceAsian + raceHispanic + 
                 raceOther + raceUnknown + time_in_hospital2 + time_in_hospital3 + 
                 time_in_hospital4 + time_in_hospital5 + time_in_hospital6 + 
                 time_in_hospital7 + time_in_hospital8 + time_in_hospital9 + 
                 time_in_hospital10 + time_in_hospital11 + time_in_hospital12 + 
                 time_in_hospital14 , data = train_risk [,-c(1,2)], family = "binomial")
summary(model_3)
vif(model_3)

model_4 <- glm(formula = readmitted ~ num_procedures + number_outpatient + 
                 number_emergency + number_inpatient + number_diagnoses + 
                 change + diabetesMed + diab_flag + circ_flag + A1CresultNone + 
                 A1CresultNorm + admission_source_id2 + admission_source_id4 + 
                 admission_source_id5 + admission_source_id6 + admission_source_id7 + 
                 admission_source_id9 + admission_source_id17 + admission_source_id20 + 
                 admission_type_id2 + admission_type_id5 + admission_type_id6 + 
                 age.10.20. + age.20.30. + 
                 age.30.40. + age.40.50. + age.50.60. + age.60.70. + age.70.80. + 
                 age.80.90. + comorbid1 + discharge_disposition_id5 + discharge_disposition_id6 + 
                 discharge_disposition_id13 + discharge_disposition_id14 + 
                 discharge_disposition_id22 + discharge_disposition_id25 + 
                 discharge_disposition_id28 + genderMale + insulinNo + insulinSteady + 
                 insulinUp + max_glu_serum.300 + raceAsian + raceHispanic + 
                 raceOther + raceUnknown + time_in_hospital2 + time_in_hospital3 + 
                 time_in_hospital4 + time_in_hospital5 + time_in_hospital6 + 
                 time_in_hospital7 + time_in_hospital8 + time_in_hospital9 + 
                 time_in_hospital10 + time_in_hospital11 + time_in_hospital12 + 
                 time_in_hospital14 , data = train_risk [,-c(1,2)], family = "binomial")
summary(model_4)
vif(model_4)


model_5 <- glm(formula = readmitted ~ num_procedures + number_outpatient + 
                 number_emergency + number_inpatient + number_diagnoses + 
                 change + diabetesMed + diab_flag + circ_flag + A1CresultNone + 
                 A1CresultNorm + admission_source_id2 + admission_source_id4 + 
                 admission_source_id5 + admission_source_id6 + admission_source_id7 + 
                 admission_source_id9 + admission_source_id17 + admission_source_id20 + 
                 admission_type_id2 + admission_type_id5 + admission_type_id6 + 
                 age.10.20. + age.20.30. + age.30.40. + age.40.50. + age.50.60. +  
                 age.80.90. + comorbid1 + discharge_disposition_id5 + discharge_disposition_id6 + 
                 discharge_disposition_id13 + discharge_disposition_id14 + 
                 discharge_disposition_id22 + discharge_disposition_id25 + 
                 discharge_disposition_id28 + genderMale + insulinNo + insulinSteady + 
                 insulinUp + max_glu_serum.300 + raceAsian + raceHispanic + 
                 raceOther + raceUnknown + time_in_hospital2 + time_in_hospital3 + 
                 time_in_hospital4 + time_in_hospital5 + time_in_hospital6 + 
                 time_in_hospital7 + time_in_hospital8 + time_in_hospital9 + 
                 time_in_hospital10 + time_in_hospital11 + time_in_hospital12 + 
                 time_in_hospital14 , data = train_risk [,-c(1,2)], family = "binomial")
summary(model_5)
vif(model_5)

#ACCEPTED THE MODEL ABOVE WITH LOW VIF VALUES AND LOW P VALUES, AFTER MULTIPLE ITERATIONS ABOVE. RUN THE MODEL 
#ON THE TEST DATA AND OBSERVE RESULTS. USE TYPE = RESPONSE TO GET PROBABILITIES

predict_risk <- predict(model_5, type = "response" , newdata = test_risk[,-c(1,2)])
test_risk$prob_risk <- predict_risk

#USE PROBABILITY CUT-OFF OF 70% FOR HIGH RISK MEMBERS AND OBSERVE RESULTS


###############################################################################################################
#                                            MODEL EVALUATION                                                 #
###############################################################################################################

predicted_risk <- factor(ifelse(test_risk$prob_risk >= 0.7, "Yes", "No"))
actual_risk <- factor(ifelse(test_risk$readmitted==1, "Yes", "No"))

model_accuracy <- as.data.frame(table(actual_risk, predicted_risk))

ggplot(data = model_accuracy, 
       mapping = aes(x = predicted_risk , y = actual_risk)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "yellow", high = "red", trans = "log")

library(caret)

confusionMatrix(predicted_risk, actual_risk, positive = "Yes") 


perform_fn <- function(cutoff) 
{
  predicted_risk <- factor(ifelse(test_risk$prob_risk >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_risk, actual_risk)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# CREATE CUT-OFF VALUES FROM 0.01 to 0.80 FOR PLOTTING AND INITIALISING A MATRIX OF 100X3 
# NOTE : CODE SECTION BELOW IS SIMILAR TO THE ONE USED IN TELECOM CHURN LECTURE

# SUMMARY OF TEST PROBABILITY

summary(predicted_risk)

s = seq(.01,.80,length=1000)
OUTn = matrix(0,1000,3)

for(i in 1:1000)
{
  OUTn[i,] = perform_fn(s[i])
} 

test_cutoff_risk <- factor(ifelse(predict_risk >=0.55, "Yes", "No"))

diab_risk_cm <- confusionMatrix(test_cutoff_risk, actual_risk, positive = "Yes")
df_results <- as.data.frame(as.matrix(diab_risk_cm, what = "classes"))
colnames(df_results)[1] <- "VALUE"


df_results

# A VALUE OF 0.55 FOR PROBABILITY RESULTS IN THE BEST VALUES FOR ACCURACY (62%), SENSITIVITY AND SPECIFICITY


#--------------------------------------------------------RANDOM FOREST--------------------------------------------------------#


rf_diab_data <- subset(diabetic_clean, select = -c(weight, payer_code, medical_specialty, metformin, repaglinide, 
                                                   nateglinide, chlorpropamide, glimepiride, glipizide, glyburide, pioglitazone,
                                                   rosiglitazone, acarbose, miglitol, tolazamide, glyburide_metformin, glipizide_metformin,
                                                   glimepiride_pioglitazone, metformin_rosiglitazone, metformin_pioglitazone, examide,
                                                   citoglipton, acetohexamide, troglitazone, tolbutamide, diag_1, diag1_num, diag_2, diag2_num, diag_3, diag3_num))

str(rf_diab_data)

# Split the data into train and test
ntrain <- as.integer(nrow(rf_diab_data)*0.8)
traindata <- rf_diab_data[1:ntrain, ]
testdata <- rf_diab_data[(ntrain+1):nrow(rf_diab_data), ]


# Build the random forest
library(randomForest)
set.seed(71)
data.rf <- randomForest(readmitted ~ ., data=traindata, proximity=FALSE,
                        ntree=500, mtry=6, do.trace=TRUE, na.action=na.omit)

data.rf
testPred <- predict(data.rf, newdata=testdata)
table(testPred, testdata$readmitted)
mean(testPred == testdata$readmitted)

#THE HIGHEST ACCURACY FOR RANDOM FOREST MODEL IS 63.55%, WHICH IS SLIGHTLY BETTER THAN THE REGRESSION MODEL

###############################################################################################################
#                                            CONCLUSION/RECOMMENDATION                                        #
###############################################################################################################

#BOTH MODELS FARE ALMOST THE SAME DURING TESTING WITH AN ACCURACY RANGING BETWEEN 62-63%. THE GOAL OF THE MODEL
#IS TO PREDICT THE PROBABILITY OF MEMBERS BEING RE-ADMITTED. FOR THIS PURPOSE, THE REGRESSION MODEL MAY BE MORE SUITABLE.
#USING THE PREDICTED PROBABILITIES, THE MEMBERS CAN BE CLASSIFIED AS HIGH RISK, MEDIUM RISK AND LOW RISK

###############################################################################################################
#                                            MEMBER STRATIFICATION                                            #
###############################################################################################################

#APPLY REGRESSION MODEL ON ORIGINAL DATA. CALCULATE AVERAGE READMIT PROBABILITY PER MEMBER AND STRATIFY MEMBERS BASED
#ON THIS AVERAGE PROBABILITY

stratify <- predict(model_5, type = "response" , newdata = model_data)
model_data$readmit_probability <- stratify

library(dplyr)
member_summary <-   model_data %>% 
    group_by(patient_nbr) %>%
  summarise(avg_readmit_prob = mean(readmit_probability)) 

attach(member_summary)

member_summary$risk_category[avg_readmit_prob >= 0.70] <- "HIGH RISK"
member_summary$risk_category[avg_readmit_prob >= 0.30 & avg_readmit_prob < 0.7 ] <- "MEDIUM RISK"
member_summary$risk_category[avg_readmit_prob <0.30] <- "LOW RISK"

detach(member_summary)

member_summary$risk_category <- as.factor(member_summary$risk_category)

ggplot(member_summary, aes(risk_category)) + geom_bar(fill = "#0073C2FF")

#IT CAN BE OBSERVED THAT MOST MEMBERS ARE MEDIUM RISK. EFFORTS SHOULD BE MADE TO LOWER THEIR RISK OF READMISSION

