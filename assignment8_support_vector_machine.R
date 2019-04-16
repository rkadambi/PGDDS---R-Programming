#########################################
# DIGIT RECOGNITION ANALYTICS           #
# SUBMITTED BY : RUPA KADAMBI           #
# ROLL NO : DDA1810283                  #
#########################################

##############################################################################################################
#                                             PRELIMINARY STEPS - DATA IMPORTS                               #
##############################################################################################################

# SET LIBRARY FOR PROJECT #

lapply(.packages(all.available = TRUE), function(x)library(x,character.only = TRUE))
library(readr)
library(caret)
library(kernlab)
library(dplyr)
library(parallel)
library(ggplot2)
library(gridExtra)


setwd("C:/Users/rupak/Documents/Training/SVM/assignment")

#IMPORT DATA INTO R #

train_digit_data <- read_csv(file = "mnist_train.csv", col_names = FALSE) 
test_digit_data <- read_csv(file = "mnist_test.csv", col_names = FALSE)


##############################################################################################################
#                                    PRELIMINARY STEPS - DATA PREPARATION & EDA                              #             
##############################################################################################################

View(train_digit_data)
str(train_digit_data)
View(test_digit_data)
str(test_digit_data)

#THE COLUMN NAME IS MISSING FOR ALL COLUMNS. RENAME THE FIRST COLUMN AS THAT IS THE TARGET VARIABLE.

colnames(train_digit_data)[1] <- c("digit")
colnames(test_digit_data)[1] <- c("digit")


#--------------------------------CHECK FOR MISSING AND DUPLICATE VALUES---------------------------------------#

missing_train <- sum(is.na(train_digit_data))

dup_train_data <- sum(duplicated(train_digit_data))

missing_test <- sum(is.na(test_digit_data))

dup_test_data <- sum(duplicated(test_digit_data))

#------------------------------CONVERT TARGET VARIABLE TO FACTOR ----------------------------------------------#

train_digit_data$digit <- as.factor(train_digit_data$digit)
class(train_digit_data$digit)


test_digit_data$digit <- as.factor(test_digit_data$digit)
class(test_digit_data$digit)


#-------------------------------CHECK VALUES AND DISTRIBUTION OF TARGET VARIABLE------------------------------#

train_data_count <- train_digit_data
train_data_count$source <- "TRAIN DATA"

test_data_count <-test_digit_data
test_data_count$source <- "TEST DATA"

digit_counts <- rbind(train_data_count, test_data_count)

#---------------------------PLOT THE TARGET VALUE DISTRIBUTION IN TEST AND TRAIN DATA-------------------------#


proportion_train <- ggplot(digit_counts, aes(x = digit, group = source, colour = source)) +
  geom_line(stat = "count", size = 2) +  scale_colour_brewer(palette = "Set2") +
  labs(x = "VALUE OF NUMBER", y = "NUMBER COUNT", title = "FREQUENCY DIST OF NUMBERS" )
proportion_train



###############################################################################################################
#                                            MODEL PREP & BUILDING                                            #
###############################################################################################################

#CREATE A SAMPLE DATASET, FOR COMPUTATIONAL PURPOSES AS TRAINING DATASET IS TOO LARGE FOR TIMELY PROCESSING


set.seed(100)

# GENERATE ROW INDICES FOR 15% OF ORIGINAL RECORDS

trainindices= sample(1:nrow(train_digit_data), 0.15*nrow(train_digit_data))
testindices= sample(1:nrow(test_digit_data), 0.15*nrow(test_digit_data))

#GENERATE TRAINING DATA

train_nbr = train_digit_data[trainindices,]

test_nbr = test_digit_data[testindices,]

#CONFIRM THE DISTRIBUTION OF TARGET VALUES IS SIMILAR TO ORIGINAL SOURCE

train <- ggplot(train_nbr) + geom_bar(aes(x=as.factor(digit)), fill = "red", col = "black") +
  scale_y_continuous(breaks = seq(0,9000,500)) +
  labs(title = "Test Distribution")
test <- ggplot(test_nbr) + geom_bar(aes(x=as.factor(digit)), fill = "orange", col = "black") +
  scale_y_continuous(breaks = seq(0,9000,500)) +
  labs(title = "Train Distribution")

grid.arrange(train,test)

#-------------------------------------BUILD SVM LINEAR MODEL--------------------------------------------------#

model_linear <-  ksvm(digit~., data = train_nbr, scaled = F, kernel = "vanilladot")

model_linear

eval_linear<- predict(model_linear, test_nbr)
eval_linear

#confusion matrix - Linear Kernel

confusionMatrix(eval_linear,test_nbr$digit)   #Accuracy = 0.9227


#-------------------------------------BUILD SVM POLYNOMIAL MODEL-----------------------------------------------#

model_polynom <-  ksvm(digit~., data = train_nbr, scaled = F, kernel = "polydot")

model_polynom

eval_polynom<- predict(model_polynom, test_nbr)

#confusion matrix - Polynomial Kernel

confusionMatrix(eval_polynom,test_nbr$digit)  #Accuracy = 0.9227

#----------------------------------------BUILD SVM RBF MODEL--------------------------------------------------#

model_rbf <-  ksvm(digit~., data = train_nbr, scaled = F, kernel = "rbfdot")

model_rbf

eval_rbf<- predict(model_rbf, test_nbr)

#confusion matrix - RBF Kernel

confusionMatrix(eval_rbf,test_nbr$digit)  #Accuracy = 0.9567


#------------------------------------------MODEL OBSERVATION -------------------------------------------------#
#CLEARLY RBF MODEL APPEARS TO HAVE HIGHER ACCURACY THAN THE OTHER MODELS, SUGGESTING THAT A NON-LINEAR FIT IS
#MORE SUITABLE MODEL THAN A LINEAR ONE.

###############################################################################################################
#                                 HYPER-PARAMETER TUNING & CROSS-VALIDATION                                   #
###############################################################################################################

#APPLYING 5 FOLD CROSS-VALIDATION FOR ACCURACY METRIC

trainControl <- trainControl(method="cv", number=5, verboseIter = T)


#TRY USING A RANGE OF SIGMA AND C VALUES CLOSE TO THE DEFAULT, VARYING THE VALUES A BIT TO CHECK FOR BETTER 
#RESULTS

set.seed(13)

grid <- expand.grid(.sigma=c(0.000000163, 0.000002), .C=c(0.01,0.5,1,3,6,7) )
svm_rbf_fit <- train(digit~., data = train_nbr, method = "svmRadial", metric = "Accuracy", 
                     tuneGrid = grid, trControl = trainControl, allowParallel = TRUE)

print(svm_rbf_fit)

plot(svm_rbf_fit)

#BASED ON THE TABLE AND PLOT ABOVE, IT IS OBSERVED THAT HIGHEST ACCURACY IS OBTAINED WHEN SIGMA IS 1.66e^-7
#AND C VALUE OF 6. USE THESE VALUES TO BUILD A NON-LINEAR MODEL AND VALIDATE THE RESULTS

model_best_rbf <-  ksvm(digit~., data = train_nbr, scaled = F, kernel = "rbfdot", C=6, kpar=list(sigma=1.63e-7))

model_best_rbf

#APPLY MODEL ON TEST DATA AND EVALUATE RESULTS

eval_best_rbf<- predict(model_best_rbf, test_nbr)

#confusion matrix - RBF Kernel

confusionMatrix(eval_best_rbf,test_nbr$digit)  #Accuracy = 0.9647

###############################################################################################################
#                                              CONCLUSION                                                     #
###############################################################################################################

#We have built both linear and non-linear models for the given problem and it has been verified that the 
#non-linear RBF model performs best with highest accuracy. Further tweaking the hyper-parameters for this
#model, it has been observed that a sigma value of 1.63e-7 and C value of 6 results in the highest accuracy
#During validation, the sensitivity and specificity values are also high across all classes of the target variable for these
#parameter values. The model is thus a good fit for the data in hand for the given parameter values.



