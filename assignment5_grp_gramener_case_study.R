#####################################
#GRAMENER  CASE GROUP PROJECT       #
# SUBMITTED BY : ABHISHEK KUMAR     #
#                RUPA KADAMBI       #
#                SOURABH DAS        #
#                SAURAV DAS         #
#####################################

###########################################################################################################################
#                                             PRELIMINARY STEPS - DATA IMPORTS                                            #
###########################################################################################################################

# SET LIBRARY FOR PROJECT #

setwd("~/PGDDS-IIITB/Statistics and Exploratory Data Analytics/Gramener Case Study")

library(lubridate)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(lattice)

install.packages("janitor")
library(janitor)

install.packages("gsubfn")
library(gsubfn)

install.packages("outliers")
library(outliers)

install.packages("ggrepel")
library(ggrepel)

install.packages("corrplot")
library(corrplot)

options(scipen = 999)

#IMPORT DATA INTO R #

loan_data <-  read.csv(file="loan.csv", head = TRUE, sep = ",")

###########################################################################################################################
#                                             DATA CLEANING                                                               #
###########################################################################################################################

#########  REMOVE COLUMNS THAT HAVE MISSING VALUES ON ALL RECORDS  ########

loan_filtered <- loan_data %>% remove_empty("cols")

# CHECK FOR DUPLICATE VALUES IN DATA SET
sum(duplicated(loan_filtered$id))

# CHANGE THE CASE OF LOAN STATUS AS IT WILL BE USED HEAVILY FOR ANALYSIS AND SEGMENTATION

loan_filtered$loan_status <- toupper(loan_filtered$loan_status)

#######  CONVERT THE VARIABLES BELOW TO NUMERIC AND STRIP THE TEXT VALUES IN THE COLUMNS  #######

loan_filtered$term <- as.numeric(gsub("months", "", loan_filtered$term))
loan_filtered$int_rate <- as.numeric(gsub("%", "", loan_filtered$int_rate))
loan_filtered$revol_util <- as.numeric(gsub("%", "", loan_filtered$revol_util))

######  STRIP TEXT FROM EMPLOYMENT LENGTH  #####

loan_filtered$emp_length <- as.character(loan_filtered$emp_length)
to_replace <- list("years" = "", "year" = "", "n/a" = "0" )
loan_filtered$emp_length <- gsubfn (paste(names(to_replace), collapse="|"), to_replace,loan_filtered$emp_length)
loan_filtered$emp_length <- gsub("10\\+", "GT 10 ", loan_filtered$emp_length)

####### FIX DATES ON THE FIELDS CONTAINING DATES AS THEY ARE IN MMM-YY FORMAT ########

loan_filtered$issue_d <- as.Date(paste("01-", loan_filtered$issue_d, sep = ""), format = "%d-%b-%y")
loan_filtered$last_pymnt_d <- as.Date(paste("01-", loan_filtered$last_pymnt_d, sep = ""), format = "%d-%b-%y")
loan_filtered$last_credit_pull_d <- as.Date(paste("01-", loan_filtered$last_credit_pull_d, sep = ""), format = "%d-%b-%y")

#########  REMOVE VARIABLES WITH THE SAME VALUE FOR ALL RECORDS #########

loan_all <- Filter(function(x)(length(unique(x)) >1) , loan_filtered )


# Keep only completed loans for analysis as it is not possible to determine if current loan will be defaulted

closed_loans <- subset(loan_all, (!loan_status %in% c("CURRENT")))
View(closed_loans)

#########  CREATE BUCKETS FOR FURTHER ANALYSIS    #################

attach(closed_loans)

closed_loans$loan_range[loan_amnt>= 0      & loan_amnt  < 5000   ]  <- "0-5K"
closed_loans$loan_range[loan_amnt>= 5000   & loan_amnt  < 10000  ]  <- "05K-10K"
closed_loans$loan_range[loan_amnt>= 10000  & loan_amnt  < 15000  ]  <- "10K-15K"
closed_loans$loan_range[loan_amnt>= 15000  & loan_amnt  < 20000  ]  <- "15K-20K"
closed_loans$loan_range[loan_amnt>= 20000  & loan_amnt  < 25000  ]  <- "20K-25K"
closed_loans$loan_range[loan_amnt>= 25000  & loan_amnt  < 30000  ]  <- "25K-30K" 
closed_loans$loan_range[loan_amnt>= 30000  & loan_amnt  <= 35000 ]  <- "30K-35K" 
closed_loans$loan_range[loan_amnt>35000 ]  <- "GT 35K" 

closed_loans$inc_range[annual_inc>= 0       & annual_inc  < 10000 ]  <- "0-10K"
closed_loans$inc_range[annual_inc>= 10000   & annual_inc  < 20000 ]  <- "10K-20K"
closed_loans$inc_range[annual_inc>= 20000   & annual_inc  < 30000 ]  <- "20K-30K"
closed_loans$inc_range[annual_inc>= 30000   & annual_inc  < 40000 ]  <- "30K-40K"
closed_loans$inc_range[annual_inc>= 40000   & annual_inc  < 50000 ]  <- "40K-50K"
closed_loans$inc_range[annual_inc>= 50000   & annual_inc  < 60000 ]  <- "50K-60K" 
closed_loans$inc_range[annual_inc>= 60000   & annual_inc  < 70000 ]  <- "60K-70K" 
closed_loans$inc_range[annual_inc>= 70000   & annual_inc  < 80000 ]  <- "70K-80K" 
closed_loans$inc_range[annual_inc>= 80000   & annual_inc  < 90000 ]  <- "80K-90K" 
closed_loans$inc_range[annual_inc>= 90000   & annual_inc  <= 100000] <- "90K-100K" 
closed_loans$inc_range[annual_inc>100000 ]  <- "GT 100K" 

detach(closed_loans)

#############################################################################################################
#                                     OUTLIER ANALYSIS AND CLEANUP                                          #
#############################################################################################################

# Review the loan amount, annual income and other key numeric fields to determine outliers and if they 
# should be excluded

boxplot(closed_loans$annual_inc)
summary(closed_loans$annual_inc)
quantile(closed_loans$annual_inc,0.997)

# There are a few records with very high value of annual income that needs to be excluded as its an outlier

boxplot(closed_loans$funded_amnt)
summary(closed_loans$funded_amnt)
quantile(closed_loans$funded_amnt, 0.98)

# One percent of records have funded amount more than 30000

boxplot(closed_loans$dti)
summary(closed_loans$dti)
quantile(closed_loans$dti, 0.99)

defaulted_loans <- subset(closed_loans, loan_status == "CHARGED OFF") 
View(defaulted_loans)

###########################################################################################################################
#                         UNIVARIATE ANALYSIS & SEGMENTED UNIVARIATE ANALYSIS                                             #
###########################################################################################################################

theme_update(axis.text.x = element_text(angle = 0, hjust = 0))

#### REVIEW LOAN DATE TO ASSESS NUMBER OF DEFAULTED LOANS OVER THE YEARS ####

p_ldates <- ggplot(closed_loans, aes(x = issue_d, fill = loan_status)) + geom_bar(stat = "count") +
  labs(x = "LOAN DATE", y = "NBR OF LOANS", title = "DEFAULTED LOAN DATES SPREAD" ) +
  scale_fill_brewer(palette = "Dark2")

p_ldates

proportion_ldates <- ggplot(closed_loans, aes(x = issue_d, group = loan_status, colour = loan_status)) +
                     geom_line(stat = "count")

proportion_ldates

#### REVIEW LOAN VOLUME ####

p_lrange <- ggplot(defaulted_loans, aes(x = loan_range)) + geom_bar(stat = "count", fill = '#FF6666') +
  labs(x = "LOAN AMOUNT", y = "NBR OF LOANS", title = "NUMBER OF DEFAULTS VS LOAN AMOUNT", fill = I("green") ) +
  geom_text(stat="count", aes(label = ..count..), position = position_stack(vjust = 0.5))
  
p_lrange

proportion_lrange <- ggplot(closed_loans, aes(x = loan_range, group = loan_status, colour = loan_status)) +
  geom_line(stat = "count") +  scale_colour_brewer(palette = "Set1")

proportion_lrange


#### REVIEW STATEWISE LOAN APPLICATIONS ####

p_lstate <- ggplot(defaulted_loans, aes(x = addr_state, fill = loan_status)) + geom_bar(stat = "count") +
  labs(x = "LOAN DATE", y = "NBR OF LOANS", title = "DEFAULTED LOANS STATES" ) +
  scale_fill_brewer(palette = "Dark2")

p_lstate

state_counts<- closed_loans %>% group_by(addr_state, loan_status) %>% 
               summarise(total_state_cnt = n())

  ggplot(state_counts, aes(x = addr_state, y = total_state_cnt)) + 
  geom_point(aes(size=total_state_cnt, colour = loan_status)) +
  scale_size(range = c(1,20)) +
  geom_text(aes(label=ifelse(total_state_cnt > 400, as.character(addr_state), ''))) +
  scale_colour_brewer(palette = "Set2")  +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(color = "black"),
        axis.text.x = element_blank()) +
        labs(x = "STATES", y = "NBR OF LOANS", title = "LOANS BY STATE" )

#### REVIEW LOAN GRADE ####

  p_lgrade <- ggplot(defaulted_loans, aes(x = grade)) + geom_bar(stat = "count", fill = '#2E9AFE') +
    labs(x = "LOAN GRADE", y = "NBR OF LOANS", title = "LOAN GRADES FOR DEFAULTED LOANS" ) +
    geom_text(stat="count", aes(label = ..count..), position = position_stack(vjust = 0.5))
  
  p_lgrade
  
  proportion_lgrade <- ggplot(closed_loans, aes(x = grade, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") +  scale_colour_brewer(palette = "Set1")
  
  proportion_lgrade
  
#### REVIEW EMPLOYMENT LENGTH ####

  p_empl <- ggplot(defaulted_loans, aes(x = emp_length)) + geom_bar(stat = "count", fill = "blue4") +
    labs(x = "EMPLOYMENT LENGTH", y = "NBR OF LOANS", title = "LOANS BY EMPLOYMENT DURATION" ) +
    scale_fill_brewer(palette = "Set1")
  
  p_empl
  
  proportion_lempl <- ggplot(closed_loans, aes(x = emp_length, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") +  scale_colour_brewer(palette = "Set2")
  
  proportion_lempl
  
#### REVIEW VERIFICATION STATUS ####
  
  p_ver <- ggplot(defaulted_loans, aes(x = verification_status)) + geom_bar(stat = "count", fill = 'brown3') +
    labs(x = "VERIFICATION STATUS", y = "NBR OF LOANS", title = "LOANS BY VERIFICATION STATUS" ) +
    scale_fill_brewer(palette = "Set1")
  
  p_ver
  
  proportion_lver <- ggplot(closed_loans, aes(x = verification_status, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") +  scale_colour_brewer(palette = "Dark2")
  
  proportion_lver
  
#### REVIEW INCOME RANGE ####

  p_incr <- ggplot(defaulted_loans, aes(x = inc_range)) + geom_bar(stat = "count", fill = 'chartreuse4') +
    labs(x = "INCOME RANGE", y = "NBR OF LOANS", title = "LOANS BY INCOME RANGE" ) +
    scale_fill_brewer(palette = "Set1")
  
  p_incr
  
  proportion_lincr <- ggplot(closed_loans, aes(x = inc_range, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") +  scale_colour_brewer(palette = "Set1")
  
  proportion_lincr
  
#### REVIEW PURPOSE ####

  p_purp <- ggplot(defaulted_loans, aes(x = purpose)) + geom_bar(stat = "count", fill = 'coral') +
    labs(x = "PURPOSE OF LOANS", y = "NBR OF LOANS", title = "LOANS BY PURPOSE" ) +
    theme_update(axis.text.x = element_text(angle = 90, hjust = 0))
  
  p_purp
  
  proportion_lpurp <- ggplot(closed_loans, aes(x = purpose, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") 
  
  proportion_lpurp
  
#### REVIEW HOME OWNERSHIP ####
  
  p_hown <- ggplot(defaulted_loans, aes(x = home_ownership)) + geom_bar(stat = "count", fill = 'coral') +
    labs(x = "HOME OWNERSHIP", y = "NBR OF LOANS", title = "LOANS BY HOME OWNERSHIP" ) +
    theme_update(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  p_hown
  
  proportion_lhown <- ggplot(closed_loans, aes(x = home_ownership, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") 
  
  proportion_lhown
  
#### REVIEW DELINQUENCIES IN BORROWERS CREDIT FILE ####
  
  p_delinq <- ggplot(defaulted_loans, aes(x = delinq_2yrs)) + geom_bar(stat = "count", fill = 'coral') +
    labs(x = "NO. OF DELINQUENCIES", y = "NBR OF LOANS", title = "LOANS BY NO. OF DELINQUENCIES IN PAST 2 YEARS" ) +
    theme_update(axis.text.x = element_text(angle = 90, hjust = 0))
  
  p_delinq
  
  proportion_ldelinq <- ggplot(closed_loans, aes(x = delinq_2yrs, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") 
  
  proportion_ldelinq
  
#### REVIEW NO. OF INQUIRIES ####
  
  p_inq <- ggplot(defaulted_loans, aes(x = inq_last_6mths)) + geom_bar(stat = "count", fill = 'coral') +
    labs(x = "NO. OF INQUIRIES", y = "NBR OF LOANS", title = "LOANS BY NO. OF INQUIRIES IN PAST 6 MONTHS" ) +
    theme_update(axis.text.x = element_text(angle = 90, hjust = 0))
  
  p_inq
  
  proportion_linq <- ggplot(closed_loans, aes(x = inq_last_6mths, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") 
  
  proportion_linq
  
#### REVIEW DEROGATORY PUBLIC RECORDS ####
  
  p_pubrec <- ggplot(defaulted_loans, aes(x = pub_rec)) + geom_bar(stat = "count", fill = 'coral') +
    labs(x = "DEROGATORY RECORDS", y = "NBR OF LOANS", title = "LOANS BY NO. OF DEROGATORY PUBLIC RECORDS" ) +
    theme_update(axis.text.x = element_text(angle = 90, hjust = 0))
  
  p_pubrec
  
  proportion_lpubrec <- ggplot(closed_loans, aes(x = pub_rec, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") 
  
  proportion_lpubrec
  
#### REVIEW PUBLIC RECORD BANKRUPTCIES ####
  
  p_bankrup <- ggplot(defaulted_loans, aes(x = pub_rec_bankruptcies)) + geom_bar(stat = "count", fill = 'coral') +
    labs(x = "PUBLIC RECORD BANKRUPTCIES", y = "NBR OF LOANS", title = "LOANS BY NO. OF PUBLIC RECORD BANKRUPTCIES") +
    theme_update(axis.text.x = element_text(angle = 90, hjust = 0))
  
  p_bankrup
  
  proportion_lbankrup <- ggplot(closed_loans, aes(x = pub_rec_bankruptcies, group = loan_status, colour = loan_status)) +
    geom_line(stat = "count") 
  
  proportion_lbankrup
  
###########################################################################################################################
#                         BIVARIATE ANALYSIS USING CORRELATION MATRIX                                             #
###########################################################################################################################
  
#### PREPARE DATA FROM FILTERED DATASET OF DEFAULT LOANS ####
  
  biv_data <- defaulted_loans[, c("dti","loan_amnt","annual_inc","int_rate","total_acc","revol_bal","revol_util","inq_last_6mths","pub_rec","pub_rec_bankruptcies","open_acc","delinq_2yrs")]

#### COMPUTE THE CORRELATION MATRIX AND REMOVE REDUNDANT INFORMATION OF DEFAULTED LOANS ####

  corr_matrix <- round(cor(biv_data, use = "complete.obs"),2)
  
  # Reorder the correlation matrix
  reorder_corr_matrix <- function(corr_matrix){
    corr_dist <- as.dist((1-corr_matrix)/2)
    hc <- hclust(corr_dist)
    corr_matrix <- corr_matrix[hc$order,hc$order]
  }
  corr_matrix <- reorder_corr_matrix(corr_matrix)
  
  # Get upper diagonal half of matrix for removing redundant information for defaulted loans
  get_upper_diag_half <- function(corr_matrix){
    corr_matrix[lower.tri(corr_matrix)] <- NA
    return(corr_matrix)
  }
  upper_diag_half <- get_upper_diag_half(corr_matrix)
  upper_diag_half
  
#### CREATE THE CORRELATION HEATMAP FOR DEFAULTED LOANS ####

  melted_corr_matrix <- melt(upper_diag_half, na.rm = TRUE)
  loan_def_heatmap <- ggplot(data = melted_corr_matrix, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "yellow", high = "red", mid = "blue",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name = "Pearson\nCorrelation")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
    coord_fixed()

  # Add correlation coefficients on the heatmap
  loan_def_heatmap +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(1,0),
          legend.position = c(0.6,0.7),
          legend.direction = "horizontal")+
          guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                       title.position = "top", title.hjust = 0.5)
          )

  
#### PREPARE DATA FROM FILTERED DATASET OF FULLY PAID LOANS ####
  
  fully_paid_loans <- subset(closed_loans, loan_status == "FULLY PAID")
  biv_data2 <- fully_paid_loans[, c("dti","loan_amnt","annual_inc","int_rate","total_acc","revol_bal","revol_util","inq_last_6mths","pub_rec","pub_rec_bankruptcies","open_acc","delinq_2yrs")]
  
#### COMPUTE THE CORRELATION MATRIX AND REMOVE REDUNDANT INFORMATION FOR FULLY PAID LOANS ####
  
  corr_matrix2 <- round(cor(biv_data2, use = "complete.obs"),2)
  
  # Reorder the correlation matrix
  reorder_corr_matrix2 <- function(corr_matrix2){
    corr_dist2 <- as.dist((1-corr_matrix2)/2)
    hc2 <- hclust(corr_dist2)
    corr_matrix2 <- corr_matrix2[hc2$order,hc2$order]
  }
  corr_matrix2 <- reorder_corr_matrix2(corr_matrix2)
  
  # Get upper diagonal half of matrix for removing redundant information
  get_upper_diag_half2 <- function(corr_matrix2){
    corr_matrix2[lower.tri(corr_matrix2)] <- NA
    return(corr_matrix2)
  }
  upper_diag_half2 <- get_upper_diag_half2(corr_matrix2)
  upper_diag_half2
  
  #### CREATE THE CORRELATION HEATMAP FOR FULLY PAID LOANS ####
  
  melted_corr_matrix2 <- melt(upper_diag_half2, na.rm = TRUE)
  loan_def_heatmap2 <- ggplot(data = melted_corr_matrix2, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "yellow", high = "red", mid = "blue",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name = "Pearson\nCorrelation")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
    coord_fixed()
  
  # Add correlation coefficients on the heatmap
  loan_def_heatmap2 +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(1,0),
          legend.position = c(0.6,0.7),
          legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5)
    )
  

###################################################################################################
