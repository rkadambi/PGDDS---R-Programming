##############################################################
# SPARK : NYC PARKING TICKETS- GROUP CASE STUDY              #
# SUBMITTED BY : RUPA KADAMBI                                #
#                SAURAV DAS                                  #
#                SOURABH DAS                                 #
##############################################################

##############################################################################################################
#                                             PRELIMINARY STEPS - DATA IMPORTS                               #
##############################################################################################################

# ESSENTIAL COMMANDS  FOR SPARK-R

spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

#LOAD APPROPRIATE R LIBRARIES

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))
library(dplyr)
library(stringr)
library(ggplot2)
library(sparklyr)

sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))

# ADD JAR FILE IN RSTUDIO

sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

###########################################################################################################################
#                                             PRELIMINARY STEPS - DATA IMPORTS                                            #
###########################################################################################################################

data_parking_2015 <- SparkR :: read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv",
                                       source = "csv", header = "true", inferSchema = "true")

data_parking_2016 <- SparkR :: read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv",
                                       source = "csv", header = "true", inferSchema = "true")

data_parking_2017 <- SparkR :: read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv",
                                       source = "csv", header = "true", inferSchema = "true")


###########################################################################################################################
#                                             PRELIMINARY STEPS - DATA CLEANING                                           #
###########################################################################################################################

#OBTAIN RECORD COUNTS FOR ALL FILES

nrow(data_parking_2015)
nrow(data_parking_2016)
nrow(data_parking_2017)

head(data_parking_2015, n= 5)
head(data_parking_2016, n= 5)
head(data_parking_2017, n= 5)

#REVIEW COLUMN NAMES IN EACH OF THE FILES

colnames_2015 <- colnames(data_parking_2015)
colnames_2016 <- colnames(data_parking_2016)
colnames_2017 <- colnames(data_parking_2017)

#IT APPEARS THERE ARE SOME SPACES AND INCONSISTENCIES IN COLUMN NAMES. FIX THE SAME FOR FURTHER ANALYSIS

fixed_colnames_2015 <- gsub(" ", "_",trimws(tolower(colnames(data_parking_2015))))
colnames(data_parking_2015) <- fixed_colnames_2015

fixed_colnames_2016 <- gsub(" ", "_",trimws(tolower(colnames(data_parking_2016))))
colnames(data_parking_2016) <- fixed_colnames_2016

fixed_colnames_2017 <- gsub(" ", "_",trimws(tolower(colnames(data_parking_2017))))
colnames(data_parking_2017) <- fixed_colnames_2017


#COMPARE TO SEE IF THE COLUMNS ARE THE SAME IN ALL 3 FILES

all.equal(fixed_colnames_2015, fixed_colnames_2016)
all.equal(fixed_colnames_2015, fixed_colnames_2017)

#THERE ARE SOME COLUMNS DROPPED IN 2017 FILE. IT APPEARS THEY ARE NOT VITAL FOR ANALYSIS, SO CAN BE DROPPED

dropped_columns <- c(setdiff(fixed_colnames_2015, fixed_colnames_2017))
dropped_columns

data_parking_2015 <- subset(data_parking_2015, select = fixed_colnames_2017)
data_parking_2016 <- subset(data_parking_2016, select = fixed_colnames_2017)


all.equal(colnames(data_parking_2015), colnames(data_parking_2017))

#ADD DATA SOURCE FILE TO EACH FILE FOR DATA ISSUES TRACKING

data_parking_2015$datasrc <- "data_parking_2015"
data_parking_2016$datasrc <- "data_parking_2016"
data_parking_2017$datasrc <- "data_parking_2017"


#--------------------------------COMBINE DATA FILES FOR FURTHER DATA CLEANING AND ANALYSIS------------------#

parking_data_combined <- rbind(data_parking_2015, data_parking_2016, data_parking_2017)
nrow(parking_data_combined)
printSchema(parking_data_combined)
str(parking_data_combined)

#----------------------------CHECK FOR DUPLICATES BASED ON UNIQUE ID AND MISSING VALUES IN KEY FIELDS-------#

#CREATE SQL VIEW FOR EDA

createOrReplaceTempView(parking_data_combined, "nyc_parking_tkts_vw")

#CHECK TO MAKE SURE COMBINED DATA RECORD COUNTS MATCH

display_data_src <- SparkR:: sql ("SELECT datasrc, count(*) from nyc_parking_tkts_vw
                                   group by datasrc")
head(display_data_src)

#CHECK FOR DUPLICATES IN SUMMONS NUMBER AS IT IS THE UNIQUE IT FOR EACH PARKING TICKETS

dupe_records <- SparkR:: sql("SELECT * from nyc_parking_tkts_vw
                              where summons_number  in (select summons_number from nyc_parking_tkts_vw
                              GROUP BY summons_number
                              having count(*) > 1)")
							  
nrow(dupe_records)

#INVESTIGATE FURTHER, THE SOURCE OF DUPLICATE RECORDS

createOrReplaceTempView(dupe_records, "dupes_vw")

display_dupe_src <- SparkR:: sql ("SELECT datasrc, count(*) from dupes_vw
                                   group by datasrc")

head(display_dupe_src)

#DROP DUPLICATE RECORDS FROM COMBINED FILE

parking_tkts_deduped <- dropDuplicates(parking_data_combined, "summons_number")
nrow(parking_tkts_deduped)

#-------------------------------DATES CONVERSION-----------------------------------------------------#

parking_tkts_deduped$issue_date <- SparkR::to_date(parking_tkts_deduped$issue_date, 'MM/dd/yyyy')
parking_tkts_deduped$vehicle_expiration_date <- SparkR::to_date(parking_tkts_deduped$vehicle_expiration_date, 'MM/dd/yyyy')
parking_tkts_deduped$date_first_observed <- SparkR::to_date(parking_tkts_deduped$date_first_observed, 'MM/dd/yyyy')


#---------------------------------DETERMINE THE DATE RANGE OF TICKETS--------------------------------#

createOrReplaceTempView(parking_tkts_deduped, "tkts_deduped_vw")

date_dist_tkts <- SparkR::sql("SELECT count(*),  year(issue_date) as tkt_yr
                                      FROM tkts_deduped_vw
                                      GROUP BY tkt_yr
                                      ORDER BY tkt_yr")

nrow(date_dist_tkts)
head(date_dist_tkts, n = 50)

#IT IS CLEAR THAT THERE ARE RECORDS THAT ARE OUT OF RANGE FOR ANALYSIS. EXCLUDE THE SAME

filtered_data <- subset(parking_tkts_deduped, parking_tkts_deduped$issue_date >= "2014-07-01" & parking_tkts_deduped$issue_date <= "2017-06-30")

createOrReplaceTempView(filtered_data, "temp_vw")

#CREATE A VARIABLE WITH FISCAL YEAR VALUE FOR FURTHER ANALYSIS

parking_tkts_analysis <- SparkR::sql("SELECT *,
                                       CASE WHEN issue_date BETWEEN '2014-07-01' AND '2015-06-30'
                                       THEN '2015'
                                       WHEN issue_date BETWEEN '2015-07-01' AND '2016-06-30'
                                       THEN '2016'
                                       WHEN issue_date BETWEEN '2016-07-01' AND '2017-06-30'
                                       THEN '2017'
                                       END AS fiscal_year
                                       FROM temp_vw")
									   
#---------------------CHECK INTEGRITY OF KEY FIELDS FOR ANALYSIS-----------------------------#

#CHECK MISSING VALUES IN KEY FIELDS

missing_state <- where(parking_tkts_analysis, isNull(parking_tkts_analysis$registration_state))
nrow(missing_state)

	   
####################################################################################################################
#                                             DATA EXAMINATION - QUERIES                                           #
####################################################################################################################									   

# Q1. FIND THE NUMBER OF TICKETS BY YEAR
#-----------------------------------------------------------------------------------------------------------------

createOrReplaceTempView(parking_tkts_analysis, "fiscal_tkts_vw")

summarized_dates <- SparkR::sql("SELECT count(*), fiscal_year
                                 from fiscal_tkts_vw
                                 group by fiscal_year
								 order by fiscal_year")
head(summarized_dates)

# Q2. FIND THE NUMBER OF UNIQUE STATES
#----------------------------------------------------------------------------------------------------------------

# FIRST FIND THE VALUES OF STATES IN THE FILE

state_freq <- SparkR:: sql("SELECT count(*)as tkts, registration_state
                            from analytics_vw
                            group by registration_state
                            order by tkts desc ")
  
nrow(state_freq)
head(state_freq, n=12)

#FIX THE STATE VALUE OF 99 WITH TOP STATE VALUE "NY" AND THEN RERUN THE SUMMARY BY YEAR

parking_tkts_analysis$registration_state<- regexp_replace(parking_tkts_analysis$registration_state,"99", "NY")

createOrReplaceTempView(parking_tkts_analysis, "analytics_vw")

state_freq_by_year <- SparkR:: sql("SELECT count(distinct registration_state)as nbr_states, fiscal_year
                                    from analytics_vw
                                    group by fiscal_year
                                    order by fiscal_year ")

nrow(state_freq_by_year)
head(state_freq_by_year)

# Q3. FIND NUMBER OF TICKETS EACH YEAR WITH MISSING ADDRESS
#----------------------------------------------------------------------------------------------------------------
missing_address <- SparkR::sql("SELECT count(*) as nbr_records, 
                                    SUM(CASE WHEN house_number IS NULL or street_name IS NULL 
                                    THEN 1 
                                    ELSE 0 END)as nbr_tickets, 
                                    100*SUM(CASE WHEN house_Number IS NULL or street_Name IS NULL
                                    THEN 1 
                                    ELSE 0 
                                    END)/count(*) as pct_tickets, fiscal_year
                                    from analytics_vw
                                    group by fiscal_year
                                    order by fiscal_year")
head(missing_address)

####################################################################################################################
#                                             AGGREGATION - QUERIES                                                #
####################################################################################################################

#-------------------------------------------------------------------------------------
# Q1. FIND TOP 5 VIOLATION CODES                                                     #
#-------------------------------------------------------------------------------------

violation_codes <- SparkR::sql ("select count(*) as nbr_tkts, violation_code, fiscal_year
                                      from analytics_vw
                                      group by fiscal_year, violation_code
                                      order by fiscal_year, nbr_tkts desc")
nrow(violation_codes)  
head(violation_codes, n=300)

createOrReplaceTempView(violation_codes, "violations_vw")

top5_violations <- SparkR::sql ("select * from (
                                            select *, 
                                            row_number() over(partition by fiscal_year order by nbr_tkts desc) as rn
                                            from violations_vw as t) as t 
                                            where t.rn <= 5")
                                      
head(top5_violations, n=15)    

#-------------------------------------------------------------------------------------
# Q2. FIND TOP 5 VEHICLE BODY TYPE AND MAKE                                          #
#-------------------------------------------------------------------------------------

#FIRST FIND TOP 5 BODY TYPE

vehicle_body <- SparkR::sql ("select count(*) as nbr_tkts, vehicle_body_type, fiscal_year
                                      from analytics_vw
                                      group by fiscal_year, vehicle_body_type
                                      order by fiscal_year, nbr_tkts desc")
nrow(vehicle_body)  

createOrReplaceTempView(vehicle_body, "body_type_vw")

top5_body_type <- SparkR::sql ("select * from (
                                            select *, 
                                            row_number() over(partition by fiscal_year order by nbr_tkts desc) as rn
                                            from body_type_vw as t) as t 
                                            where t.rn <= 5")

nrow(top5_body_type)
head(top5_body_type, n=15)

# FIND TOP 5 VEHICLE MAKE

vehicle_make <- SparkR::sql ("select count(*) as nbr_tkts, vehicle_make, fiscal_year
                                      from analytics_vw
                                      group by fiscal_year, vehicle_make
                                      order by fiscal_year, nbr_tkts desc")
nrow(vehicle_make)  

createOrReplaceTempView(vehicle_make, "veh_make_vw")

top5_make <- SparkR::sql ("select * from (
                                            select *, 
                                            row_number() over(partition by fiscal_year order by fiscal_year, nbr_tkts desc) as rn
                                            from veh_make_vw as t) as t 
                                            where t.rn <= 5")

nrow(top5_make)
head(top5_make, n=15)

#-------------------------------------------------------------------------------------
# Q3. FIND PRECINCT SUMMARY                                                          #
#-------------------------------------------------------------------------------------
# FIND TOP VIOLATION PRECINCT
viol_precinct <- SparkR::sql ("select count(*) as nbr_tkts, violation_precinct, fiscal_year
                                      from analytics_vw
                                      group by fiscal_year, violation_precinct
                                      order by fiscal_year, nbr_tkts desc")
nrow(viol_precinct)  

createOrReplaceTempView(viol_precinct, "vl_prec_vw")

top5_viol_precinct <- SparkR::sql ("select * from (
                                            select *, 
                                            row_number() over(partition by fiscal_year order by fiscal_year, nbr_tkts desc) as rn
                                            from vl_prec_vw as t) as t 
                                            where t.rn <= 5")


head(top5_viol_precinct, n=15)  

# FIND TOP ISSUER PRECINCT

iss_precinct <- SparkR::sql ("select count(*) as nbr_tkts, issuer_precinct, fiscal_year
                                      from analytics_vw
                                      group by fiscal_year, issuer_precinct
                                      order by fiscal_year, nbr_tkts desc")
nrow(iss_precinct)  

createOrReplaceTempView(iss_precinct, "iss_prec_vw")

top5_iss_precinct <- SparkR::sql ("select * from (
                                            select *, 
                                            row_number() over(partition by fiscal_year order by fiscal_year, nbr_tkts desc) as rn
                                            from iss_prec_vw as t) as t 
                                            where t.rn <= 5")


head(top5_iss_precinct, n=15) 

#-------------------------------------------------------------------------------------
# Q4. FIND VIOLATION CODES FOR TOP 3 PRECINCTS                                       #
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Q5. FIND VIOLATIONS BASED ON TIME OF DAY                                           #
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# Q6. FIND TOP VIOLATIONS BY SEASONS                                                 #
#-------------------------------------------------------------------------------------
# FIRST FIND NBR OF TICKETS BY SEASONS EACH YEAR

parking_tkts_seasons <- SparkR::sql("SELECT *,
                                     CASE WHEN month(issue_date) in (3,4,5) THEN 'SPRING'
                                     WHEN month(issue_date) in (6,7,8) THEN 'SUMMER'
                                     WHEN month(issue_date) in (9,10,11) THEN 'AUTUMN'
                                     WHEN month(issue_date) in (12,1,2) THEN 'WINTER'
                                     END AS season
                                     FROM analytics_vw")

createOrReplaceTempView(parking_tkts_seasons, "seasons_vw")

tickets_by_seasons <- SparkR:: sql("select count(*) as nbr_tkts, fiscal_year, season
                                    from seasons_vw
                                    group by fiscal_year, season
                                    order by fiscal_year")

head(tickets_by_seasons, n = 12) 

#NEXT FIND TOP 3 VIOLATION CODES FOR EACH SEASON IN A YEAR

seasonal_violations <- SparkR::sql ("select count(*) as nbr_tkts, violation_code, fiscal_year, season
                                      from seasons_vw
                                      group by fiscal_year, season, violation_code
                                      order by fiscal_year, season, nbr_tkts desc")
nrow(seasonal_violations)

createOrReplaceTempView(seasonal_violations, "seasons_viol_vw")

top3_seasonal_violations <- SparkR::sql ("select * from (
                                            select *, 
                                            row_number() over(partition by fiscal_year, season order by nbr_tkts desc) as rn
                                            from seasons_viol_vw as t) as t 
                                            where t.rn <= 3")

nrow(top3_seasonal_violations)
head(top3_seasonal_violations,n=36)
									
#-------------------------------------------------------------------------------------
# Q7. DETERMINE FINES COLLECTED FOR TOP 3 VIOLATIONS                                 #
#-------------------------------------------------------------------------------------







