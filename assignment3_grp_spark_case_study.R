#########################################
###INVESTMENT CASE STUDY GROUP PROJECT###
###SUBMITTED BY : ABHISHEK KUMAR        #
#                 RUPA KADAMBI          #
#                 SAURAV DAS            #
#                 SOURABH DAS           #
#########################################

###########################################################################################################################
#                                             PRELIMINARY STEPS - DATA IMPORTS                                            #
###########################################################################################################################

#IMPORT DATA INTO R #

#	Import the Companies data set in Rstudio
companies <- read.delim("companies.txt", header=TRUE, stringsAsFactors = FALSE, sep="\t")

#	Import the Funding Round Details data set in Rstudio
rounds2 <- read.csv("rounds2.csv", header=TRUE, stringsAsFactors = FALSE)

mapping <- read.csv(file= "mapping.csv", head = TRUE, sep = ",", check.names = FALSE, stringsAsFactors = FALSE)


#Import the English countries list text file, created using the PDF provided and country code obtained from external source

english_countries <- read.delim(file="english_countries_list.txt", head = TRUE, stringsAsFactors = FALSE, sep="\t")


###########################################################################################################################
#                                             PRELIMINARY STEPS - DATA CLEANING                                           #
###########################################################################################################################

# Clean up the companies and rounds2 data frames to have the same case values in the permalink column, for accurate analysis

companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# Clean up the mapping dataset to replace 0 with na in category list column

mapping$category_list <- gsub("0", "na", mapping$category_list)

#Change case on the category list in mapping and companies data

mapping$category_list <- toupper(mapping$category_list)
companies$category_list <- toupper(companies$category_list)


###########################################################################################################################
#                                             CHECKPOINT 1  - MERGE DATASETS                                              #
###########################################################################################################################

# TABLE 1.1 UNDERSTAND THE DATA SET #

# Count of unique companies in Rounds2

uniq_comp_rounds2 <- length(unique(rounds2$company_permalink))
uniq_comp_rounds2

# Count of unique companies in Companies

uniq_comp_companies <- length(unique(companies$permalink))
uniq_comp_companies

#Determine if there are companies in Rounds2 and not in companies

rounds2_excl <- length(setdiff(rounds2$company_permalink, companies$permalink))
rounds2_excl

#Merge the two datasets and determine record count 

master_frame <- merge(companies, rounds2, by.x = "permalink", by.y ="company_permalink" )
View(master_frame)

###########################################################################################################################
#                                           CHECKPOINT 2 - FUND TYPE ANALYSIS                                             #
###########################################################################################################################

# TABLE 2.1 - Aggregate functions

#Find average funding amount by venture type

rollup <- aggregate(master_frame$raised_amount_usd, by = list(master_frame$funding_round_type), FUN = mean, na.rm = TRUE)

#Find the best funding type based on the 4 venture types in Excel and the investment amount range

optimal_funding_type <- subset(rollup, Group.1 %in% c("venture", "angel", "seed", "private_equity")
                               & x >= 5000000 & x <= 15000000)
optimal_funding_type

###########################################################################################################################
#                                           CHECKPOINT 3 - COUNTRY ANALYSIS                                               #
###########################################################################################################################

# TABLE 3.1 - Country Analysis - For Venture funding

#Merge the english countries data with master frame and filter only venture type funding

library(dplyr)

merged_countries <- merge(master_frame, english_countries, by.x = "country_code", by.y ="COUNTRY.CODE", all.x = TRUE)


filtered_master <- merged_countries %>%
  filter(funding_round_type == "venture"  & (!is.na(country_code))
         & (!category_list %in% ""))


#Determine the total investments for funding_round_typeVenture funding by country and retain top 9

top9 <-   filtered_master %>% 
  filter(!is.na(raised_amount_usd) & funding_round_type == "venture") %>%
  group_by(country_code) %>%
  summarise(total_raised = sum(raised_amount_usd)) %>%
  arrange(desc(total_raised)) %>%
  top_n (9, total_raised) 

#Determine the top3 English speaking countries

top3_eng <-   filtered_master %>% 
  filter(!is.na(raised_amount_usd) & funding_round_type == "venture" & OFFICIAL_ENGLISH == "Y") %>%
  group_by(country_code) %>%
  summarise(total_raised = sum(raised_amount_usd)) %>%
  arrange(desc(total_raised)) %>%
  top_n (3, total_raised)

 
###########################################################################################################################
#                                               CHECKPOINT 4 - SECTOR ANALYSIS                                            #
###########################################################################################################################

#Identify the primary sector in the merged investment file

filtered_master$primary_sector <- vapply(strsplit(filtered_master$category_list, "\\|.*"),"[","",1)

#Convert the mapping data frame into long format

library(tidyr)
mapping_long <- mapping %>%
  group_by (category_list) %>%
  gather(main_sector, value, "Automotive & Sports":"Social, Finance, Analytics, Advertising") %>%
  filter(value==1) %>%
  select(category_list, main_sector)

#Merge mapping with master_frame, to get the Main sector values for all categories

filtered_master_with_mapping <- merge(filtered_master, mapping_long, by.x = "primary_sector", by.y = "category_list", all.x = TRUE )


###########################################################################################################################
#                                         CHECKPOINT 5 - SECTOR ANALYSIS - PART 2                                         #
###########################################################################################################################


# Calculate the Aggregate values by main sector and merge with data frames by country for top 3 values 

create_cntry_df <- function(cntry)
{
  filtered_master_with_mapping %>%
    filter(country_code == cntry & (!is.na(main_sector))  & raised_amount_usd %in% 5000000:15000000) %>%
    group_by (main_sector) %>%
    mutate(total_investments = length(funding_round_permalink), total_amt_invested = sum(raised_amount_usd)) %>%
    arrange(desc(total_investments))
}

D1 <- create_cntry_df("USA")
D2 <- create_cntry_df("GBR")
D3 <- create_cntry_df("IND")

##############################################################################################################################

# Calculate total number (or count) of investments for each main sector for country USA

D1_sector_groups <- group_by(D1, main_sector)
D1_investment_count_sectorwise <- summarise(D1_sector_groups, number = n())

# Calculate total amount invested in each main sector for country USA

D1_investment_amount_sectorwise <- summarise(D1_sector_groups, sum(raised_amount_usd, na.rm = T))
names(D1_investment_amount_sectorwise)[2] <- "investment_usd"
D1_tot_investment <- sum(D1_investment_amount_sectorwise$investment_usd) # Total investment in USA for all sectors using venture funding

# Identify the top 3 sectors in USA in terms of number of investments

names(D1_investment_count_sectorwise)[2] <- "total_no_of_investments"
D1_investment_count_sectorwise_arranged <- arrange(D1_investment_count_sectorwise, desc(total_no_of_investments))
D1_top3_sectors <- head(D1_investment_count_sectorwise_arranged, n=3)

# Identify the company receiving the highest investment of venture funding in top sector in USA

D1_top_sector <- filter(D1, main_sector == D1_top3_sectors$main_sector[1]) # Filter D1 to contain data for only the top sector
D1_investment_companywise_top <- summarise(group_by(D1_top_sector, name), sum(raised_amount_usd, na.rm = T)) # Create summary of investments for each company 
names(D1_investment_companywise_top)[2] <- "total_investment_amount"
D1_investment_companywise_top_arranged <- arrange(D1_investment_companywise_top, desc(total_investment_amount)) # Arrange the investments in descending order
D1_highest_invested_company_top <- head(D1_investment_companywise_top_arranged, n=1) # Select the first row to get the company with highest investment

# Identify the company receiving the highest investment of venture funding in the second best sector in USA

D1_second_best_sector <- filter(D1, main_sector == D1_top3_sectors$main_sector[2])
D1_investment_companywise_second <- summarise(group_by(D1_second_best_sector, name), sum(raised_amount_usd, na.rm = T)) # Create summary of investments for each company 
names(D1_investment_companywise_second)[2] <- "total_investment_amount"
D1_investment_companywise_second_arranged <- arrange(D1_investment_companywise_second, desc(total_investment_amount)) # Arrange the investments in descending order
D1_highest_invested_company_second <- head(D1_investment_companywise_second_arranged, n=1) # Select the first row to get the company with highest investment


##############################################################################################################################

# Calculate total number (or count) of investments for each main sector for country GBR

D2_sector_groups <- group_by(D2, main_sector)
D2_investment_count_sectorwise <- summarise(D2_sector_groups, number = n())

# Calculate total amount invested in each main sector for country GBR

D2_investment_amount_sectorwise <- summarise(D2_sector_groups, sum(raised_amount_usd, na.rm = T))
names(D2_investment_amount_sectorwise)[2] <- "investment_usd"
D2_tot_investment <- sum(D2_investment_amount_sectorwise$investment_usd) # Total investment in GBR for all sectors using venture funding

# Identify the top 3 sectors in GBR in terms of number of investments

names(D2_investment_count_sectorwise)[2] <- "total_no_of_investments"
D2_investment_count_sectorwise_arranged <- arrange(D2_investment_count_sectorwise, desc(total_no_of_investments))
D2_top3_sectors <- head(D2_investment_count_sectorwise_arranged, n=3)

# Identify the company receiving the highest investment of venture funding in top sector in GBR

D2_top_sector <- filter(D2, main_sector == D2_top3_sectors$main_sector[1]) # Filter D2 to contain data for only the top sector
D2_investment_companywise_top <- summarise(group_by(D2_top_sector, name), sum(raised_amount_usd, na.rm = T)) # Create summary of investments for each company 
names(D2_investment_companywise_top)[2] <- "total_investment_amount"
D2_investment_companywise_top_arranged <- arrange(D2_investment_companywise_top, desc(total_investment_amount)) # Arrange the investments in descending order
D2_highest_invested_company_top <- head(D2_investment_companywise_top_arranged, n=1) # Select the first row to get the company with highest investment

# Identify the company receiving the highest investment of venture funding in the second best sector in GBR

D2_second_best_sector <- filter(D2, main_sector == D2_top3_sectors$main_sector[2])
D2_investment_companywise_second <- summarise(group_by(D2_second_best_sector, name), sum(raised_amount_usd, na.rm = T)) # Create summary of investments for each company 
names(D2_investment_companywise_second)[2] <- "total_investment_amount"
D2_investment_companywise_second_arranged <- arrange(D2_investment_companywise_second, desc(total_investment_amount)) # Arrange the investments in descending order
D2_highest_invested_company_second <- head(D2_investment_companywise_second_arranged, n=1) # Select the first row to get the company with highest investment


##############################################################################################################################

# Calculate total number (or count) of investments for each main sector for country IND

D3_sector_groups <- group_by(D3, main_sector)
D3_investment_count_sectorwise <- summarise(D3_sector_groups, number = n())

# Calculate total amount invested in each main sector for country IND

D3_investment_amount_sectorwise <- summarise(D3_sector_groups, sum(raised_amount_usd, na.rm = T))
names(D3_investment_amount_sectorwise)[2] <- "investment_usd"
D3_tot_investment <- sum(D3_investment_amount_sectorwise$investment_usd) # Total investment in India for all sectors using venture funding

# Identify the top 3 sectors in IND in terms of number of investments

names(D3_investment_count_sectorwise)[2] <- "total_no_of_investments"
D3_investment_count_sectorwise_arranged <- arrange(D3_investment_count_sectorwise, desc(total_no_of_investments))
D3_top3_sectors <- head(D3_investment_count_sectorwise_arranged, n=3)

# Identify the company receiving the highest investment of venture funding in top sector in India

D3_top_sector <- filter(D3, main_sector == D3_top3_sectors$main_sector[1]) # Filter D3 to contain data for only the top sector
D3_investment_companywise_top <- summarise(group_by(D3_top_sector, name), sum(raised_amount_usd, na.rm = T)) # Create summary of investments for each company 
names(D3_investment_companywise_top)[2] <- "total_investment_amount"
D3_investment_companywise_top_arranged <- arrange(D3_investment_companywise_top, desc(total_investment_amount)) # Arrange the investments in descending order
D3_highest_invested_company_top <- head(D3_investment_companywise_top_arranged, n=1) # Select the first row to get the company with highest investment

# Identify the company receiving the highest investment of venture funding in the second best sector in India

D3_second_best_sector <- filter(D3, main_sector == D3_top3_sectors$main_sector[2])
D3_investment_companywise_second <- summarise(group_by(D3_second_best_sector, name), sum(raised_amount_usd, na.rm = T)) # Create summary of investments for each company 
names(D3_investment_companywise_second)[2] <- "total_investment_amount"
D3_investment_companywise_second_arranged <- arrange(D3_investment_companywise_second, desc(total_investment_amount)) # Arrange the investments in descending order
D3_highest_invested_company_second <- head(D3_investment_companywise_second_arranged, n=1) # Select the first row to get the company with highest investment

##############################################################################################################################