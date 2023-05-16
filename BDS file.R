# Ben and Connor Analysis
# BDS high Tech blog post????

# Content:
# 1. Packages 
# 2. Set Paths 
# 3. Data Load 
# 4. Descriptive Figures 
# 5. Run the FECT
# 6. Counter factual Plots

##################
###  Options   ###
##################
options(scipen=100000)
# file_date <- "" # define the file date we want to use

##################
###  Library   ###
##################
library(dplyr)
library(ggplot2)
library(ggmap)
library(sf)
library(scales)
library(plm)
library(blscrapeR)
library(tidycensus)
library(censusapi)
library (readr)
library(tidyr)
library(panelView)
library(fect)

#################
### Set paths ###
#################
if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox"
}
if(Sys.info()[["user"]]=="Benjamin Glasner"){
  # Root folder
  path_project <- "C:/Users/Benjamin Glasner/Dropbox"
}
if(Sys.info()[["user"]]=="connorobrien"){
  # Root folder
  path_project <- "C~/Documents/Github"
}

# Path to saved cohort data 
path_bds <- paste0(path_project,"/GitHub/BDS-hightech")
setwd(path_bds)


##################
###  Data Load ###
##################

########
# Collect info on BDS HT at the city - year level

bds_ht_metro <- read_excel ("~/Documents/GitHub/BDS-hightech/BDS_HT_MSA.xlsx", sheet = 1, col_names = TRUE) 
bds_ht_metro[bds_ht_metro == "(D)"] <- NA

########
# Collect info on Industry Composition at the city - year level

## MSA x 2-digit sector: All BDS variables:
url <- "https://www2.census.gov/programs-surveys/bds/tables/time-series/bds2020_msa_sec.csv"
bds_sector <- read_csv(url, col_names = TRUE)
bds_sector[bds_sector == "(D)"] <- NA
## 2-digit sector employment x MSA, 1978-2020:
bds_sector_employment <- bds_sector %>% select(year, msa, sector, emp) %>% spread(key = sector, value = emp)
colnames(bds_sector_employment) <- paste('emp', colnames(bds_sector_employment), sep = "_")
bds_sector_employment <- bds_sector_employment %>% rename("year" = 1, "MSA" = 2)

########
# Collect info on housing costs at the city - year level

## FHFA Home Price Index
url <- "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_metro.csv"
fhfa_all_transactions_index <- read_csv (url, col_names = FALSE)
colnames (fhfa_all_transactions_index) <- c ("MSA_Name", "MSA", "year", "quarter", "price_index", "index_se")
## Keep quarter 1 values (to align with BDS March timeline)
fhfa_all_transactions_index_q1 <- subset(fhfa_all_transactions_index, quarter==1)

## FM Home Price Index (1975-2023)
url <- "https://www.freddiemac.com/fmac-resources/research/docs/fmhpi_master_file.csv"
freddie_mac_hpi <- read_csv(url, col_names = TRUE) %>% subset(GEO_Type == "CBSA" & Month == 1)
colnames(freddie_mac_hpi) <- c("year", "month", "geo_type", "MSA_Name", "MSA", "FMHPI_NSA", "FMHPI_SA" )

########
# Collect info on education at the city - year level

## 2005 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2005 <- getCensus(
  name = "acs/acs1",
  vintage = 2005, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*") %>% mutate(year = 2005)
colnames (acs_education_2005) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2006 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2006 <- getCensus(
  name = "acs/acs1",
  vintage = 2006, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2006)
colnames (acs_education_2006) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2007 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2007 <- getCensus(
  name = "acs/acs1",
  vintage = 2007, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2007)
colnames (acs_education_2007) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2008 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2008 <- getCensus(
  name = "acs/acs1",
  vintage = 2008, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2008)
colnames (acs_education_2008) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2009 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2009 <- getCensus(
  name = "acs/acs1",
  vintage = 2009, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2009)
colnames (acs_education_2009) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2010 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2010 <- getCensus(
  name = "acs/acs1",
  vintage = 2010, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2010)
colnames (acs_education_2010) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2011 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2011 <- getCensus(
  name = "acs/acs1",
  vintage = 2011, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2011)
colnames (acs_education_2011) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2012 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2012 <- getCensus(
  name = "acs/acs1",
  vintage = 2012, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2012)
colnames (acs_education_2012) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2013 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2013 <- getCensus(
  name = "acs/acs1",
  vintage = 2013, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2013)
colnames (acs_education_2013) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2014 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2014 <- getCensus(
  name = "acs/acs1",
  vintage = 2014, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2014)
colnames (acs_education_2014) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2015 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2015 <- getCensus(
  name = "acs/acs1",
  vintage = 2015, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2015)
colnames (acs_education_2015) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2016 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2016 <- getCensus(
  name = "acs/acs1",
  vintage = 2016, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2016)
colnames (acs_education_2016) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2017 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2017 <- getCensus(
  name = "acs/acs1",
  vintage = 2017, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2017)
colnames (acs_education_2017) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2018 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2018 <- getCensus(
  name = "acs/acs1",
  vintage = 2018, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2018)
colnames (acs_education_2018) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## 2019 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2019 <- getCensus(
  name = "acs/acs1",
  vintage = 2019, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2019)
colnames (acs_education_2019) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## No 2020 one-year ACS

## 2021 one-year ACS: Educational attainment for 25+ population: HS, BA by MSA
acs_education_2021 <- getCensus(
  name = "acs/acs1",
  vintage = 2021, 
  vars = c("NAME", "B15002_001E", "B15002_011E", "B15002_015E","B15002_028E", "B15002_032E", "B25105_001E"), 
  region = "metropolitan statistical area/micropolitan statistical area:*")%>% mutate(year = 2021)
colnames (acs_education_2021) <- c ("MSA", "MSA_Name", "Pop_25_up","Male_HS", "Male_BA","Female_HS", "Female_BA", "Median_Housing_Costs", "year")

## Combine ACS education data, 2005-2021

acs_ed_data <- rbind(acs_education_2005, acs_education_2006,acs_education_2007,acs_education_2008,acs_education_2009, acs_education_2010, acs_education_2011,acs_education_2012,acs_education_2013,acs_education_2014,acs_education_2015,acs_education_2016,acs_education_2017,acs_education_2018,acs_education_2019,acs_education_2021)

########
# Collect info on labor force participation and employment at the city - year level

########
# Combine datasets

full_bdsht_analysis_dataset <- merge(bds_ht_metro, bds_sector_employment, by.x = c("year", "MSA"), by.y = c("year", "MSA"),all.x = TRUE)
full_bdsht_analysis_dataset <- merge(full_bdsht_analysis_dataset, acs_ed_data, by.x = c("year", "MSA"), by.y = c("year", "MSA"),all.x = TRUE)
full_bdsht_analysis_dataset <- merge(full_bdsht_analysis_dataset, freddie_mac_hpi, by.x = c("year", "MSA"), by.y = c("year", "MSA"),all.x = TRUE)

clean_full_bdsht_analysis_dataset <- full_bdsht_analysis_dataset %>% mutate(BA = Male_BA + Female_BA, HS = Male_HS + Female_HS)%>% select(-c(month, geo_type, Male_BA, Female_BA, Male_HS, Female_HS, MSA_Name.x))

########
# "Treatment" status: Treated if 2008 or later, >50th percentile FMHPI, 1990-2007. Considered "dynamic" if average >50th percentile HT startup rate, 1990-2007

dynamic_costs_indicators<- clean_full_bdsht_analysis_dataset %>% filter(year > 1989 & year <2008) %>% select(year, MSA, FMHPI_SA, estabs_entry_rate_HT) %>% group_by(year) %>% mutate(pct_rank_FMHPI = percent_rank(FMHPI_SA), pct_rank_startup = percent_rank(estabs_entry_rate_HT)) %>% group_by(MSA) %>% mutate(avg_percentile_FMHPI = mean(pct_rank_FMHPI), avg_percentile_startup = mean(pct_rank_startup)) %>% mutate(high_cost = ifelse(avg_percentile_FMHPI>= 0.5,1,0), dynamic= ifelse(avg_percentile_startup>= 0.5,1,0) ) %>% select(MSA, high_cost, dynamic) %>% unique()
clean_full_bdsht_analysis_dataset <- merge(clean_full_bdsht_analysis_dataset,
                                           dynamic_costs_indicators,
                   by.x = "MSA",
                   by.y = "MSA",
                   all.x = T)
clean_full_bdsht_analysis_dataset <-clean_full_bdsht_analysis_dataset %>% mutate(treated_yr = ifelse(year >= 2008, 1,0)) %>% mutate(treated = treated_yr * high_cost) %>% filter(year<2020 & dynamic == 1)

panelview(data = clean_full_bdsht_analysis_dataset, formula = estabs_entry_rate_HT~treated, index = c("MSA","year"), 
          axis.lab = "both", xlab = "year", ylab = "MSA", 
          theme.bw = TRUE, type = "treat", main = "BDS HT Treated Status")

panelview(data = clean_full_bdsht_analysis_dataset, formula = estabs_entry_rate_HT~treated, index = c("MSA","year"), 
          axis.lab = "both", xlab = "year", ylab = "MSA", 
          theme.bw = TRUE, type = "outcome", main = "Simulated Data: Outcome")

# Convert Education and Industry Shares to percentages
## Education Share of Population 25+ with BA and HS

clean_full_bdsht_analysis_dataset <- clean_full_bdsht_analysis_dataset %>% mutate(share_BA = BA/Pop_25_up, share_HS = HS/Pop_25_up)
## Industry Shares
### Convert job totals to numeric
clean_full_bdsht_analysis_dataset <- clean_full_bdsht_analysis_dataset %>% mutate_at(c('emp_11','emp_21', 'emp_22', 'emp_23', 'emp_31-33','emp_42','emp_44-45', 'emp_48-49', 'emp_51', 'emp_52','emp_53','emp_54','emp_55','emp_56','emp_61','emp_62','emp_71','emp_72','emp_81'), as.numeric)

clean_full_bdsht_analysis_dataset <- clean_full_bdsht_analysis_dataset %>% mutate(total_employment = rowSums(across(c('emp_11','emp_21', 'emp_22', 'emp_23', 'emp_31-33','emp_42','emp_44-45', 'emp_48-49', 'emp_51', 'emp_52','emp_53','emp_54','emp_55','emp_56','emp_61','emp_62','emp_71','emp_72','emp_81')), na.rm = TRUE))

clean_full_bdsht_analysis_dataset <- clean_full_bdsht_analysis_dataset %>% 
  mutate(share_emp_11 = emp_11/total_employment, 
         share_emp_21 = emp_21/total_employment, 
         share_emp_22 = emp_22/total_employment, 
         share_emp_23 = emp_23/total_employment, 
         share_emp_31_33 = `emp_31-33`/total_employment, 
         share_emp_42 = emp_42/total_employment,
         share_emp_44_45 = `emp_44-45`/total_employment,
         share_emp_48_49 = `emp_48-49`/total_employment,
         share_emp_51 = emp_51/total_employment,
         share_emp_52 = emp_52/total_employment,
         share_emp_53 = emp_53/total_employment,
         share_emp_54 = emp_54/total_employment,
         share_emp_55 = emp_55/total_employment,
         share_emp_56 = emp_56/total_employment,
         share_emp_61 = emp_61/total_employment,
         share_emp_62 = emp_62/total_employment,
         share_emp_71 = emp_71/total_employment,
         share_emp_72 = emp_72/total_employment)



########
# Other BDS HT data (not at metro level)

bds_national_ht_sector <- read_excel ("~/Documents/GitHub/BDS-hightech/BDS_HT_NHT_National.xlsx", sheet = 1, col_names = TRUE)

########

############################
###  Descriptive Figures ###
############################

# National Data

bds_national_ht_sector %>% ggplot(aes(x = year, y = estabs_entry_rate, color = ht))



############################
###         FECT         ###
############################


# Drop if missing treated variable
clean_full_bdsht_analysis_dataset <- clean_full_bdsht_analysis_dataset %>% drop_na(treated)
# Convert startup rate to numeric
clean_full_bdsht_analysis_dataset$estabs_entry_rate_HT <- as.numeric(clean_full_bdsht_analysis_dataset$estabs_entry_rate_HT)
# Basic FEct model
out.fect <- fect(estabs_entry_rate_HT ~ treated + share_BA + share_HS + share_emp_11 + share_emp_21 + share_emp_22 + share_emp_23 + share_emp_31_33 + share_emp_42 + share_emp_44_45 + share_emp_48_49 + share_emp_51 + share_emp_52 + share_emp_53 + share_emp_54 + share_emp_55 + share_emp_56 + share_emp_61 + share_emp_62 + share_emp_71 + share_emp_72, data = clean_full_bdsht_analysis_dataset, index = c("MSA","year"), 
                 method = "fe", force = "two-way")
# Plot basic FEct model
plot(out.fect, main = "Estimated ATT (FEct)", ylab = "Effect of D on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
