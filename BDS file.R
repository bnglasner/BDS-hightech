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
library(tidycensus)
library(censusapi)
library (readr)
library(tidyr)
library(panelView)
library(fect)
library(readxl)

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
clean_full_bdsht_analysis_dataset <- clean_full_bdsht_analysis_dataset %>% mutate_at(c('emp_11','emp_21', 'emp_22', 'emp_23', 'emp_31-33','emp_42','emp_44-45', 'emp_48-49', 'emp_51', 'emp_52','emp_53','emp_54','emp_55','emp_56','emp_61','emp_62','emp_71','emp_72','emp_81','emp_HT', 'emp_nonHT'), as.numeric)

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
         share_emp_72 = emp_72/total_employment,
         share_emp_81 = emp_81/total_employment)


# Create the lagged versions of the employment variables
df_pseries <- pdata.frame(clean_full_bdsht_analysis_dataset, index = c("MSA", "year"))

df_pseries$share_emp_11_lag <- lag(df_pseries$share_emp_11)
df_pseries$share_emp_21_lag <- lag(df_pseries$share_emp_21)
df_pseries$share_emp_22_lag <- lag(df_pseries$share_emp_22)
df_pseries$share_emp_23_lag <- lag(df_pseries$share_emp_23)
df_pseries$share_emp_31_33_lag <- lag(df_pseries$share_emp_31_33)
df_pseries$share_emp_42_lag <- lag(df_pseries$share_emp_42)
df_pseries$share_emp_44_45_lag <- lag(df_pseries$share_emp_44_45)
df_pseries$share_emp_48_49_lag <- lag(df_pseries$share_emp_48_49)
df_pseries$share_emp_51_lag <- lag(df_pseries$share_emp_51)
df_pseries$share_emp_52_lag <- lag(df_pseries$share_emp_52)
df_pseries$share_emp_53_lag <- lag(df_pseries$share_emp_53)
df_pseries$share_emp_54_lag <- lag(df_pseries$share_emp_54)
df_pseries$share_emp_55_lag <- lag(df_pseries$share_emp_55)
df_pseries$share_emp_56_lag <- lag(df_pseries$share_emp_56)
df_pseries$share_emp_61_lag <- lag(df_pseries$share_emp_61)
df_pseries$share_emp_62_lag <- lag(df_pseries$share_emp_62)
df_pseries$share_emp_71_lag <- lag(df_pseries$share_emp_71)
df_pseries$share_emp_72_lag <- lag(df_pseries$share_emp_72)
df_pseries$share_emp_81_lag <- lag(df_pseries$share_emp_81)

clean_full_bdsht_analysis_dataset <- as.data.frame(df_pseries)



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

# Dependent Variable list



# Drop if missing treated variable
clean_full_bdsht_analysis_dataset <- clean_full_bdsht_analysis_dataset %>% drop_na(treated)
# Convert startup rate to numeric
clean_full_bdsht_analysis_dataset$estabs_entry_rate_HT <- as.numeric(clean_full_bdsht_analysis_dataset$estabs_entry_rate_HT)
clean_full_bdsht_analysis_dataset <- clean_full_bdsht_analysis_dataset %>% mutate_at(c('treated','share_emp_11_lag','share_emp_21_lag', 'share_emp_22_lag', 'share_emp_23_lag', 'share_emp_31_33_lag','share_emp_42_lag','share_emp_44_45_lag', 'share_emp_48_49_lag', 'share_emp_51_lag', 'share_emp_52_lag','share_emp_53_lag','share_emp_54_lag','share_emp_55_lag','share_emp_56_lag','share_emp_61_lag','share_emp_62_lag','share_emp_71','share_emp_72_lag','share_emp_81_lag','estabs_entry_rate_HT'), as.numeric)


# Basic FEct model
out.fect <- fect(estabs_entry_rate_HT ~ treated + share_emp_11_lag + share_emp_21_lag + share_emp_22_lag + share_emp_23_lag + share_emp_31_33_lag + share_emp_42_lag + share_emp_44_45_lag + share_emp_48_49_lag + share_emp_51_lag + share_emp_52_lag + share_emp_53_lag + share_emp_54_lag + share_emp_55_lag + share_emp_56_lag + share_emp_61_lag + share_emp_62_lag + share_emp_71_lag + share_emp_72_lag + share_emp_81_lag, data = clean_full_bdsht_analysis_dataset, index = c("MSA","year"), 
                 method = "fe", force = "two-way")


# Plot basic FEct model
plot(out.fect, main = "Estimated ATT (FEct)", ylab = "Effect of D on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)


############################
###         FECT         ###
############################
using <- clean_full_bdsht_analysis_dataset

using <- using %>% mutate(hightechshare = (emp_HT/(emp_HT + emp_nonHT))*100,
                          share_emp_51 = share_emp_51*100)
using <- using %>% mutate_at(c('treated','hightechshare','estabs_entry_rate_HT', 'estabs_entry_rate_nonHT', 'job_creation_rate_HT', 'job_creation_rate_nonHT','reallocation_rate_HT','reallocation_rate_nonHT'), as.numeric)

dependent_list <- c("hightechshare", 
                    "estabs_entry_rate_HT",
                    "estabs_entry_rate_nonHT",
                    "job_creation_rate_HT",
                    "job_creation_rate_nonHT", 
                    "reallocation_rate_HT",
                    "reallocation_rate_nonHT")

dependent_title <- c("Share of Employment in High Tech",
                     "High Tech Estab. Entry Rate",
                     "Non-High Tech Estab. Entry Rate", 
                     "High Tech Job Creation Rate",
                     "Non-High Tech Job Creation Rate", 
                     "High Tech Reallocation Rate",
                     "Non-High Tech Reallocation Rate")
data_list <- list()
fect_list <- list()

for(j in seq_along(dependent_list)){
  data_list[[j]] <- using
  names(data_list[[j]])[which(colnames(data_list[[j]])==dependent_list[[j]])] <- "dependent"
  
}

sapply(using, function(x) sum(is.na(x)))

for(i in seq_along(data_list)){
  
  fect_list[[i]] <- fect(data = data_list[[i]],
                         formula = dependent ~ treated +  share_emp_11_lag + share_emp_21_lag + share_emp_22_lag + share_emp_23_lag + share_emp_31_33_lag + share_emp_42_lag + share_emp_44_45_lag + share_emp_48_49_lag + share_emp_51_lag + share_emp_52_lag + share_emp_53_lag + share_emp_54_lag + share_emp_55_lag + share_emp_56_lag + share_emp_61_lag + share_emp_62_lag + share_emp_71_lag + share_emp_72 + share_emp_81_lag,
                         force = "two-way",
                         index = c("MSA","year"),
                         method = "both",
                         # method = "fe",
                         # method = "ife",
                         # method = "mc",
                         nlambda = 5,
                         vartype = "bootstrap",
                         nboots = 1000,
                         parallel = TRUE,
                         cores = 8,
                         # force = "time",
                         min.T0 = 3,
                         se = TRUE,
                         na.rm = FALSE)
}

for(i in seq_along(data_list)){
  
  fect_list[[i]] <- fect(data = data_list[[i]],
                         formula = dependent ~ treated,
                         force = "two-way",
                         index = c("MSA","year"),
                         method = "both",
                         # method = "fe",
                         # method = "ife",
                         # method = "mc",
                         nlambda = 5,
                         vartype = "bootstrap",
                         nboots = 1000,
                         parallel = TRUE,
                         cores = 8,
                         # force = "time",
                         min.T0 = 3,
                         se = TRUE,
                         na.rm = FALSE)
}

for(i in seq_along(fect_list)){
  print("***********************************")
  print(i)
  print(dependent_title[[i]])
  print("***********************************")
  print(fect_list[[i]])
  print("***********************************")
  print("***********************************")
}

#############################################

###    Counter Factual Plot               ###

#############################################

Outcome_paths <- list()
counterfactual_plots <- list()

for (i in seq_along(fect_list)) {
  ###############################
  
  # Define who is and is not treated via the FEct
  IDs_treatmentstatus <- as.data.frame(cbind(fect_list[[i]][["id"]],fect_list[[i]][["unit.type"]]))
  IDs_treated <- IDs_treatmentstatus %>% filter(V2!=1)
  IDs_treated_list <- as.vector(IDs_treated$V1)
  
  IDs_control <- IDs_treatmentstatus %>% filter(V2==1)
  IDs_control_list <- as.vector(IDs_control$V1)
  
  idx <- match(IDs_treated_list, names(as.data.frame(fect_list[[i]][["eff"]])))
  idx_control <- match(IDs_control_list, names(as.data.frame(fect_list[[i]][["eff"]])))
  
  ###########################
  # capture the average predicted path for each age from FEct 
  eff <- as.data.frame(fect_list[[i]][["eff"]])[,idx] 
  Y.ct <- as.data.frame(fect_list[[i]][["Y.ct"]])[,idx] 
  
  eff <- as.data.frame(rowMeans(eff, na.rm = TRUE))
  Y.ct <- as.data.frame(rowMeans(Y.ct, na.rm = TRUE))
  
  effect_df <- cbind(eff,Y.ct,fect_list[[i]][["rawtime"]])
  names(effect_df) <- c("Effect","Predicted","Year")
  
  Outcome_paths[[i]] <- effect_df
  #############################
  # Calculate the observed average path for treated and control units
  
  treat <- data_list[[i]] %>% filter(MSA %in% IDs_treated_list) %>% group_by(year) %>% summarise(Treated = mean(dependent,na.rm=TRUE)) %>% mutate(Year = year)
  control <- data_list[[i]] %>% filter(MSA %in% IDs_control_list) %>% group_by(year) %>% summarise(Control = mean(dependent,na.rm=TRUE)) %>% mutate(Year = year)
  
  Outcome_paths[[i]] <- left_join(Outcome_paths[[i]],treat)
  Outcome_paths[[i]] <- left_join(Outcome_paths[[i]],control)
  
  counterfactual_plots[[i]] <- Outcome_paths[[i]] %>% 
    ggplot(aes(x = Year)) +
    geom_vline(xintercept = 2008) +
    geom_line(aes(y = Predicted,color = 'Predicted')) + 
    geom_line(aes(y = Treated,color = 'Treated')) + 
    geom_point(aes(y = Predicted,color = 'Predicted')) + 
    geom_point(aes(y = Treated,color = 'Treated')) + 
    ggtitle(dependent_title[[i]]) +
    ylab("Predicted or Observed") +
    theme(plot.title = element_text(size=20),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 18),
          strip.text.x = element_text(size = 20),
          strip.text.y = element_text(size = 15),
          strip.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_line(colour = "grey"),
          panel.spacing = unit(2, "lines"),
          axis.line = element_line(colour = "black"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key = element_rect(fill="white"),
          legend.key.width = unit(2,"cm"),
          legend.text = element_text(size = 18),
          legend.background = element_rect(fill=NA),
          plot.caption = element_text(hjust = 0.5,size = 15))    
  
}

for(i in seq_along(counterfactual_plots)){
  plot(counterfactual_plots[[i]])
}


ATT <- list()
se <- list()

for (i in seq_along(fect_list)) {
  ATT[[i]] <- fect_list[[i]]$att.avg
  
  weighted_boots <- fect_list[[i]][["att.avg.boot"]]
  weighted_boots <- as.data.frame(t(weighted_boots))
  
  se[[i]] <- sd(weighted_boots$V1)
}

results_fect <- as.data.frame(cbind(dependent_list,
                                    dependent_title,
                                    do.call(rbind,ATT),
                                    do.call(rbind,se)))
results_fect <- results_fect %>% rename("ATT" = "V3",
                                        "SE" = "V4") %>%
  mutate(ATT = as.numeric(ATT),
         SE = as.numeric(SE))


results_fect <- results_fect %>% mutate(Significant = if_else((ATT - SE*1.95)>0,"Positive, Sig. (95%)",
                                                              if_else((ATT + SE*1.95)<0, "Negative, Sig. (95%)","Insignificant")))

group.colors <- c("Positive, Sig. (95%)" = "darkgreen", "Negative, Sig. (95%)" = "darkred","Insignificant" ="grey50")

P <- results_fect %>% 
  ggplot(aes(x = dependent_title,
             y = ATT,
             color = Significant)) + 
  geom_hline(yintercept = 0) +
  geom_point(size = 4) + 
  geom_errorbar(aes(ymin = ATT - 1.95*SE, ymax = ATT + 1.95*SE)) + 
  scale_color_manual(values=group.colors)+ 
  theme(plot.title = element_text(size=20),
        axis.text.y = element_text(size = 25),
        axis.text.x = element_text(size = 15, angle = 90),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        strip.text.x = element_text(size = 30),
        strip.text.y = element_text(size = 30),
        strip.background = element_rect(fill = "white"),
        strip.placement = "outside",
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey"),
        panel.spacing = unit(2, "lines"),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_text(size = 20),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size = 25),
        legend.background = element_rect(fill=NA)) +
  guides(col = guide_legend(title = "")) 
P
