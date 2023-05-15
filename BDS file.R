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

########
# Collect info on Industry Composition at the city - year level

## MSA x 2-digit sector: All BDS variables:
url <- "https://www2.census.gov/programs-surveys/bds/tables/time-series/bds2020_msa_sec.csv"
bds_sector <- read_csv(url, col_names = TRUE)
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

