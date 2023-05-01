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
# Path to saved cohort data 
path_bds <- paste0(path_project,"/GitHub/BDS-hightech")
setwd(path_bds)


##################
###  Data Load ###
##################

########
# Collect info on BDS at the city - year level


########
# Collect info on Industry Composition at the city - year level


########
# Collect info on housing costs at the city - year level


########
# Collect info on education at the city - year level


########
# Collect info on labor force participation and employment at the city - year level



############################
###  Descriptive Figures ###
############################



############################
###         FECT         ###
############################

