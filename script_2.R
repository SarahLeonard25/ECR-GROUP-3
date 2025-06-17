##call libraries##
library(dplyr)
library(ggplot2)
library(stringr)    # string operations
library(sf)         # spatial operations
library(readr)      # CSV I/O
library(scales)     # squish() for color capping
library(stringdist) # fuzzy matching
library(tidyverse)
library(survey)


##installing required libraries (option 1)##
install.packages("devtools")
devtools::install_github("ropensci/rdhs")
library(rdhs)

install.packages("survey")

# Install remotes if option 1 failed (option 2)

if (!require("remotes")) install.packages("remotes")

# Install rdhs from GitHub

remotes::install_github("ropensci/rdhs")
library(rdhs)

## load data file
getwd()
data <- read.csv("DHS_ITN_person_data.csv")


##creating dataset by parts
##for adequate ITN

data6<- data %>% 
  select(country, admin1_dhs, admin1_name, dhsid, urban_rural,
         clusterid, householdid,personid,netid,slept_under_net_types) %>%
  mutate(across(netid, ~ replace_na(netid, 0))) %>%
  mutate(has_ITN=ifelse(netid>0&
                          (slept_under_net_types=="treated and untreated nets"|slept_under_net_types=="treated nets"),1,0))%>%
  group_by(householdid) %>%
  mutate(across(has_ITN, ~ replace_na(has_ITN, 0))) %>%
  mutate(HHmember_count=n_distinct(personid))%>%
  filter(netid==max(netid))%>%
  select(-c(personid))%>%
  slice(1)


##creating variables to be used in calculating indicators

HH_data <- data6 %>%
  mutate(
    net_per_person = netid / HHmember_count)

### Create survey design with clusters only (no weights/strata)
svy_design <- svydesign(
  ids = ~clusterid,      # Primary sampling unit (PSU)
  strata = NULL,         # No stratification
  weights = ~1,          # Equal weights (assume self-weighting)
  data = HH_data,
  nest = TRUE            # Handle nested households if applicable
)

##create ITN utilization
data_with_ITN_use<- data %>% 
  select(country, admin1_dhs, admin1_name, dhsid, urban_rural,
         clusterid, householdid,personid,slept_under_net_types,sex,
         slept_under_itn) %>%
  mutate(itn_use =
           ifelse(slept_under_itn == "yes",1,     # Yes
                  ifelse(slept_under_itn == "no", 0, NA)))

##creating ITN utilization variables
HH_ITN_utilization_variables<-data_with_ITN_use %>%
  group_by(householdid) %>%
  mutate(ITN_use_count=n_distinct(itn_use))%>%
  mutate(HHmember_count=n_distinct(personid))%>%
  select(-c(personid,sex,slept_under_net_types,slept_under_itn,itn_use))%>%
  slice(1)


##computing ITN utilization

HH_with_ITN_utilization<-HH_ITN_utilization_variables %>%
  mutate(ITN_utilization=ITN_use_count/HHmember_count)

##save in excell

write.csv(HH_with_ITN_utilization, "C:/Users/HP EliteBook/OneDrive/Desktop/Group 2/data/HH_with_ITN_utilization.csv", row.names = FALSE)

write.csv(HH_ITN_utilization_variables, "C:/Users/HP EliteBook/OneDrive/Desktop/Group 2/data/HH_ITN_utilization_variables.csv", row.names = FALSE)

write.csv(data_with_ITN_use, "C:/Users/HP EliteBook/OneDrive/Desktop/Group 2/data/data_with_ITN_use.csv", row.names = FALSE)

write.csv(HH_data, "C:/Users/HP EliteBook/OneDrive/Desktop/Group 2/data/HH_data.csv", row.names = FALSE)

write.csv(data6, "C:/Users/HP EliteBook/OneDrive/Desktop/Group 2/data/data6.csv", row.names = FALSE)


