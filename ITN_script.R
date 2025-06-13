##ITN Utilization##

# ── 0. Packages ─────────────────────────────────────────────────────────────
library(dplyr)      # data wrangling
library(stringr)    # string operations
library(sf)         # spatial operations
library(readr)      # CSV I/O
library(ggplot2)    # plotting
library(scales)     # squish() for color capping
library(stringdist) # fuzzy matching


## Read data from csv
data_1<-read.csv("DHS_ITN_person_data.csv")

## keep only who slept under net##
# this the data set that we will be using it

data_2<-data_1 %>%  filter(slept_under_net=="yes")

## for the start


