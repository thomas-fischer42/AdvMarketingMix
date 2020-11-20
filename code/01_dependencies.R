## ---------------------------
##
## Script name: 01_dependencies.R
##
## Purpose of script: Load packages and initialize
##										empty data structures
##
## Author: Thomas Fischer
##
## Date Created: 2020-11-16
##
## ---------------------------

# Dependencies ----

library(dplyr)
library(ggplot2)

# Constants ----

brands              <- 7
brand_names         <- paste("Brand", 1:brands, sep = "_")
brand_price         <- paste(brand_names, "Price", sep = "_")
column_names        <- c("ID", "Days", "ID_Store", "Purchase_Value", "Purchase_Category", "Purchase_Brand", "Purchase_Quantity", "Days_last_trip", brand_price)


