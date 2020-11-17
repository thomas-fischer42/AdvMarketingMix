
# Dependencies ----

library(dplyr)
library(ggplot2)

# Constants ----

brands              <- 7
brand_names         <- paste("Brand", 1:brands, sep = "_")
brand_price         <- paste(brand_names, "Price", sep = "_")
column_names        <- c("ID", "Days", "ID_Store", "Purchase_Value", "Purchase_Category", "Purchase_Brand", "Purchase_Quantity", "Days_last_trip", brand_price)
kaffee              <- read.csv( "data/Kaffee1.csv", sep = ";", header = FALSE, col.names = column_names)
rows                <- nrow(kaffee)
unique_id           <- unique(kaffee$ID)

# Pre-initialize containers ----
brand_purchases     <- matrix(0,  nrow = rows, ncol = brands + 1)
brand_loyalty       <- matrix(0,  nrow = rows, ncol = brands)
LBP                 <- matrix(0,  nrow = rows, ncol = brands)
price_reduced       <- matrix(0,  nrow = rows, ncol = brands)
utility             <- matrix(NA, nrow = rows, ncol = brands)
lambda              <- matrix(NA, nrow = rows, ncol = brands)
logprob_brandchoice <- matrix(NA, nrow = rows, ncol = brands)
logprob_quantity    <- matrix(NA, nrow = rows, ncol = brands)

Inventory           <- numeric(rows)
avg_qty             <- numeric(rows)
logprob_purchase    <- numeric(rows)
cv                  <- numeric(rows)
loglik              <- numeric(rows)

# Add names to containers ----
colnames(brand_loyalty)       <- paste(brand_names, "Loyalty", sep = "_")
colnames(LBP)                 <- paste(brand_names, "LBP", sep = "_")
colnames(price_reduced)       <- paste(brand_names, "Pricecut", sep = "_")
colnames(logprob_brandchoice) <- paste(brand_names, "LP_BCH", sep = "_")
colnames(logprob_quantity)    <- paste(brand_names, "LP_QTY", sep = "_")
colnames(brand_purchases)     <- c("None", brand_names)

logprob_quantity    <- matrix(NA, nrow = rows, ncol = brands)

