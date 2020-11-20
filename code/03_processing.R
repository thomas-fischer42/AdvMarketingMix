## ---------------------------
##
## Script name: 03_processing.R
##
## Purpose of script: Data preprocessing
##
## Author: Thomas Fischer
##
## Date Created: 2020-11-16
##
## ---------------------------

kaffee <- read.csv( "data/Kaffee1.csv", sep = ";", header = FALSE, col.names = column_names)
rows <- nrow(kaffee)

kaffee_households <- split(kaffee, kaffee$ID) %>%
	lapply(function(data) {
		data <- data %>% 
			reshapePurchasedBrand() %>%
			arrange(ID, Days) %>% 
			estimateInventory() %>%    
			lastBrandPurchased()     %>%    # Dummy variable (=1 if same brand was purchased on last occasion)
			hasReducedPrice()        %>%    # Dummy variable (=1 if price has decreased since last trip)
			averagePurchaseQty()     %>%    # Average purchase quantity if something was purchased
			estimateBrandLoyalty()
		return(data)
	})

kaffee2 <- do.call("rbind", kaffee_households)
