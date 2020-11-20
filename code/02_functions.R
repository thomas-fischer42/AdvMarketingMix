## ---------------------------
##
## Script name: 02_functions.R
##
## Purpose of script: Initialize custom functions
##
## Author: Thomas Fischer
##
## Date Created: 2020-11-16
##
## ---------------------------


# Variable transformation ----

logit <- function(p) {
	log(p) - log(1-p)
}

expit <- function(x) {
	1/(1 + exp(-x))
}

dtruncpois <- function(lambda, quantity) {
	exp(-lambda) * lambda^quantity / ((1-exp(-lambda))*factorial(quantity))
}

getStorePrices <- function(data, id = NULL) {
	if (!is.null(id)) {
		store_data <- drop_duplicates(subset(data, ID_Store == id, select = c("Days", brand_price)))
	} else {
		store_data <- drop_duplicates(subset(data, select = c("ID_Store", "Days", brand_price)))
	}
	return(store_data[order(store_data$Days),])
}

getPurchaseVolume <- function(data) {
	tmp <- subset(data, Purchase_Quantity > 0, select = c("Days", "Purchase_Quantity", brand_names))
	tmp[,brand_names] <- tmp[,brand_names] * tmp[,"Purchase_Quantity"]
	
	tmp[, c("Days", brand_names)] %>%
		tidyr::pivot_longer(col = starts_with("Brand"), names_to = "Brand", values_to = "Units") %>%
		group_by(Days, Brand) %>% 
		summarise(across(.fns = sum)) %>% 
		ungroup()
}

# Data wrangling ----

drop_duplicates <- function(data) {
	return(data[!duplicated(data), ])
}

reshapePurchasedBrand <- function(data) {
	rows <- nrow(data)
	brand_purchases <- matrix(0,  nrow = rows, ncol = brands + 1)
	colnames(brand_purchases) <- c("None", brand_names)
	brand_purchases[cbind(1:rows, data$Purchase_Brand + 1)] <- 1
	return(data.frame(data, brand_purchases[, -1]))
}



# Inventory and Consumption ----

averagePurchaseQty <- function(data) {
	rows <- nrow(data)
	avg_qty             <- numeric(rows)
	purch <- which(data$Purchase_Quantity > 0)	
	
	for (id in unique(data$ID)) {
		index <- which(data$ID == id)
		avg_qty[index] <- sum(data[intersect(index,purch), "Purchase_Quantity"]) / length(intersect(index, purch))
	}
	
	data$PR <- avg_qty
	return(data)
}

estimateConsumptionRate <- function(Days, Purchase_Quantity) {
	return(sum(Purchase_Quantity) / max(Days))
}

checkNewDay <- function(Days) {
	return(c(FALSE, diff(Days) != 0))
}

estimateInventory <- function(data) {
	rows <- nrow(data)
	Inventory <- numeric(rows)
	
	CR <- estimateConsumptionRate(data$Days, data$Purchase_Quantity)
	Consumption <- CR * (data$Days - lag(data$Days, default = 0))
	
	for (i in 2:rows) {
		Inventory[i] <- max(0, Inventory[i-1] + data$Purchase_Quantity[i-1] - Consumption[i-1])
	}
	
	return(data.frame(data, CR = CR, Consumption = Consumption, Inventory = Inventory))
}


estimateBrandLoyalty <- function(data) {
	rows <- nrow(data)
	brand_loyalty <- matrix(0,  nrow = rows, ncol = brands)
	
	calibr <- which(data$Days <= 61 * 7)
	brand_loyalty <- matrix(rep(colSums(data[calibr, brand_names]) / sum(data[calibr, brand_names]), each = rows), rows, brands) 
	colnames(brand_loyalty) <- paste(brand_names, "Loyalty", sep = "_")
	
	return(data.frame(data, brand_loyalty))
}


lastBrandPurchased <- function(data) {
	
	rows <- nrow(data)
	LBP                 <- matrix(0,  nrow = rows, ncol = brands)
	colnames(LBP)                 <- paste(brand_names, "LBP", sep = "_")
	
	last <- 0
	
	for (i in 1:rows) {
		if (data$Purchase_Brand[i] != 0) { 
			LBP[i, data$Purchase_Brand[i]] <- data$Purchase_Brand[i] == last
			last <- data$Purchase_Brand[i] 
		}
	}
	return(data.frame(data, LBP))
}


# Promotion ----
hasReducedPrice <- function(data) {
	
	rows <- nrow(data)
	price_reduced <- matrix(0,  nrow = rows, ncol = brands)
	colnames(price_reduced) <- paste(brand_names, "Pricecut", sep = "_")
	
	last <- numeric(brands)
		
	for (i in 1:rows) {
		for (p in 1:brands) {
			if (data[i, brand_price[p]] != 0) {
					price_reduced[i,p] <- data[i, brand_price[p]] < last[p]
					last[p] <- data[i, brand_price[p]]
			}
		}
	}
	
	return(data.frame(data, price_reduced))
}


