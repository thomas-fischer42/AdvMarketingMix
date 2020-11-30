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



logit <- function(p) {
	log(p) - log(1-p)
}

expit <- function(x) {
	1/(1 + exp(-x))
}

dtruncpois <- function(x, lambda, .log = FALSE) {
	if (any(x %% 1 != 0)) {
		warning("Non-integer input for x")
		x <- floor(x)
	}
	
	if (.log) return( (-lambda)+log(lambda)*x - log((1-exp(-lambda))*factorial(x)) + log(x>0)) 
	else return((exp(-lambda) * lambda^x / ((1-exp(-lambda))*factorial(x)))*(x > 0))
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
	
	rows          <- nrow(data)
	LBP           <- matrix(0,  nrow = rows, ncol = brands)
	colnames(LBP) <- paste(brand_names, "LBP", sep = "_")
	
	last <- 0
	
	for (i in 1:rows) {
		if (data$Purchase_Brand[i] != 0) { 
			LBP[i, data$Purchase_Brand[i]] <- data$Purchase_Brand[i] == last
			last <- data$Purchase_Brand[i] 
		}
		LBP[i, last] <- 1
	}
	return(data.frame(data, LBP))
}


basePrice <- function(data) {
	
	rows <- nrow(data)
	base_price <- matrix(0,  nrow = rows, ncol = brands)
	price_cut <- matrix(0, nrow = rows, ncol = brands)
	colnames(base_price) <- paste(brand_names, "Baseprice", sep = "_")
	colnames(price_cut) <- paste(brand_names, "Pricecut", sep = "_")
	
	for (p in 1:brands) {
		tab <- table(data[, paste(brand_names[p], "Price", sep = "_")])
		tab <- tab[names(tab) != "0"]
		base_price[,p] <- as.numeric(names(tab[which.max(tab)]))
		price_cut[,p] <- data[, paste(brand_names[p], "Price", sep = "_")] != 0 & 
										 data[, paste(brand_names[p], "Price", sep = "_")] < base_price[,p]
	}
	return(data.frame(data, base_price, price_cut))
}




