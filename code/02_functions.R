


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



# Data wrangling ----

drop_duplicates <- function(data) {
	return(data[!duplicated(data), ])
}

reshapePurchasedBrand <- function(data) {
	brand_purchases[cbind(1:rows, data$Purchase_Brand + 1)] <- 1
	return(data.frame(data, brand_purchases[, -1]))
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

# Inventory and Consumption ----

averagePurchaseQty <- function(data) {
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
	
	CR <- estimateConsumptionRate(data$Days, data$Purchase_Quantity)
	Consumption <- CR * checkNewDay(data$Days)
	
	for (i in 2:rows) {
		Inventory[i] <- Inventory[i-1] + data$Purchase_Quantity[i-1] - Consumption[i-1]
		
		# Ensure that Consumption does not exceed Inventory on day i
		Consumption[i] <- min(Inventory[i], Consumption[i])
	}
	
	return(data.frame(data, CR = CR, Consumption = Consumption, Inventory = Inventory))
}

# Brand loyalty ----

# estimateBrandLoyalty <- function(data) {
# 	rows <- nrow(data)
# 	
# 	brand_purchases <- apply(data[,brand_names], 2, cumsum)
# 	category_purchases <- rowSums(brand_purchases)
# 	
# 	brand_loyalty <- matrix(0, nrow = rows, ncol = 7)
# 	ind <- category_purchases > 0
# 	brand_loyalty[ind,] <- brand_purchases[ind, ] / category_purchases[ind]
# 	brand_loyalty[!ind,] <- 1/7
# 	colnames(brand_loyalty) <- paste(brand_names, "Loyalty", sep = "_")
# 	return(data.frame(data, brand_loyalty))
# }


estimateBrandLoyalty <- function(data) {
	# Only up to week 61 for calibration
	calibr <- which(data$Days <= 61 * 7)

	for (id in unique(data$ID)) {
		index <- which(data$ID == id)
		brand_loyalty[index,] <- colSums(data[intersect(index, calibr), brand_names]) / sum(data[intersect(index, calibr), brand_names])
	}
	
	return(data.frame(data, brand_loyalty))
}


lastBrandPurchased <- function(data) {
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
	
	for (id in unique_id) {
		index <- which(data$ID == id)
		last <- numeric(brands)
		
		for (j in 1:length(index)) {
			for (p in 1:brands) {
				if (data[j, brand_price[p]] != 0) {
					price_reduced[j,p] <- data[j, brand_price[p]] < last[p]
					last[p] <- data[j, brand_price[p]]
				}
			}
		}
	}
	return(data.frame(data, price_reduced))
}


