

base_model <- function(data) {
	
	# Initialize empty environment
	model <- env()
	
	# Data
	model$data <- list( ID        = as.vector(data$ID),
											PR        = as.vector(data$PR),
											CR        = as.vector(data$CR),
											INVENTORY = as.vector(data$Inventory),
											VALUE     = as.vector(data$Purchase_Value),
											QTY       = as.vector(data$Purchase_Quantity),
											BRAND     = as.matrix(data[, brand_names]),
											PRICE     = as.matrix(data[, paste(brand_names, "Price", sep = "_")]),
											LOYALTY   = as.matrix(data[, paste(brand_names, "Loyalty", sep = "_")]),
											LBP       = as.matrix(data[, paste(brand_names, "LBP", sep = "_")]))
	
	return(model)
}
