
reorganizePars.quantity <- function(pars) {
	intercept <- pars[1]
	brand_intercept <- c(0, pars[2:brands])
	covariates <- pars[brands + 1:5]
	return(list(Intercept = intercept, Brand_Intercept = brand_intercept, Covariates = covariates))
}

quantityModel <- function(data) {
	
	likelihood <- function(pars, return.model = TRUE) {
		
		mat <- as.matrix(data)
		pars <- reorganizePars.quantity(pars)
		
		for (id in unique_id) {
			index <- which(mat[,"ID"] == id)
			
			for (p in 1:brands) {
				predictors <- c("PR", "Inventory", paste(brand_names[p], c("Loyalty", "Price", "Pricecut"), sep = "_"))
				linpred <- pars[["Intercept"]] + pars[["Brand_Intercept"]][p] + mat[index, predictors] %*% pars[["Covariates"]]
				lambda[index, p] <- exp(linpred)
			}
		}
		logprob_quantity <- log(dtruncpois(lambda, mat[, "Purchase_Quantity"]))
		loglik <- rowSums(logprob_quantity * mat[, brand_names])
		data <- data.frame(data, LL_QTY = loglik)
		if (return.model) return(data)
		return(-sum(loglik))
	}
	return(likelihood)
}

# res <- optim(rep(0, 12), fn = quantityModel(kaffee), return.model=FALSE, control = list(maxit = 1e4))

# tmp <- quantityModel(kaffee)
# test <- tmp(rep(0, 12))