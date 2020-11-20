
reorganizePars.quantity <- function(pars) {
	intercept <- pars[1]
	brand_intercept <- c(0, pars[2:brands])
	covariates <- pars[brands + 1:5]
	return(list(Intercept = intercept, Brand_Intercept = brand_intercept, Covariates = covariates))
}

quantityModel <- function(data) {
	
	unique_id <- unique(kaffee2$ID)
	
	# Pre-initialize containers ----
	linpred             <- matrix(0,  nrow = rows, ncol = brands)
	lambda              <- matrix(0, nrow = rows, ncol = brands)
	logprob_quantity    <- matrix(0, nrow = rows, ncol = brands)
	loglik              <- numeric(rows)
	
	# Add names to containers ----
	colnames(logprob_quantity) <- paste(brand_names, "LP_QTY", sep = "_")

	likelihood <- function(pars, return.data = TRUE) {
		
		mat <- as.matrix(data)
		pars <- reorganizePars.quantity(pars)
		
		for (id in unique_id) {
			index <- which(mat[,"ID"] == id)
			
			for (p in 1:brands) {
				predictors <- c("PR", "Inventory", paste(brand_names[p], c("Loyalty", "Price", "Pricecut"), sep = "_"))
				linpred[index, p] <- pars[["Intercept"]] + pars[["Brand_Intercept"]][p] + mat[index, predictors] %*% pars[["Covariates"]]
			}
		}
		
		lambda <- exp(linpred)
		logprob_quantity <- log(dtruncpois(lambda, mat[, "Purchase_Quantity"]))
		loglik <- rowSums(logprob_quantity * mat[, brand_names], na.rm = TRUE)
		loglik[is.nan(loglik)] <- 0 # log(0) * 0
		
		LL <- sum(loglik)
		
		if (return.data) {
			data <- data.frame(data, LL_QTY = loglik)
			return(data)
		}
		
		if (is.nan(LL)) stop("Quantity model yielded NAN loglikelihood")
		if (is.infinite(LL)) LL <- -1e100
		return(-LL)
	}
	return(likelihood)
}

