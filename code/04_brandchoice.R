
reorganizePars.brandchoice <- function(pars) {
	intercept <- pars[1]
	brand_intercept <- c(0, pars[2:brands])
	covariates <- pars[brands + 1:4]
	return(list(Intercept = intercept, Brand_Intercept = brand_intercept, Covariates = covariates))
}


brandChoiceModel <- function(data) {
	
	unique_id <- unique(kaffee2$ID)

	# Pre-initialize containers ----
	utility             <- matrix(0, nrow = rows, ncol = brands)
	logprob_brandchoice <- matrix(0, nrow = rows, ncol = brands)
	cv                  <- numeric(rows)
	loglik              <- numeric(rows)
	
	# Add names to containers ----
	colnames(logprob_brandchoice) <- paste(brand_names, "LP_BCH", sep = "_")
	
	likelihood <- function(pars, return.data=TRUE) {
		mat <- as.matrix(data)
		pars <- reorganizePars.brandchoice(pars)
		
		for (id in unique_id) {
			index <- which(mat[,"ID"] == id)
			
			for (p in 1:brands) {
				predictors <- paste(brand_names[p], c("Loyalty", "LBP", "Price", "Pricecut"), sep = "_")
				utility[index,p] <- (pars[["Intercept"]] + pars[["Brand_Intercept"]][p] + mat[index, predictors] %*% pars[["Covariates"]])
			}
		}
		
		cv <- log(rowSums(exp(utility)))
		logprob_brandchoice <- log(exp(utility)/exp(cv))
		loglik <- rowSums(logprob_brandchoice * mat[, brand_names])
		LL <- sum(loglik)
		
		if (return.data) {
			data <- data.frame(data, CV = cv, LL_BCH = loglik)
			return(data)
		}
		
		if (is.nan(LL)) stop("Brandchoice model yielded NAN loglikelihood")
		if (is.infinite(LL)) LL <- -1e100
		
		return(-LL)
	}
	return(likelihood)
}
