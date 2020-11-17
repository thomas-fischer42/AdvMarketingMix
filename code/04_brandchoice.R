
reorganizePars.brandchoice <- function(pars) {
	intercept <- pars[1]
	brand_intercept <- c(0, pars[2:brands])
	covariates <- pars[brands + 1:4]
	return(list(Intercept = intercept, Brand_Intercept = brand_intercept, Covariates = covariates))
}


brandChoiceModel <- function(data) {
	
	likelihood <- function(pars, return.model=TRUE) {
		
		mat <- as.matrix(data)
		pars <- reorganizePars.brandchoice(pars)
		for (id in unique_id) {
			index <- which(mat[,"ID"] == id)
			
			for (p in 1:brands) {
				predictors <- paste(brand_names[p], c("Loyalty", "LBP", "Price", "Pricecut"), sep = "_")
				utility[index,p] <- (pars[["Intercept"]] + pars[["Brand_Intercept"]][p] + mat[index, predictors] %*% pars[["Covariates"]])
			}
		}
		cv <- rowSums(exp(utility))
		logprob_brandchoice <- utility - log(cv)
		loglik <- rowSums(logprob_brandchoice * mat[, brand_names])
		data <- data.frame(data, CV = cv, LL_BCH = loglik)
		if (return.model) return(data)
		return(-sum(loglik))
	}
	return(likelihood)
}
# 
# res <- optim(rep(0, 11), fn = brandChoiceModel(kaffee), return.model=FALSE, control = list(maxit = 1e3))
# 
# tmp <- brandChoiceModel(kaffee)
# tmp(rep(0, 11))
