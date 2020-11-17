
reorganizePars.incidence <- function(pars) {
	intercept <- pars[1]
	covariates <- pars[2:4]
	return(list(Intercept = intercept, Covariates = covariates))
}

incidenceModel <- function(data) {
	
	likelihood <- function(pars, return.model=TRUE) {
		
		mat <- as.matrix(data)
		pars <- reorganizePars.incidence(pars)
		
		for (id in unique_id) {
			index <- which(mat[,"ID"] == id)
			
			predictors <- c("CR", "Inventory", "CV")
			linpred <- pars[["Intercept"]] + mat[index, predictors] %*% pars[["Covariates"]]
			logprob_purchase[index] <- -log(1 + exp(-linpred))
		}
		loglik <- logprob_purchase * rowSums(mat[, brand_names]) + log(1-exp(logprob_purchase)) * (1-rowSums(mat[, brand_names]))
		data <- data.frame(data, LL_PUR = loglik)
		
		if (return.model) return(data)
		return(-sum(loglik))
	}
	return(likelihood)
}
