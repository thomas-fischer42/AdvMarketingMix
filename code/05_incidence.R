
reorganizePars.incidence <- function(pars) {
	intercept <- pars[1]
	covariates <- pars[2:4]
	return(list(Intercept = intercept, Covariates = covariates))
}

incidenceModel <- function(data) {
	
	unique_id <- unique(kaffee2$ID)
	# Pre-initialize containers ----
	linpred             <- numeric(rows)
	logprob_purchase    <- numeric(rows)
	loglik              <- numeric(rows)
	

	likelihood <- function(pars, return.data = TRUE) {
		
		mat <- as.matrix(data)
		pars <- reorganizePars.incidence(pars)
		
		for (id in unique_id) {
			index <- which(mat[,"ID"] == id)
			predictors <- c("CR", "Inventory", "CV")
			linpred[index] <- pars[["Intercept"]] + mat[index, predictors] %*% pars[["Covariates"]]
		}
		
		logprob_purchase <- log(expit(linpred))
		loglik <- dbinom(rowSums(mat[, brand_names]), 1, exp(logprob_purchase), log = TRUE)
		LL <- sum(loglik)

		if (return.data) {
			data <- data.frame(data, LL_PUR = loglik)
			return(data)
		}
		
		if (is.nan(LL)) stop("Incidence model yielded NAN loglikelihood")
		if (is.infinite(LL)) LL <- -1e100
		return(-LL)
	}
	return(likelihood)
}
