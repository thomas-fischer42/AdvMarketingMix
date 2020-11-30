
reorganizePars.quantity <- function(pars) {
	return(list( Intercept        = pars[1],
							 Brand_Intercepts = pars[2:brands],
							 PR               = pars[brands+1],
							 Inventory        = pars[brands+2],
							 Loyalty          = pars[brands+3],
							 Price            = pars[brands+4]))
}

quantityModel <- function(model) {
	
	# Convert input data.frame to base_model environment ----
	if (is.data.frame(model)) model <- base_model(model)
	
	# Pre-initialize matrices and vectors ----
	linpred  <- matrix(0, nrow = rows, ncol = brands)
	lambda   <- matrix(0, nrow = rows, ncol = brands)
	logprob  <- matrix(0, nrow = rows, ncol = brands)
	loglik   <- numeric(rows)
	
	
	likelihood <- function(pars, segment = 1) {
		
		# Reorganize parameters to allow for easy access ----
		pars <- reorganizePars.quantity(pars)
		
		# Compute linear predictor and lambda ----
		linpred <- pars[["Intercept"]]                         + 
			         pars[["Brand_Intercepts"]]                  + 
			         pars[["PR"]]        * model$data$PR         + 
			         pars[["Inventory"]] * model$data$INVENTORY  + 
			         pars[["Loyalty"]]   * model$data$LOYALTY    + 
			         pars[["Price"]]     * model$data$PRICE
		
		lambda <- exp(linpred)
		
		# Compute log(Pr) of household h buying q units of brand b on day t, given that q>0 ----
		#   Note that there may be issues with numerical stability, especially 
		#   when lambda becomes large. To counteract these problems we directly calculate
		#   log probabilities
		
		logprob <- dtruncpois(model$data$QTY, lambda, .log = TRUE)
		
		# Compute log-likelihood of household buying q units of any brand on day t, given that q>0 ----
		#   Because log(0)*0 is NaN, we set na.rm = TRUE in our call to rowSums()
		
		loglik <- rowSums(logprob * model$data$BRAND, na.rm = TRUE)
		
		# Store results and return model log-likelihood ----
		model$Quantity[[segment]] <- list(LogLik = loglik,
																			LogProb = logprob,
																			Lambda = lambda)
		
		LL <- sum(loglik)
		if (is.nan(LL)) stop("Quantity model yielded NAN loglikelihood")
		if (is.infinite(LL)) LL <- -1e100
		return(-LL)
	}
	return(likelihood)
}

