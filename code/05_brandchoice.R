
reorganizePars.brandchoice <- function(pars) {
	return(list( Intercept        = pars[1],
							 Brand_Intercepts = c(0, pars[2:brands]),
							 Loyalty          = pars[brands+1],
							 LBP              = pars[brands+2],
							 Price            = pars[brands+3] ))
}


brandChoiceModel <- function(model) {
	
	# Convert input data.frame to base_model environment ----
	if (is.data.frame(model)) model <- base_model(model)
	
	# Pre-initialize matrices and vectors ----
	utility   <- matrix(NA, nrow = rows, ncol = brands)
	logprob   <- matrix(NA, nrow = rows, ncol = brands)
	cv        <- numeric(rows)
	loglik    <- numeric(rows)
	
	
	# Specification of model likelihood ----
	likelihood <- function(pars, segment = 1) {
		
		# Reorganize parameters to allow for easy access ----
		pars <- reorganizePars.brandchoice(pars)
		
		# Compute utility ----
		utility <- pars[["Intercept"]]                    +   # Global intercept
							 pars[["Brand_Intercepts"]]             +   # Intercepts for brands 2:7
							 pars[["Loyalty"]] * model$data$LOYALTY +   # Loyalty of household h to brand b
							 pars[["LBP"]]     * model$data$LBP     +   # Household h purchased brand b on day t[i-1]
							 pars[["Price"]]   * model$data$PRICE       # Price of brand b in store s on day t
		
		# Compute category value ----
		# Note that unavailable brands should not contribute to CV
		cv <- log(rowSums(exp(utility) * (model$data$PRICE > 0)))
		
		# Compute log(Pr) of household h purchasing brand b on day t ----
		#   Note that probability of purchasing unavailable brand must
		#   be zero. The following corresponds to
		# 
		#   log(exp(utility) / exp(cv) * (model$PRICE > 0))

		logprob <- (utility + log(model$data$PRICE > 0) - cv)

		# Compute log-likelihood of household h buying any brand on day t ----
		#   Due to the aforementioned problems, logpr_bc is sometimes infinite.
		#   This creates a computational issue because R is set up to return
		#   NaN if zero is multiplied by infinity. To address this problem, we
		#   set na.rm = TRUE in rowSums
		
		loglik <- rowSums(logprob * model$data$BRAND, na.rm = TRUE)
		
		# Store results and optionally return model loglikelihood ----
		model$data$CV[[segment]]     <- cv
		model$BrandChoice[[segment]] <- list( LogLik = loglik,
																					LogProb = logprob,
																					Utility = utility )
		
		LL <- sum(loglik)
		if (is.nan(LL)) stop("Brandchoice model yielded NAN loglikelihood")
		if (is.infinite(LL)) LL <- -1e100
		return(-LL)
	}
	return(likelihood)
}
