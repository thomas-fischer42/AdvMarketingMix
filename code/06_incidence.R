
reorganizePars.incidence <- function(pars) {
	return(list( Intercept = pars[1],
							 CR        = pars[2],
							 Inventory = pars[3],
							 CV        = pars[4] ))
}

incidenceModel <- function(model) {
	
	# Convert input data.frame to base_model environment ----
	if (is.data.frame(model)) model <- base_model(model)
	
	# Pre-initialize vectors ----
	linpred  <- numeric(rows)
	logprob  <- numeric(rows)
	loglik   <- numeric(rows)
	
	likelihood <- function(pars, segment = 1) {
		
		# Reorganize parameters to allow for easy access ----
		pars <- reorganizePars.incidence(pars)
		
		# Initialize missing category value to log(#(brands)) ----
		if (!("CV" %in% names(model$data))) {
			warning("Variable CV not found. Initialized with log(N_brands)")
			model$data$CV[[segment]] <- rep(log(brands), rows)
		}
		
		# Compute linear predictor ----
		linpred <- pars[["Intercept"]]                            +   # Global intercept
			         pars[["CR"]]        * model$data$CR            +   # Consumption rate of household h (currently constant)
			         pars[["Inventory"]] * model$data$INVENTORY     +   # Inventory of household h on day t
			         pars[["CV"]]        * model$data$CV[[segment]]     # Category value of household h on day t
		
		# Compute log(Pr) of household h making purchase on day t ----
		logprob <- log(expit(linpred)) 
		
		# Compute log-likelihood of household h making purchase on day t ----
		loglik <- dbinom(rowSums(model$data$BRAND), 1, exp(logprob), log = TRUE)
		
		# Store results and return model log-likelihood ----
		model$Incidence[[segment]] <- list(LogLik = loglik,
																			 LogProb = logprob)

		LL <- sum(loglik)
		if (is.nan(LL)) stop("Incidence model yielded NAN loglikelihood")
		if (is.infinite(LL)) LL <- -1e100
		return(-LL)
	}
	return(likelihood)
}

