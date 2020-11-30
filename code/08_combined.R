
reorganizePars.combined <- function(pars) {
	return(list( BrandChoice = pars[     1:10],
							 Incidence   = pars[10 + 1:4],
							 Quantity    = pars[14 + 1:11] ))
}

jointModel <- function(model) {
	
	# Convert input data.frame to base_model environment ----
	if (is.data.frame(model)) model <- base_model(model)
	
	# Initialize sub-models ----
	brandChoice <- brandChoiceModel(model)
	incidence <- incidenceModel(model)
	quantity <- quantityModel(model)
	
	
	likelihood <- function(pars, segment = 1) {
		
		# Reorganize parameters to allow for easy access ----
		pars <- reorganizePars.combined(pars)
		
		# Run computations for brand choice model
		LL_BCH <- brandChoice(pars[["BrandChoice"]], segment)
		
		# Run computations for incidence model ---- 
		LL_INC <- incidence(pars[["Incidence"]], segment)
		
		# Run computations for quantity model ----
		LL_QTY <- quantity(pars[["Quantity"]], segment)
		
		# Compute log-likelihood for household h on day t ----
		loglik <- model$BrandChoice[[segment]]$LogLik + 
			        model$Incidence[[segment]]$LogLik   + 
			        model$Quantity[[segment]]$LogLik
		
		# Store results and optionally return model log-likelihood ----
		model$Conjoint[[segment]] <- list(LogLik = loglik) 
		
		LL <- sum(loglik)
		if (is.nan(LL)) stop("Combined model loglikelihood is nan")
		if (is.infinite(LL)) LL <- -1e100
		return(-LL)
	}
	return(likelihood)
}
