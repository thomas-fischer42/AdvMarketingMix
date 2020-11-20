
reorganizePars.combined <- function(pars) {
	parlist <- list(
		BrandChoice = pars[     1:11],
		Incidence   = pars[11 + 1:4],
		Quantity    = pars[15 + 1:12]
	)
	return(parlist)
}

jointModel <- function(data) {
	
	likelihood <- function(pars, return.data = FALSE) {
		
		pars <- reorganizePars.combined(pars)
		
		brandChoice <- brandChoiceModel(data)
		data <- brandChoice(pars[["BrandChoice"]])
		incidence <- incidenceModel(data)
		data <- incidence(pars[["Incidence"]])
		quantity <- quantityModel(data)
		data <- quantity(pars[["Quantity"]])
		
		loglik <- rowSums(data[, c("LL_BCH", "LL_PUR", "LL_QTY")])
		LL <- sum(loglik)

		if (is.infinite(LL)) LL <- -1e100
		
		if (return.data) {
			return(data)
		}
		return(-LL)
	}
	return(likelihood)
}

