
reorganizePars.combined <- function(pars) {
	parlist <- list(
		BrandChoice = pars[     1:11],
		Incidence   = pars[11 + 1:4],
		Quantity    = pars[15 + 1:12]
	)
	return(parlist)
}

jointModel <- function(data) {
	
	likelihood <- function(pars, return.model = FALSE) {
		
		pars <- reorganizePars.combined(pars)
		
		brandChoice <- brandChoiceModel(data)
		data <- brandChoice(pars[["BrandChoice"]])
		incidence <- incidenceModel(data)
		data <- incidence(pars[["Incidence"]])
		quantity <- quantityModel(data)
		data <- quantity(pars[["Quantity"]])
		
		loglik <- sum(data[, c("LL_BCH", "LL_PUR", "LL_QTY")])
		
		if (return.model) return(data)
		return(-loglik)
	}
	return(likelihood)
}

# res <- optim(rep(0, 27), jointModel(kaffee), return.model = FALSE, control = list(maxit = 2e2))
# res

