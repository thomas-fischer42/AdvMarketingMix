

reorganizePars.segments <- function(pars) {
	pars <- matrix(pars, nrow = segments, byrow = TRUE)
	pars[,1] <- exp(pars[,1]) / (sum(exp(pars[,1])))
	return(pars)
}

jointModelSegments <- function(data, segments = 1) {
	
	likelihood <- function(pars, return.model = FALSE) {
		
		pars <- reorganizePars.segments(pars)
		
		# Initialize model for single segment case
		joint_model <- jointModel(data)
		
		segment_results <- lapply(1:segments, function(s) {
			sdata <- joint_model(pars[s,-1], return.model = TRUE)
			return(pars[s, 1] * exp(sdata[, c("LL_BCH", "LL_PUR", "LL_QTY")]))
		})
		
		LL <- sum(log(Reduce("+", segment_results)))
		if (is.infinite(LL)) LL <- -1e100
		
		if (return.model) return(data)
		return(-LL)
	}
	return(likelihood)
}


initPars <- function(data, segments) {
	
	bch <- brandChoiceModel(data)
	bch_res <- optim(rep(0, 11), bch, return.model = FALSE)
	data <- bch(bch_res$par, return.model = TRUE)
	
	inc <- incidenceModel(data)
	inc_res <- optim(rep(0, 4), inc, return.model = FALSE)
	data <- inc(inc_res$par, return.model = TRUE)
	
	qty <- quantityModel(data)
	qty_res <- optim(rep(0, 12), qty, return.model = FALSE)
	data <- qty(qty_res$par, return.model = TRUE)
	
	return(rep(c(0, bch_res$par, inc_res$par, qty_res$par), segments))
}

# set.seed(1234)
# segments <- 3
# par_init <- initPars(kaffee, segments)
# res <- optim(par_init + rnorm(28*segments, 0, 0.1), jointModelSegments(kaffee, segments), return.model = FALSE, control = list(maxit = 5e2))
# res
# 
# apply(reorganizePars.segments(res$par), 1, reorganizePars.combined)


