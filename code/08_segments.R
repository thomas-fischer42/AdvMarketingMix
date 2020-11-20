

reorganizePars.segments <- function(pars, segments) {
	pars <- matrix(pars, nrow = segments, byrow = TRUE)
	pars[,1] <- exp(pars[,1]) / (sum(exp(pars[,1])))
	return(pars)
}

jointModelSegments <- function(data, segments = 1) {
	
	likelihood <- function(pars, return.data = FALSE) {
		
		pars <- reorganizePars.segments(pars, segments)
		
		# Initialize model for single segment case
		joint_model <- jointModel(data)
		
		segment_results <- lapply(1:segments, function(s) {
			sdata <- joint_model(pars[s,-1], return.data = TRUE)
			return(pars[s, 1] * exp(sdata[, c("LL_BCH", "LL_PUR", "LL_QTY")]))
		})
		
		loglik <- rowSums(log(Reduce("+", segment_results)))
		
		LL <- sum(loglik)
		if (is.infinite(LL)) LL <- -1e100
		if (is.nan(LL)) stop("NAN loglikelihood for segment model")
		if (return.data) {
			return(data)
		}
		
		return(-LL)
	}
	return(likelihood)
}

