
reorganizePars.segments <- function(pars, segments) {
	if (length(pars) %% segments != 0) {
		stop("Number of parameters does not match number of segments")
	}
	pars <- matrix(pars, nrow = segments, byrow = TRUE)
	
	# Convert segment parameters into segment sizes ----
	pars[,1] <- exp(pars[,1]) / (sum(exp(pars[,1])))
	return(pars)
}


jointModelSegments <- function(model, segments = 1) {
	
	# Convert input data.frame to base_model environment ----
	if (is.data.frame(model)) model <- base_model(model)
	
	# Initialize full model for single segment case ----
	joint_model <- jointModel(model)
	
	# Initialize vectors ----
	loglik_by_seg <- matrix(NA, nrow = rows, ncol = segments)
	loglik        <- numeric(segments)
	
	likelihood <- function(pars) {
		
		# Reorganize parameters to allow for easy access ----
		pars <- reorganizePars.segments(pars, segments)
		
		# Compute log-likelihood for household h on day t in segment s ----
		loglik_by_seg <- sapply(1:segments, function(s) {
			joint_model(pars[s,-1], segment = s)
			return(log(pars[s, 1]) + model$Conjoint[[s]]$LogLik)
		})
		
		# Compute log-likelihood for household h on day t in any segment ----
		loglik <- rowSums(loglik_by_seg, na.rm = TRUE)
		
		# Segment membership ----
		w_h <- lapply(split(as.data.frame(loglik_by_seg), model$data$ID), function(x) {
			colSums(x) / sum(x)})
		
		membership <- do.call("rbind", w_h)
		
		# Store results and optionally return model log-likelihood ----
		model$Segmentation <- list( LogLik = loglik,
																Membership = membership )

		LL <- sum(loglik)
		if (is.infinite(LL)) LL <- -1e100
		if (is.nan(LL)) stop("NAN loglikelihood for segment model")
		return(-LL)
	}
	return(likelihood)
}
 
