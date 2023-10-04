
## Functions to calculate the absolute standardized difference analogous to the functions in the package stddiff), 
## but allowing for frequency weights.
## Rutger van den Bor (r.m.vandenbor@umcutrecht.nl) - 2022-04-07

## Description: 
## The functions below can be used to calculate the absolute standardized difference for comparing the distribution
## of continuous/numeric, binary, and categorical variables between two groups.
##
## The three functions are based on stddiff.numeric(), stddiff.binary() and stddiff.category() from the package stddiff,
## but are adjusted to allow for weights. 
##
## Note: other differences are
## - the specification of the input parameters (which I believe now is more direct and less error prone, because the stddiff functions 
##   require the user to specify the position (column number), which may lead to errors), 
## - more checks on the data + informative warnings, 
## - the output does not give a CI for the ASD, as we will not be needing it
## - the output also only gives the ASD, not e.g. the proportions or means/sds.
## - also note that the function does not accept NA values, so these have to be manually removed if needed
##   (this is done on purpose to more easily detect issues in the data).

	## ASD for numeric values, allowing for weights:
	ASD.numeric <- function(group, obs, weights = NULL){ 	## Note: takes the vectors as input directly. Less error-prone but make sure the order is the same

		## If not specified, set all weights equal to 1
		if(is.null(weights)) weights <- rep(1, length(group))

		## Input checks
		if(length(group) != length(obs) | length(obs) != length(weights)){
			warning('vectors of group, obs and weight (if applicable) should have the same length. NA returned')
			return(NA)			
			}
		if(any(is.na(group) | is.na(obs) | is.na(weights))){
			warning('NA values are not allowed. NA returned')
			return(NA)
			}
		if(length(unique(group)) != 2){
			warning('group should have two unique values. NA returned')
			return(NA)
			}
		obs <- as.numeric(obs)
		if(any(is.na(obs))){
			warning('Problem with transforming obs to class numeric. NA returned.')
			return(NA)
			}

		## Internal function for weighted sd (with frequency weights)
		weighted.sd <- function(x, w = NULL){
			if(is.null(w)) w <- rep(1, length(x))
			if(any(is.na(x) | is.na(w))){
				warning('NA values not allowed, NA returned')
				return(NA)
				}
			return(sqrt(sum(w * (x - weighted.mean(x, w))^2) / (sum(w) - 1)))
			}
	
		## Calculate weighted means and SD per group
		m <- as.numeric(sapply(sort(unique(group)), FUN = function(i) weighted.mean(obs[group == i], w = weights[group == i])))
		s <- as.numeric(sapply(sort(unique(group)), FUN = function(i) weighted.sd(obs[group == i], w = weights[group == i])))

		## calculate ASD
		ASD <- base::abs(m[2] - m[1]) / sqrt((s[2]^2 + s[1]^2) / 2)

		## return
		return(ASD)
		}

	## ASD for binary values, allowing for weights:
	ASD.binary <- function(group, obs, weights = NULL){ 	## Note: takes the vectors as input directly. Less error-prone but make sure the order is the same

		## If not specified, set all weights equal to 1
		if(is.null(weights)) weights <- rep(1, length(group))

		## Input checks
		if(length(group) != length(obs) | length(obs) != length(weights)){
			warning('vectors of group, obs and weight (if applicable) should have the same length. NA returned')
			return(NA)			
			}
		if(any(is.na(group) | is.na(obs) | is.na(weights))){
			warning('NA values are not allowed. NA returned')
			return(NA)
			}
		if(length(unique(group)) != 2){
			warning('group should have two unique values. NA returned')
			return(NA)
			}
		if(length(unique(obs)) != 2){
			warning('obs should have at exactly two unique values. NA returned')
			return(NA)
			}

		## Calculate weighted proportion per group
		obs.values <- sort(unique(obs))
		p <- as.numeric(sapply(sort(unique(group)), FUN = function(i) weighted.mean(obs[group == i] == obs.values[1], w = weights[group == i])))

		## Check for prob. of 0 vs 1
		if((p[1] == 0 & p[2] == 1) | (p[1] == 1 & p[2] == 0)){
			warning('probabilities equal to 0 in one group and 1 in the other. NA returned')
			return(NA)
			}

		## Calculate ASD
      	ASD <- base::abs(p[1] - p[2]) / sqrt((p[1] * (1 - p[1]) + p[2] * (1 - p[2])) / 2)

		## return
		return(ASD)
		}


	## ASD for categorical values, allowing for weights:
	ASD.category <- function(group, obs, weights = NULL){ 	## Note: takes the vectors as input directly. Less error-prone but make sure the order is the same

		## If not specified, set all weights equal to 1
		if(is.null(weights)) weights <- rep(1, length(group))

		## Input checks
		if(length(group) != length(obs) | length(obs) != length(weights)){
			warning('vectors of group, obs and weight (if applicable) should have the same length. NA returned')
			return(NA)			
			}
		if(any(is.na(group) | is.na(obs) | is.na(weights))){
			warning('NA values are not allowed. NA returned')
			return(NA)
			}
		if(length(unique(group)) != 2){
			warning('group should have two unique values. NA returned')
			return(NA)
			}
		if(length(unique(obs)) == 1){
			warning('obs should have at least two unique values. NA returned')
			return(NA)
			}

		## Calculate weighted proportions per group
		obs.values <- sort(unique(obs))
		prop <- lapply(unique(obs.values), FUN = function(j) as.numeric(sapply(sort(unique(group)), FUN = function(i) weighted.mean(obs[group == i] == j, w = weights[group == i]))))			
		prop <- cbind(obs.values, as.data.frame(do.call(rbind, prop)))
		colnames(prop)[2:3] <- c('p1', 'p2')

		## Check for probabilities of 0 vs 1
		if(any((prop$p1 == 0 & prop$p2 == 1) | prop$p1 == 1 & prop$p2 == 0)){
			warning('probabilities equal to 0 in one group and 1 in the other. NA returned')
			return(NA)
			}

		## Calculate ASD (following the function in stddiff)
		t. <- prop$p2[-1]
		c. <- prop$p1[-1]
		k. <- length(obs.values) - 1
		l. <- k.
		s. <- matrix(rep(0, k. * l.), ncol = k.)
		for (ii in 1:k.) {
			for (j in 1:l.) {
				if (ii == j) s.[ii, j] <- 0.5 * (t.[ii] * (1 - t.[ii]) + c.[ii] * (1 - c.[ii]))
				if (ii != j) s.[ii, j] <- -0.5 * (t.[ii] * t.[j] + c.[ii] * c.[j])
				}}
		e. <- rep(1, k.)
		e. <- diag(e.)
		s. <- solve(s., e.)
		tc1 <- t. - c.
		tc2 <- t. - c.
		ASD <- as.numeric(sqrt(t(tc1) %*% s. %*% tc2))

		## Return output
		#return(list(prop, ASD))
		return(ASD)
		}	





	