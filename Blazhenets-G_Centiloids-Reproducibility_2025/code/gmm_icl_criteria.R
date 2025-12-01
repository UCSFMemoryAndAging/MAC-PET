## ICL criteria with fitdistr for k = 1 ----
find_best_gmm_icl <- function(data, k_range = 1:5, maxit = 5000) {
  # Initialize an empty list to store the results
  results <- list()
  
  # Initialize an empty vector to store the ICL values
  icl_values <- rep(Inf, length(k_range))
  
  # Loop over the range of k values
  for (k in k_range) {
    if (k == 1) {
      # Special handling for k = 1 using fitdistr
      result <- tryCatch({
        # Fit a single Gaussian using fitdistr
        fit <- MASS::fitdistr(data$cl, densfun = "normal")
        loglik <- sum(dnorm(data$cl, mean = fit$estimate['mean'], sd = fit$estimate['sd'], log = TRUE))
        
        # Create a GMM-like structure for consistency
        gmm <- list(mu = fit$estimate['mean'], sigma = fit$estimate['sd'], loglik = loglik)
        
        # Calculate ICL for k = 1
        icl <- calculate_icl(gmm, data$cl, k = 1)
        list(gmm = gmm, icl = icl)
      }, error = function(e) {
        list(gmm = NULL, icl = Inf)
      })
    } else {
      # Fit the GMM for k > 1
      result <- tryCatch({
        gmm <- normalmixEM(data$cl, k = k, ECM = TRUE, maxit = maxit, fast = FALSE, arbmean = TRUE, arbvar = TRUE)
        icl <- calculate_icl(gmm, data$cl, k = k)
        list(gmm = gmm, icl = icl)
      }, error = function(e) {
        list(gmm = NULL, icl = Inf)
      })
    }
    
    # Store the ICL value
    icl_values[k] <- result$icl
    results[[k]] <- result$gmm  # Optionally store the model
  }
  
  # Find the k with the minimum ICL value
  best_k <- which.min(icl_values)
  list(best_k = best_k, icl_values = icl_values, models = results)
}

# Define a custom ICL function for mixEM objects
calculate_icl <- function(model, data, k) {
  n <- length(data)
  
  # For k = 1, there are no mixture weights
  if (k == 1) {
    log_likelihood <- model$loglik
    p <- 2  # mean + variance for k = 1
    bic <- -2 * log_likelihood + log(n) * p
    entropy <- 0  # No posterior probabilities for k = 1
  } else {
    log_likelihood <- model$loglik
    p <- k + k + (k - 1)  # means + variances + mixture weights for k > 1
    bic <- -2 * log_likelihood + log(n) * p
    
    # Calculate entropy
    posterior <- model$posterior
    entropy <- -sum(posterior * log(posterior + .Machine$double.eps), na.rm = TRUE)  # Avoid log(0)
  }
  
  # Calculate ICL
  icl <- bic + entropy
  return(icl)
}
