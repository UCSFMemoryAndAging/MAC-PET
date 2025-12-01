## AIC criteria with fitdistr for k = 1 ------
find_best_gmm_aic <- function(data, k_range = 1:5, maxit = 5000) {
  results <- list()
  aic_values <- rep(Inf, length(k_range))
  
  # Loop over the range of k values
  for (k in k_range) {
    if (k == 1) {
      # Special handling for k = 1 using fitdistr
      result <- tryCatch({
        # Fit a single Gaussian using fitdistr
        fit <- MASS::fitdistr(data$cl, densfun = "normal")
        
        # Calculate the log-likelihood for the fit
        log_likelihood <- sum(dnorm(data$cl, mean = fit$estimate['mean'], sd = fit$estimate['sd'], log = TRUE))
        
        # For k = 1, the number of parameters is 2 (mean and variance)
        p <- 2
        aic <- -2 * log_likelihood + 2 * p
        
        # Create a GMM-like structure for consistency
        gmm <- list(mu = fit$estimate['mean'], sigma = fit$estimate['sd'], loglik = log_likelihood)
        
        list(gmm = gmm, aic = aic)
      }, error = function(e) {
        list(gmm = NULL, aic = Inf)
      })
    } else {
      # Fit the GMM for k > 1
      result <- tryCatch({
        gmm <- normalmixEM(data$cl, k = k, ECM = TRUE, maxit = maxit, fast = FALSE, arbmean = TRUE, arbvar = TRUE, verb=FALSE)
        aic <- calculate_aic(gmm, data$cl)
        list(gmm = gmm, aic = aic)
      }, error = function(e) {
        list(gmm = NULL, aic = Inf)
      })
    }
    
    # Store the AIC value
    aic_values[k] <- result$aic
    results[[k]] <- result$gmm  # Optionally store the model
  }
  
  # Find the k with the minimum AIC value
  best_k <- which.min(aic_values)
  list(best_k = best_k, aic_values = aic_values, models = results)
}

# Define a custom AIC function for mixEM objects ----------
calculate_aic <- function(model, data) {
  n <- length(data)
  k <- length(model$lambda)
  log_likelihood <- model$loglik
  
  # Correct parameter count
  p <- k + k + (k - 1)  # means + variances + mixture weights
  
  # Calculate AIC
  aic <- -2 * log_likelihood + 2 * p
  return(aic)
}
