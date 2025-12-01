## Loglik criteria ------
find_best_gmm_loglik <- function(data, k_range = 1:5, maxit = 5000) {
  results <- list()  # List to store fitted models
  loglik_values <- rep(NA, length(k_range))  # Initialize a vector to store log-likelihood values
  
  # Loop over the range of k values
  for (k in k_range) {
    result <- tryCatch({
      if (k == 1) {
        # For k = 1, we fit a single Gaussian using a different approach
        single_gaussian <- fitdistr(data$cl, "normal")  # Fit a single normal distribution
        loglik <- single_gaussian$loglik  # Extract log-likelihood
        list(gmm = single_gaussian, loglik = loglik)
      } else {
        # For k > 1, we fit a Gaussian Mixture Model (GMM)
        gmm <- normalmixEM(data$cl, k = k, ECM = TRUE, maxit = maxit, fast = FALSE, arbmean = TRUE, arbvar = TRUE)
        loglik <- gmm$loglik  # Extract log-likelihood
        list(gmm = gmm, loglik = loglik)
      }
    }, error = function(e) {
      # Handle the error by returning NULL
      list(gmm = NULL, loglik = NA)  # Return NA for log-likelihood if an error occurs
    })
    
    # Store the fitted model and log-likelihood
    results[[k]] <- result$gmm  # Store the fitted model
    loglik_values[k] <- result$loglik  # Store the log-likelihood
  }
  
  # Find the best k based on non-NA log-likelihood values
  best_k <- which.max(loglik_values)  # Skip NA values
  
  # Return the best k, log-likelihood values, and fitted models
  list(best_k = best_k, loglik_values = loglik_values, models = results)
}
