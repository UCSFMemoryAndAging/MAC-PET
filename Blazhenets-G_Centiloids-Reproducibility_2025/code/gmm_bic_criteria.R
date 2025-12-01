## BIC criteria with fitdistr for k = 1 ------
find_best_gmm_bic <- function(data, k_range = 1:5, maxit = 5000) {
  results <- list()
  bic_values <- rep(Inf, length(k_range))
  
  # Loop over the range of k values
  for (k in k_range) {
    if (k == 1) {
      # Special handling for k = 1 using fitdistr
      result <- tryCatch({
        # Fit a single Gaussian using fitdistr
        fit <- fitdistr(data$cl, densfun = "normal")
        loglik <- sum(dnorm(data$cl, mean = fit$estimate['mean'], sd = fit$estimate['sd'], log = TRUE))
        
        # Create a GMM-like structure for consistency
        gmm <- list(mu = fit$estimate['mean'], sigma = fit$estimate['sd'], loglik = loglik)
        
        # Calculate BIC for k = 1
        bic <- calculate_bic(gmm, data$cl)
        list(gmm = gmm, bic = bic)
      }, error = function(e) {
        list(gmm = NULL, bic = Inf)
      })
    } else {
      # Fit the GMM for k > 1
      result <- tryCatch({
        gmm <- normalmixEM(data$cl, k = k, ECM = TRUE, maxit = maxit, fast = FALSE, arbmean = TRUE, arbvar = TRUE)
        bic <- calculate_bic(gmm, data$cl)
        list(gmm = gmm, bic = bic)
      }, error = function(e) {
        list(gmm = NULL, bic = Inf)
      })
    }
    
    # Store the BIC value
    bic_values[k] <- result$bic
    results[[k]] <- result$gmm  # Optionally store the model
  }
  
  best_k <- which.min(bic_values)
  list(best_k = best_k, bic_values = bic_values, models = results)
}

# Define a custom BIC function for mixEM objects----------
calculate_bic <- function(model, data) {
  n <- length(data)
  k <- length(model$lambda)
  
  # For k = 1, there is no lambda parameter
  if (is.null(k)) {
    k <- 1
  }
  
  log_likelihood <- model$loglik
  
  # Correct parameter count
  p <- k + k + (k - 1)  # means + variances + mixture weights (weights only for k > 1)
  
  # Calculate BIC
  bic <- -2 * log_likelihood + log(n) * p
  return(bic)
}
