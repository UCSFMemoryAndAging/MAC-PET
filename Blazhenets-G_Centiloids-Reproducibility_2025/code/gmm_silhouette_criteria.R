## Silhouette score criteria with fitdistr for k = 1 ----------
find_best_gmm_silhouette <- function(data, k_range = 1:5, maxit = 5000) {
  # Initialize an empty list to store the results
  results <- list()
  
  # Initialize an empty vector to store the silhouette scores
  silhouette_scores <- rep(NA, length(k_range))  # Use NA for clarity
  
  for (k in k_range) {
    if (k == 1) {
      # Special handling for k = 1 using fitdistr
      result <- tryCatch({
        # Fit a single Gaussian using fitdistr
        fit <- MASS::fitdistr(data$cl, densfun = "normal")
        
        # For k = 1, all points are in one cluster, silhouette score is not defined
        silhouette_score <- NA  # Silhouette score doesn't apply to single cluster
        
        # Create a GMM-like structure for consistency
        gmm <- list(mu = fit$estimate['mean'], sigma = fit$estimate['sd'], loglik = sum(dnorm(data$cl, mean = fit$estimate['mean'], sd = fit$estimate['sd'], log = TRUE)))
        
        list(gmm = gmm, silhouette_score = silhouette_score)
      }, error = function(e) {
        list(gmm = NULL, silhouette_score = NA)
      })
    } else {
      # Fit the GMM for k > 1
      result <- tryCatch({
        gmm <- normalmixEM(data$cl, k = k, ECM = TRUE, maxit = maxit, fast = FALSE, arbmean = TRUE, arbvar = TRUE)
        silhouette_score <- calculate_silhouette(gmm, data$cl)
        list(gmm = gmm, silhouette_score = silhouette_score)
      }, error = function(e) {
        list(gmm = NULL, silhouette_score = NA)
      })
    }
    
    # Store the silhouette score
    silhouette_scores[k] <- result$silhouette_score  # Adjust index for storing scores
    results[[k]] <- result$gmm  # Optionally store the model
  }
  
  # Find the k with the maximum silhouette score
  best_k <- k_range[which.max(silhouette_scores)]
  
  # Return the best k and the corresponding model
  list(best_k = best_k, silhouette_scores = silhouette_scores, models = results)
}

# Define a custom silhouette score function for mixEM objects --------
calculate_silhouette <- function(model, data) {
  # Assign data points to the component with the highest posterior probability
  cluster_assignments <- apply(model$posterior, 1, which.max)
  
  # Calculate silhouette score
  dist_matrix <- dist(data)  # Calculate the distance matrix
  silhouette_score <- silhouette(cluster_assignments, dist_matrix)
  
  # Return the mean silhouette score
  mean(silhouette_score[, 3], na.rm = TRUE)  # Calculate mean silhouette score
}
