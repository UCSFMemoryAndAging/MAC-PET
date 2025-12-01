# Function to fit GMM, order components, and create summary and density data frames
fit_gmm <- function(data) {
  gmm <- normalmixEM(data, k = 2, ECM=T, maxit=5000, fast=F, verb=F)
  order_index <- order(gmm$mu)
  gmm$lambda <- gmm$lambda[order_index]
  gmm$mu <- gmm$mu[order_index]
  gmm$sigma <- gmm$sigma[order_index]
  gmm$posterior <- gmm$posterior[, order_index]
  summary_df <- data.frame(data = data, 
                           posterior1 = gmm$posterior[, 1], 
                           posterior2 = gmm$posterior[, 2])
  dens_df <- data.frame(x = seq(min(data), max(data), length.out = 1000))
  dens_df$comp1 <- gmm$lambda[1] * dnorm(dens_df$x, mean = gmm$mu[1], sd = gmm$sigma[1])
  dens_df$comp2 <- gmm$lambda[2] * dnorm(dens_df$x, mean = gmm$mu[2], sd = gmm$sigma[2])
  dens_df$total <- dens_df$comp1 + dens_df$comp2
  gmm$cutoff<-gmm$mu[1]+2*gmm$sigma[1]
  
  # Double cutoff approach with posterior distribution cutoff of 50%/60%/70%/80%/90% prob
  cutoff_values<-c('0.5', '0.6', '0.7', '0.8', '0.9')
  xout<-seq(from=-50, to=250, by = 0.01)
  
  # Approximate the posterior prob with spline to find the intersection with the cutoffs
  # Posterior is the continuous function, but the exact values might be missing in the cohort
  posterior1_spline<-splinefun(x = summary_df$data,y = summary_df$posterior1)
  posterior1_interp <- posterior1_spline(xout)
  
  posterior2_spline<-splinefun(x = summary_df$data,y = summary_df$posterior2)
  posterior2_interp <- posterior2_spline(xout)
  
  ## Confidently negative cutoff
  cutoff_double_low<-list()
  prob_double_low<-list()
  for (val in cutoff_values) { 
    above_idx <- which(posterior1_interp > as.numeric(val) & xout > gmm$mu[1]) #assumption: on the right side of the curve and crosses the threshold value
    if (length(above_idx) > 0) {
      closest_idx <- above_idx[which.min(posterior1_interp[above_idx] - as.numeric(val))]
      cutoff_double_low[[val]] <- xout[closest_idx]
      prob_double_low[[val]] <- posterior1_interp[closest_idx]
    } else {
      # fallback to closest to cutoff value regardless of being above or positive
      fallback_idx <- which.min(abs(posterior1_interp - as.numeric(val)))
      cutoff_double_low[[val]] <- xout[fallback_idx]
      prob_double_low[[val]] <- posterior1_interp[fallback_idx]
    }
  }
  
  ## Confidently positive cutoff
  cutoff_double_high<-list()
  prob_double_high<-list()
  for (val in cutoff_values) { 
    above_idx <- which(posterior2_interp>as.numeric(val) & xout > gmm$mu[1]) #assumption: in the range of CL above the first peak and crosses the threshold value
    if (length(above_idx) > 0) {
      closest_idx <- above_idx[which.min(posterior2_interp[above_idx] - as.numeric(val))]
      cutoff_double_high[[val]] <- xout[closest_idx]
      prob_double_high[[val]] <-posterior2_interp[closest_idx]
    } else {
      fallback_idx <- which.min(abs(posterior2_interp - as.numeric(val)))
      cutoff_double_high[[val]]<-xout[fallback_idx]
      prob_double_high[[val]] <-posterior2_interp[fallback_idx]
    }
  }
  
  list(gmm = gmm, summary = summary_df, density = dens_df, 
       cutoff_double_high = cutoff_double_high, cutoff_double_low = cutoff_double_low,
       prob_double_high = prob_double_high, prob_double_low = prob_double_low)
}

# Bootstrap cohort to estimate 95% CIs
## Define the function to return the resampled data
resample_cutoff_func <- function(data, indices) {
  resampled_data<-data[indices]
  result<-fit_gmm(resampled_data)
  return(c(result$gmm$mu[1],result$gmm$mu[2], result$gmm$sigma[1],result$gmm$sigma[2], 
           result$gmm$cutoff, 
           result$cutoff_double_low[["0.5"]], result$cutoff_double_high[["0.5"]],
           result$cutoff_double_low[["0.6"]], result$cutoff_double_high[["0.6"]],
           result$cutoff_double_low[["0.7"]], result$cutoff_double_high[["0.7"]],
           result$cutoff_double_low[["0.8"]], result$cutoff_double_high[["0.8"]],
           result$cutoff_double_low[["0.9"]], result$cutoff_double_high[["0.9"]]))
}

boot_gmm<- function(data) {
  boot_out <- boot(data = data$cl, statistic = resample_cutoff_func, R = 5000)
  mu1_ci<-boot.ci(boot_out, type = "perc", index=1)
  mu2_ci<-boot.ci(boot_out,  type = "perc", index=2)
  sigma1_ci<-boot.ci(boot_out, type = "perc", index=3)
  sigma2_ci<-boot.ci(boot_out,  type = "perc", index=4)
  cutoff_ci<-boot.ci(boot_out,  type = "perc", index=5)
  cutoff_double_low_0p5_ci<-boot.ci(boot_out,  type = "perc", index=6)
  cutoff_double_high_0p5_ci<-boot.ci(boot_out,  type = "perc", index=7)
  cutoff_double_low_0p6_ci<-boot.ci(boot_out,  type = "perc", index=8)
  cutoff_double_high_0p6_ci<-boot.ci(boot_out,  type = "perc", index=9)
  cutoff_double_low_0p7_ci<-boot.ci(boot_out,  type = "perc", index=10)
  cutoff_double_high_0p7_ci<-boot.ci(boot_out,  type = "perc", index=11)
  cutoff_double_low_0p8_ci<-boot.ci(boot_out,  type = "perc", index=12)
  cutoff_double_high_0p8_ci<-boot.ci(boot_out,  type = "perc", index=13)
  cutoff_double_low_0p9_ci<-boot.ci(boot_out,  type = "perc", index=14)
  cutoff_double_high_0p9_ci<-boot.ci(boot_out,  type = "perc", index=15)
  
  return(list(mu1_ci,mu2_ci,sigma1_ci,sigma2_ci,cutoff_ci,
              cutoff_double_low_0p5_ci,cutoff_double_high_0p5_ci,
              cutoff_double_low_0p6_ci,cutoff_double_high_0p6_ci,
              cutoff_double_low_0p7_ci,cutoff_double_high_0p7_ci,
              cutoff_double_low_0p8_ci,cutoff_double_high_0p8_ci,
              cutoff_double_low_0p9_ci,cutoff_double_high_0p9_ci))
}