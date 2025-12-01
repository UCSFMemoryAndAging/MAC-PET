# Function to create ROC plot using pROC
create_roc_plot <- function(roc_curve, youden_index, title) {
  ggroc(roc_curve)+
  geom_point(aes(x = youden_index$specificity, y = youden_index$sensitivity), color = pal[2], size = 3) +
  geom_vline(xintercept = youden_index$specificity, linetype = "dotted", color = pal[2]) +
  geom_hline(yintercept = youden_index$sensitivity, linetype = "dotted", color = pal[2]) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "gray")+
  annotate("text", x = youden_index$specificity, y = 1, label = paste("Cutoff [Youden]:", 
          round(youden_index$threshold, 2)), hjust = -0.1, vjust = -0.5, color = pal[2], size = 3.5)+
  ggtitle(paste(cohort))+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.grid.major.x = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
}

# Function to calculate optimal cutoffs using pROC
calculate_cutoff <- function(data) {
  roc_curve <- roc(data$visual_read, data$cl, levels=c("negative", "positive"), direction="<")
  youden_index <- coords(roc_curve, "best", best.method = "youden", best.policy = "random", ret=c("threshold", "sensitivity", "specificity"))
  cutoff_95sens <- roc_curve$thresholds[tail(which(roc_curve$sensitivities > 0.95), 1)]
  cutoff_95spec <- roc_curve$thresholds[head(which(roc_curve$specificities > 0.95), 1)]
  cutoff_90sens <- roc_curve$thresholds[tail(which(roc_curve$sensitivities > 0.90), 1)]
  cutoff_90spec <- roc_curve$thresholds[head(which(roc_curve$specificities > 0.90), 1)]
  cutoff_youden_ci<-ci.coords(roc_curve, "best", best.method = "youden", best.policy = "random", ret = "threshold",  transpose = FALSE, boot.n = 5000)
  auc_ci<-ci.auc(roc_curve)
 
  bootstrap_cutoff <- function(data, indices) {
    d <- data[indices, ]
    roc_sample <- roc(d$visual_read, d$cl, levels=c("negative", "positive"), direction="<")
    cutoff_sample_sens_95 <- roc_sample$thresholds[tail(which(roc_sample$sensitivities > 0.95), 1)]
    cutoff_sample_sens_90 <- roc_sample$thresholds[tail(which(roc_sample$sensitivities > 0.90), 1)]
    cutoff_sample_spec_95 <- roc_sample$thresholds[head(which(roc_sample$specificities > 0.95), 1)]
    cutoff_sample_spec_90 <- roc_sample$thresholds[head(which(roc_sample$specificities > 0.90), 1)]
    return(c(cutoff_sample_sens_95, cutoff_sample_sens_90, cutoff_sample_spec_95, cutoff_sample_spec_90))
  }
  boot_out <- boot(data, statistic = bootstrap_cutoff, R = 5000)
  cutoff_95sens_ci<-boot.ci(boot_out, type = "perc", index=1)
  cutoff_90sens_ci<-boot.ci(boot_out, type = "perc", index=2)
  cutoff_95spec_ci<-boot.ci(boot_out, type = "perc", index=3)
  cutoff_90spec_ci<-boot.ci(boot_out, type = "perc", index=4)
  
  list(cutoff_youden = youden_index, roc_curve = roc_curve,
       cutoff_95sens=cutoff_95sens, cutoff_95spec=cutoff_95spec,
       cutoff_90sens=cutoff_90sens, cutoff_90spec=cutoff_90spec,
       cutoff_youden_ci=cutoff_youden_ci,
       cutoff_95sens_ci=cutoff_95sens_ci,cutoff_95spec_ci=cutoff_95spec_ci,
       cutoff_90sens_ci=cutoff_90sens_ci,cutoff_90spec_ci=cutoff_90spec_ci,
       auc_ci=auc_ci)
}



