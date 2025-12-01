# apoe by dx analysis
analyze_apoe_dx <- function(data, group){
  
  #prepare data
  if (length(group) != 1) { 
    if (typeof(group) == "character") {
      group <- tolower(group)
      data$group_bin <- group
      data$group_bin[group == "mci"] <- 'patient'
      data$group_bin[group == "dementia"] <- 'patient'
      data$group_bin[group == "cn"] <- 'cn'
      
    } else if (typeof(group) == "double") {
      data$group_bin <- group
      data$group_bin[group == 0] <- 'cn'
      data$group_bin[group >= 0.5] <- 'patient'
    }
    data<-data%>%
      subset(is.na(apoe4_status)==F)%>%
      subset(!is.na(group_bin)==T)
    group_counts <- table(data$group_bin)
  }
  
  apoe_non_n<-list()
  apoe_homo_n<-list()
  apoe_hetero_n<-list()
  apoe_non_mean_cl<-list()
  apoe_homo_mean_cl<-list()
  apoe_hetero_mean_cl<-list()
  apoe_non_sd_cl<-list()
  apoe_homo_sd_cl<-list()
  apoe_hetero_sd_cl<-list()
  # Check if group_bin exists in the data
  if ("group_bin" %in% colnames(data) &&
      !is.na(group_counts["cn"]) && group_counts["cn"] > 10 && 
      !is.na(group_counts["patient"]) && group_counts["patient"] > 10) {
    
    # Calculate association strength for each group
    for (g in unique(data$group_bin)) {
      data_subset <- subset(data, data$group_bin == g)
      apoe_non_n[[g]]<-length(data_subset$cl[data_subset$apoe4_status=="Non-Carrier"])
      apoe_homo_n[[g]]<-length(data_subset$cl[data_subset$apoe4_status=="Homozygous-Carrier"])
      apoe_hetero_n[[g]]<-length(data_subset$cl[data_subset$apoe4_status=="Heterozygous-Carrier"])
      apoe_non_mean_cl[[g]]<-mean(data_subset$cl[data_subset$apoe4_status=="Non-Carrier"])
      apoe_homo_mean_cl[[g]]<-mean(data_subset$cl[data_subset$apoe4_status=="Homozygous-Carrier"])
      apoe_hetero_mean_cl[[g]]<-mean(data_subset$cl[data_subset$apoe4_status=="Heterozygous-Carrier"])
      apoe_non_sd_cl[[g]]<-sd(data_subset$cl[data_subset$apoe4_status=="Non-Carrier"])
      apoe_homo_sd_cl[[g]]<-sd(data_subset$cl[data_subset$apoe4_status=="Homozygous-Carrier"])
      apoe_hetero_sd_cl[[g]]<-sd(data_subset$cl[data_subset$apoe4_status=="Heterozygous-Carrier"])
    }
    
  } else {
    apoe_non_n["cn"]<-NA
    apoe_homo_n["cn"]<-NA
    apoe_hetero_n["cn"]<-NA
    apoe_non_mean_cl["cn"]<-NA
    apoe_homo_mean_cl["cn"]<-NA
    apoe_hetero_mean_cl["cn"]<-NA
    apoe_non_sd_cl["cn"]<-NA
    apoe_homo_sd_cl["cn"]<-NA
    apoe_hetero_sd_cl["cn"]<-NA
    apoe_non_n["patient"]<-NA
    apoe_homo_n["patient"]<-NA
    apoe_hetero_n["patient"]<-NA
    apoe_non_mean_cl["patient"]<-NA
    apoe_homo_mean_cl["patient"]<-NA
    apoe_hetero_mean_cl["patient"]<-NA
    apoe_non_sd_cl["patient"]<-NA
    apoe_homo_sd_cl["patient"]<-NA
    apoe_hetero_sd_cl["patient"]<-NA
  }
  
  list(apoe_non_n=apoe_non_n, apoe_homo_n=apoe_homo_n, apoe_hetero_n=apoe_hetero_n, 
       apoe_non_mean_cl=apoe_non_mean_cl, apoe_homo_mean_cl=apoe_homo_mean_cl, apoe_hetero_mean_cl=apoe_hetero_mean_cl, 
       apoe_non_sd_cl=apoe_non_sd_cl, apoe_homo_sd_cl=apoe_homo_sd_cl, apoe_hetero_sd_cl=apoe_hetero_sd_cl)
}
