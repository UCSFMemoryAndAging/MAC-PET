# associations with apoe
plot_apoe <- function(data, cohort){
  #prepare data
  data_clean<-data %>% 
    subset(is.na(apoe4_status)==F)%>%
    mutate(apoe4_status = factor(apoe4_status, levels=c("Non-Carrier", "Heterozygous-Carrier", "Homozygous-Carrier")))
  if (nrow(data_clean)!=0) {
  #plot
    p<-ggplot(data_clean, aes(y= cl, x = as.factor(apoe4_status)))+
      geom_point(aes(fill=as.factor(apoe4_status)),shape=21, size=3, position=position_jitterdodge(), alpha=0.6, stroke =0.4, color= "black")+
      geom_boxplot(aes(fill=as.factor(apoe4_status)),alpha= 0.6, outlier.shape = NA, width = 0.25)+
      theme_minimal()+
      ylab("Centiloids")+
      xlab("")+
      scale_fill_brewer(palette=('Set2'), guide = "none")+
      labs(title=paste(cohort))+
      theme(legend.position = "none")+
      theme_bw()+
      theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
      theme(panel.grid.major.x = element_blank())+
      theme(axis.line = element_line(colour = "black"))+
      theme(axis.text.x = element_text(size = 8))
  
  apoe_anova<-aov(cl~as.factor(apoe4_status), data = data_clean)
  apoe_tukey<-TukeyHSD(apoe_anova,which = "as.factor(apoe4_status)")
  
  list(apoe_anova=apoe_anova, apoe_tukey=apoe_tukey, apoe_plot=p)
  }
}


plot_apoe_full <- function(data, cohort){
  #prepare data
  data_clean<-data %>% 
    subset(is.na(apoe)==F)
  
  #plot
  p<-ggplot(data_clean, aes(y= cl, x = as.factor(apoe)))+
    geom_point(aes(fill=as.factor(apoe)),shape=21, size=3, position=position_jitterdodge(), alpha=0.6, stroke =0.4, color= "black")+
    geom_boxplot(aes(fill=as.factor(apoe)),alpha= 0.6, outlier.shape = NA, width = 0.25)+
    theme_minimal()+
    ylab("Centiloids")+
    xlab("")+
    scale_fill_brewer(palette=('Set2'), guide = "none")+
    labs(title=paste(cohort))+
    theme(legend.position = "none")+
    theme_bw()+
    theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(size = 12))
  
  apoe_anova<-aov(cl~as.factor(apoe), data = data_clean)
  apoe_tukey<-TukeyHSD(apoe_anova,which = "as.factor(apoe)")
  
  list(apoe_anova=apoe_anova, apoe_tukey=apoe_tukey, apoe_plot=p)
}

