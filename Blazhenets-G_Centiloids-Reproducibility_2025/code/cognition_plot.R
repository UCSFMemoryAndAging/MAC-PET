# associations with clinical variables continuous
plot_mmse <- function(data) {
  data_clean<-subset(data, is.na(data$mmse)==F)
  p<-ggplot(data_clean, aes(y= cl, x = mmse))+
    geom_point(shape=21, size=2, alpha=0.6, stroke = .4, color="black", fill = "#999999")+
    geom_smooth(method="lm", formula = "y~x", color="#E69F00",fill="lightgrey")+
    theme_minimal()+
    ylab("Centiloids")+
    xlab("MMSE")+
    xlim(0,30)+
    ylim(-50,250)+
    guides()+
    ggtitle(paste(cohort))+
    theme(legend.position = "none")+
    theme_bw()+
    theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(size = 12))
  
  correlation_mmse_cl<-cor.test(data_clean$cl, data_clean$mmse, method = "pearson")
  list(correlation_mmse_cl=correlation_mmse_cl, mmse_plot=p)
}

# associations with clinical variables bin
plot_cdr <- function(data, cohort){
  data_clean<-subset(data, is.na(data$cdr)==F)
  p<-ggplot(data_clean, aes(y= cl, x = as.factor(cdr)))+
    geom_point(aes(fill=as.factor(cdr)),shape=21, size=2, position=position_jitterdodge(), alpha=0.6, stroke =0.4, color= "black")+
    geom_boxplot(aes(fill=as.factor(cdr)),alpha= 0.6, outlier.shape = NA, width=0.25)+
    theme_minimal()+
    ylab("Centiloids")+
    xlab("CDR")+
    scale_fill_brewer(palette=('YlOrRd'), guide = "none")+
    labs(title=paste(cohort))+
    theme(legend.position = "none")+
    theme_bw()+
    theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(size = 12))
  
    correlation_cdr_cl<-cor.test(data_clean$cl, data_clean$cdr, method = "pearson")
    cdr_anova<-aov(cl~as.factor(cdr), data = data_clean)
    cdr_tukey<-TukeyHSD(cdr_anova,which = "as.factor(cdr)")
    #specific_pairs <- c("0.5-0", "1-0.5", "2-1", "3-2")
    #cdr_tukey<-cdr_tukey$`as.factor(cdr)`[rownames(cdr_tukey$`as.factor(cdr)`) %in% specific_pairs, c("diff", "p adj")]

    list(correlation_cdr_cl=correlation_cdr_cl, cdr_anova=cdr_anova, cdr_tukey=cdr_tukey, cdr_plot=p)
}

