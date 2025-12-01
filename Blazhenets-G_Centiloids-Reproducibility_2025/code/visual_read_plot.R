# Plot for visual read vs centiloids
plot_vr <- function(data,cohort,kappa) {
  data_clean<-subset(data, is.na(data$visual_read)==F)
  ggplot(data, aes(y= visual_read, x = cl, color=as.factor(visual_read)))+
    geom_point(size=2,alpha=0.8,position = position_jitter(w = 0, h = 0.1))+
    geom_violin(alpha = 0.7, linewidth=1)+
    xlim(-50,250)+
    xlab("Centiloids")+
    ylab("")+
    scale_color_manual(values = pal2) +
    theme_bw()+
    theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12))+
    theme(legend.position = "none")
}