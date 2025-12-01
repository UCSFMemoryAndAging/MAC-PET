# Function to create GMM plot
create_gmm_plot_display <- function(summary_df, dens_df, gmm, title, boot) {
  max_y<-max(dens_df$total)
  boot<-boot[[5]]#boot for cutoff_ci
  ggplot() +
    geom_rect(aes(xmin=boot$percent[4],xmax=boot$percent[5], ymin=-Inf, ymax=Inf), fill="#f0efeb", alpha=0.8)+
    geom_histogram(aes(x = data, y = after_stat(density)), binwidth = 2, fill = "white", color = "lightgrey", alpha = 0.5, data = summary_df) +
    geom_line(aes(x = x, y = comp1), color = pal[1], linewidth = 1, data = dens_df) +
    geom_line(aes(x = x, y = comp2), color = pal[2], linewidth = 1, data = dens_df) +
    geom_vline(xintercept = gmm$mu[1]+2*gmm$sigma[1], color = "black", linewidth = 0.5) +
    annotate("text",x=gmm$mu[1]+2*gmm$sigma[1], y = max_y+0.5*max_y, label = round(as.numeric(gmm$mu[1]+2*gmm$sigma[1]),1), color = 'darkred')+
    annotate("text",x=gmm$mu[1], y = max_y+0.2*max_y, label = round(as.numeric(gmm$mu[1]),1), color = 'black')+
    annotate("text",x=gmm$mu[2], y = max_y+0.2*max_y, label = round(as.numeric(gmm$mu[2]),1), color = 'black')+
    labs(title = title, y = "", x = "") +
    scale_x_continuous(limits = c(-50, 250),
                       breaks = c(0, 50, 100, 150, 200),
                       labels = c(0, 50, 100, 150, 200)) +
    theme_bw()+
    theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(size = 12))
}


create_gmm_plot_save <- function(summary_df, dens_df, gmm, title, boot) {
  max_y<-max(dens_df$total)
  ggplot() +
    geom_histogram(aes(x = data, y = after_stat(density)), binwidth = 5, fill = "#f0efeb", color = "darkgrey", alpha = 0.3, data = summary_df) +
    geom_line(aes(x = x, y = comp1), color = pal[1], linewidth = 2, data = dens_df) +
    geom_line(aes(x = x, y = comp2), color = pal[2], linewidth = 2, data = dens_df) +
    labs(title = "", y = "", x = "") +
    scale_x_continuous(limits = c(-50, 250),
                       breaks = c(0, 50, 100, 150, 200),
                       labels = c(0, 50, 100, 150, 200)) +
    theme_bw()+
    theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))+
    scale_y_continuous(
      labels = scales::number_format(accuracy = 0.001,
                                     decimal.mark = '.'))
}

# Create Posterior probability function plot
create_ppf_plot<-function(summary_df, gmm){
  ggplot()+
    geom_rect(aes(xmin=-Inf, xmax = gmm$cutoff_double_low[["0.9"]], ymin = -Inf, ymax = Inf), fill=pal[1], alpha=0.1) +
    geom_rect(aes(xmin=gmm$cutoff_double_high[["0.9"]], xmax = Inf, ymin = -Inf, ymax = Inf), fill=pal[2], alpha=0.1) +
    geom_line(aes(x = data, y = posterior1), color = pal[1], linewidth = 1, data = summary_df) +
    geom_line(aes(x = data, y = posterior2), color = pal[2], linewidth = 1, data = summary_df) +
    annotate("text",x=gmm$cutoff_double_low[["0.9"]], y = 1, label = round(as.numeric(gmm$cutoff_double_low[["0.9"]]),1), color = pal[1])+
    annotate("text",x=gmm$cutoff_double_high[["0.9"]], y = 1, label = round(as.numeric(gmm$cutoff_double_high[["0.9"]]),1), color = pal[2])+
    annotate("text",x=gmm$cutoff_double_low[["0.9"]]-10, y = 1.1, label = 'Confidently negative', color = "black")+
    annotate("text",x=gmm$cutoff_double_high[["0.9"]]+10, y = 1.1, label = 'Confidently positive', color = "black")+
    labs(title = "", y = "Posterior probability", x = "Centiloid") +
    scale_x_continuous(limits = c(-10, 100),
                       breaks = c(0, 5, 10, 15, 20, 25, 30, 50, 75, 100),
                       labels = c(0, 5, 10, 15, 20, 25, 30, 50, 75, 100)) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = c(0, 0.50, 0.60, 0.70, 0.80, 0.90, 1),
                       labels = c(0, 0.50, 0.60, 0.70, 0.80, 0.90, 1)) +
    theme_bw()+
    theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
    theme(panel.grid.major.x = element_blank())+
    theme(axis.line = element_line(colour = "black"))+
    theme(axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16))
}

