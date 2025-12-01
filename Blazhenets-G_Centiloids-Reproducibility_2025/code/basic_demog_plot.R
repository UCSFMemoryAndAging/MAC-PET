# Function to create scatter plot with regression for age
plot_age <- function(data, cohort, group) {
  correlation_age_cl <- list()
  
  # Prepare data
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
      subset(!is.na(group_bin)==T)
    group_counts <- table(data$group_bin)
    
  }
  
  # Check if group_bin exists in the data
  if ("group_bin" %in% colnames(data) &&
      !is.na(group_counts["cn"]) && group_counts["cn"] > 10 && 
        !is.na(group_counts["patient"]) && group_counts["patient"] > 10) {
      
      # Plot with group_bin and fill aesthetics
      p <- ggplot(data, aes(y = cl, x = age, group = group_bin, color = group_bin)) +
        geom_point(aes(fill = group_bin), shape = 21, stroke = .4, size = 2, alpha = .6, color = "black", show.legend = T) +
        geom_smooth(aes(color = group_bin), method = "lm", formula = "y ~ x", fill = "lightgrey", show.legend = T) +
        theme_minimal() +
        ylab("Centiloids") +
        xlab("Age at PET") +
        xlim(18, 110) +
        ylim(-50, 250) +
        scale_fill_manual(values = c("cn" = pal[1], "patient" = pal[2]), guide = "none") +
        scale_color_manual(values = c("cn" = pal[1], "patient" = pal[2]), name = "Dx", labels = c("CN", "Cognitively impaired")) +
        theme_bw() +
        theme(legend.position = "bottom", panel.border = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))
      
      # Calculate association strength for each group
      for (g in unique(data$group_bin)) {
        data_subset <- subset(data, data$group_bin == g)
        correlation_age_cl[[g]] <- cor.test(data_subset$cl, data_subset$age, method = "pearson")
      }
      
    } else {
      # Plot without group_bin (default plot)
      p <- ggplot(data, aes(y = cl, x = age)) +
        geom_point(shape = 21, size = 2, alpha = 0.6, stroke = .4, color = "black", fill = "#999999") +
        geom_smooth(method = "lm", formula = "y ~ x", color = "#CC79A7", fill = "lightgrey") +
        theme_minimal() +
        ylab("Centiloids") +
        xlab("Age at PET") +
        xlim(18, 110) +
        ylim(-50, 250) +
        theme_bw() +
        theme(legend.position = "none", panel.border = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(size = 12))
      
      # Calculate overall association strength
      correlation_age_cl <- cor.test(data$cl, data$age, method = "pearson")
    }

  
  # Return both the correlation list and plot
  list(correlation_age_cl = correlation_age_cl, age_plot = p)
}


# Function to create box plots for sex
pal2<-c('#955c9F91','#0239a099')
plot_sex <- function(data, cohort, group) {
  difference_sex_cl<-list()
  mean_cl_male<-list()
  mean_cl_female<-list()
  sd_cl_male<-list()
  sd_cl_female<-list()
  n_male<-list()
  n_female<-list()
  
  # Prepare data
  if (nlevels(as.factor(group)) != 1) { 
    if (typeof(group) == "character") {
      group <- tolower(group)
      data$group_bin <- group
      data$group_bin[group == 'mci'] <- 'patient'
      data$group_bin[group == 'dementia'] <- 'patient'
      data$group_bin[group == 'cn'] <- 'cn'
      
    } else if (typeof(group) == "double") {
      data$group_bin <- group
      data$group_bin[group == 0] <- 'cn'
      data$group_bin[group >= 0.5] <- 'patient'
    }
    data<-data%>%
      subset(!is.na(group_bin)==T)
    group_counts <- table(data$group_bin)
    
  } else
    group_counts=NA
  
  # Check if group_bin exists in the data
  if ("group_bin" %in% colnames(data) &&
      !is.na(group_counts["cn"]) && group_counts["cn"] > 10 && 
      !is.na(group_counts["patient"]) && group_counts["patient"] > 10) {
    
    
    #plot  
    p<-ggplot(subset(data, is.na(data$group_bin)==F), aes(y= cl, fill = as.factor(male), x= as.factor(group_bin)))+
      geom_point(shape=21, size=2, position=position_jitterdodge(),alpha=0.6, stroke=0.4, color="black")+
      geom_boxplot(alpha= 0.8, outlier.shape = NA)+
      theme_minimal()+
      ylab("Centiloids")+
      xlab("")+
      scale_x_discrete(labels=c("CN","Cognitively impaired"))+
      scale_fill_manual(values=c("0" = pal2[1], "1" = pal2[2]), labels = c("Female", "Male"), name = "Sex")+
      scale_color_manual(values=c("0" = pal2[1], "1" = pal2[2]), labels = c("Female", "Male"))+
      labs(title=paste(cohort))+
      theme_bw()+
      theme(legend.position = "bottom")+
      theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
      theme(panel.grid.major.x = element_blank())+
      theme(axis.line = element_line(colour = "black"))+
      theme(axis.text.x = element_text(size = 12))
    
    #calculate group differences
    difference_sex_cl<-list()
    for (g in levels(as.factor(data$group_bin))){
      data_subset<-subset(data,data$group_bin==g)
      sex_counts <- table(as.factor(data_subset$male))
      if (!is.na(sex_counts['1'])) {
        n_male[[g]] <-sex_counts['1']
        mean_cl_male[[g]]<-mean(data_subset$cl[data_subset$male==1],na.rm=T)
        sd_cl_male[[g]]<-sd(data_subset$cl[data_subset$male==1],na.rm=T)
      } else {
        n_male[[g]]=0
        mean_cl_male[[g]]=NA
        sd_cl_male[[g]]=NA
      }
      if (!is.na(sex_counts['0'])) {
        n_female[[g]] <-sex_counts['0']
        mean_cl_female[[g]]<-mean(data_subset$cl[data_subset$male==0],na.rm=T)
        sd_cl_female[[g]]<-sd(data_subset$cl[data_subset$male==0],na.rm=T)
      } else {
        n_female[[g]]=0
        mean_cl_female[[g]]=NA
        sd_cl_female[[g]]=NA
      }

      if (nlevels(as.factor(data_subset$male)) != 1 &&
        !is.na(sex_counts['0']) && sex_counts['0'] > 10 && 
        !is.na(sex_counts['1']) && sex_counts['1'] > 10) {
          difference_sex_cl[[g]]<-t.test(cl~as.factor(male), data = data_subset)
        }
      }

  } else {
    #plot
    p<-ggplot(data, aes(y= cl, x = as.factor(male)))+
          geom_point(aes(fill=as.factor(male)),shape=21, size=2, position=position_jitterdodge(), alpha=0.6, stroke =0.4, color= "black")+
          geom_boxplot(aes(fill=as.factor(male)),alpha= 0.8, outlier.shape = NA)+
          ylab("Centiloids")+
          xlab("")+
          scale_x_discrete(labels = c("Female", "Male"))+
          scale_fill_manual(values=c("0" = pal2[1], "1" = pal2[2]))+
          scale_color_manual(values=c("0" = pal2[1], "1" = pal2[2]))+
          labs(title=paste(cohort))+
          theme_bw()+
          theme(legend.position = "none")+
          theme(panel.border = element_blank(),panel.grid.minor = element_blank())+
          theme(panel.grid.major.x = element_blank())+
          theme(axis.line = element_line(colour = "black"))+
          theme(axis.text.x = element_text(size = 12))
    
    #calculate group difference
    mean_cl_male<-mean(data$cl[data$male==1],na.rm=T)
    mean_cl_female<-mean(data$cl[data$male==0],na.rm=T)
    sd_cl_male<-sd(data$cl[data$male==1],na.rm=T)
    sd_cl_female<-sd(data$cl[data$male==0],na.rm=T)
    n_male<-NA
    n_female<-NA
    
    if (nlevels(as.factor(data$male)) != 1){
    difference_sex_cl<-t.test(cl~as.factor(male), data = data)
    }
  }
  list(difference_sex_cl=difference_sex_cl, n_male=n_male, n_female=n_female,
       mean_cl_male=mean_cl_male, mean_cl_female=mean_cl_female,
       sd_cl_male=sd_cl_male, sd_cl_female=sd_cl_female, group_counts=group_counts, sex_plot=p)
}


