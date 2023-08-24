
# function to plot the CCM (simple plot : only the CCM)
fun_ccm_plot2 <- function(correlation_df, var, time_frame,  metric_name, indicator){
  
  if(metric_name == "glmm"){
    if(indicator=="presence"){
      correlation_df$abs_corr <- abs(correlation_df$correlation - 1 )
    } else if (indicator=="abundance"){
      correlation_df$abs_corr <- abs(correlation_df$correlation)
    }
  }
  
  if(length(unique(correlation_df$abs_corr))!=1){ # to deal with case all correlation values are NAs
    most_corr <- correlation_df %>% filter(abs_corr == max(abs_corr, na.rm = T))
    
    #most_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% head(round(0.1*nrow(.))) # 3 % top correlations will be blacked borders
    #most_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% filter(abs_corr >= most_corr$abs_corr - 0.05)
    
    most_corr2 <- correlation_df %>% arrange(desc(abs_corr)) %>% filter(abs_corr >= most_corr$abs_corr * 0.9)
  } else {
    most_corr <- most_corr2 <- correlation_df[1,]
  }
  
  ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = correlation)) +
    geom_tile(color = "white", show.legend = TRUE, size = 0.05,aes(width=1, height=1)) + 
    geom_tile(data = most_corr2 , color = "black", size = 0.2, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1)
    geom_tile(data = most_corr , color = "deeppink3", size = 0.6, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1)
    theme_minimal() + 
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          axis.title = element_text(size = 8),
          legend.key.size = unit(0.8, "cm"),
          legend.title=element_text(size=10),
          legend.position = "none"
    ) +
    ggtitle(var) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(",most_corr$time_lag_2*time_frame,",",most_corr$time_lag_1*time_frame,") = ",round(most_corr$correlation,2))) +
    coord_fixed() +
    ylab("time lag 1") +
    xlab("time lag 2")
  #scale_x_continuous(breaks = seq(0,8,2), labels = seq(0,40,10)) +
  #scale_y_continuous(breaks = seq(0,8,2), labels = seq(0,40,10))
  
  
  if(metric_name=="Spearman"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-.71,.71), space = "Lab", name = "Spearman correlation", na.value = "grey") #+
  } else if(metric_name=="glmm"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = ifelse(indicator == "abundance", 0, 1), space = "Lab", name = ifelse(indicator == "abundance","DDR","ODR"),
                           # limit = c(correlation_df$min[1], correlation_df$max[1])) 
                           #limit = c(-1.5, 1.5)
      ) 
  }
  
  return(ccm_plot)
  
}


fun_plot_tile_univ_spatial <- function(correlation_df, metric_name , indicator){
  
  correlation_df <- correlation_df %>%
    mutate(correlation = ifelse(indicator=="abundance",correlation,correlation-1))
  
  correlation_df <- correlation_df %>%
    mutate(buffer = forcats::fct_relevel(buffer, c("0","20","50","100","250"))) %>%
    mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance")))
  
  #correlation_df$label <- factor(correlation_df$label, levels = unique(correlation_df$label[order(correlation_df$correlation)]))
  correlation_df$label <- factor(correlation_df$label, levels = unique(correlation_df$label[order(correlation_df$label)]))
  correlation_df <- correlation_df %>% filter(!is.na(correlation),p.value<=0.2) %>%
    mutate(p.value2 = case_when(
      p.value <= 0.001 ~ "***",
      p.value > 0.001 & p.value <= 0.01  ~  "**",
      p.value > 0.01 & p.value <= 0.05 ~ " *",
      p.value > 0.05 ~ ""
    ))
  
  
  p <- ggplot(correlation_df, aes(buffer, label)) + 
    geom_tile(aes(fill = correlation), color = "white") + facet_grid(type~indicator, scales="free_y", space="free_y") +
    xlab("buffer radius around the collection site (meters)") + 
    ylab("") +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10, face = "italic")
    ) +
    #geom_text(aes(label = ifelse(is.na(correlation), "",paste(round(correlation,2), p.value2))), size = 3)
    geom_text(aes(label = ifelse(is.na(correlation), "", p.value2)), size = 3)
    
  
  if(metric_name == "glmm"){
  #  if(indicator == "presence"){
     # p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 1, space = "Lab", name = metric_name, na.value="grey")
   # } else if (indicator == "abundance"){
      p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, space = "Lab", name = "effect", na.value="grey")
    #}
  } else if (metric_name == "Spearman"){
    p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-0.7,0.7), space = "Lab", name = metric_name, na.value="grey")
  }
  
  return(p)
}

