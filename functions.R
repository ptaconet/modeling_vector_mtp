########################### Functions which allow the interpretation of results of GLMM

########################### Function which allows the realization of Cross Correlation maps (simple plots) 
#########'It is using the Marginal, which is recalculated: it is negative if the correlation is negative or positive if so. 
#########'Only the significtaive relations are conserved (p-value <0.2)
###########################

fun_ccm_plot_r2 <- function(correlation_df, var, time_frame,  metric_name, indicator){
  
  if(metric_name == "glmm"){ 
    if(indicator=="presence"){ ## for presence model: it highlights the direction of the correlation and adapts the marginal r2: if correlation is negative, r2 is going to be negative also.
      correlation_df$abs_corr <- abs(correlation_df$correlation - 1 )
      correlation_df$r2_modif<-as.numeric(ifelse(correlation_df$abs_corr<1,correlation_df$r2*(-1), correlation_df$r2))
    } else if (indicator=="abundance"){ ## for presence model: it highlights the direction of the correlation and adapts the marginal r2: if correlation is negative, r2 is going to be negative also.
      correlation_df$abs_corr <- abs(correlation_df$correlation)
      correlation_df$r2_modif<-as.numeric(ifelse(correlation_df$correlation>1,correlation_df$r2*(-1), correlation_df$r2))
    }
  }
  
  if(length(unique(correlation_df$abs_corr))!=1){ ## to deal with case all correlation values are NAs when p-value >0.2
    
    most_corr <- correlation_df %>% filter(r2 == max(r2, na.rm = T)) ## to underline the R2 which is among the highest and will be marked in pink in the CCMs.
    most_corr2 <- correlation_df %>% arrange(desc(r2)) %>% filter(r2 >= most_corr$r2 * 0.9) ## to underline the R2 which are among the highest and will be marked in black in the CCMs.
  } else {
    most_corr <- most_corr2 <- correlation_df[1,]
  }
  
  ccm_plot <- ggplot(data = correlation_df, aes(as.numeric(time_lag_1), as.numeric(time_lag_2), fill = as.numeric(r2_modif))) + ## to represent the CMM between different time lags and with the values or marginal R2
    geom_tile(color = "white", show.legend = TRUE, size = 0.05,aes(width=1, height=1)) + 
    geom_tile(data = most_corr2 , color = "black", size = 0.2, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1) ## To underline highest r2s in black
    geom_tile(data = most_corr , color = "deeppink3", size = 0.6, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1) ## to underlien the highest r2 in pink
    theme_minimal() + 
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          axis.title = element_text(size = 8),
          legend.key.size = unit(0.8, "cm"),
          legend.title=element_text(size=10),
          legend.position = "none"
    ) +
    ggtitle(var) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(",most_corr$time_lag_2*time_frame,",",most_corr$time_lag_1*time_frame,")")) +
    coord_fixed() +
    ylab("time lag 1") +
    xlab("time lag 2")
  #scale_x_continuous(breaks = seq(0,8,2), labels = seq(0,40,10)) +
  #scale_y_continuous(breaks = seq(0,8,2), labels = seq(0,40,10))
  common_scale <- scale_fill_gradient2( ## to put a common scale for all ccms, which is red for positive modified R2 and blue if negative and white if 0 = no effect
    low = "blue", 
    high = "red", 
    mid = "white", 
    midpoint = 0, 
    limit = c(-0.5, 0.6), 
    space = "Lab", 
    name = "r2 ajuste", 
    na.value = "grey"
  )
  
  if(metric_name=="Spearman"){
    ccm_plot <- ccm_plot + 
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-.71,.71), space = "Lab", name = "Spearman correlation", na.value = "grey") 
  } else if(metric_name=="glmm"){
    ccm_plot <- ccm_plot +  
      common_scale+ theme(legend.position = "bottom") ## Final CCM with the legend correctly positionned
  }
  return(ccm_plot)
}


########################### Function which allows the interpretation of GLMMs results for  spatial data and micro climatic
#########'It is using the Marginal, which is recalculated: it is negative if the correlation is negative or positive if so. 
#########'Only the significtaive relations are conserved (p-value <0.2)
##########################


fun_plot_tile_univ_spatial_r2 <- function(correlation_df, metric_name , indicator, lc_source, type, xlabel){
 
  correlation_df <- correlation_df %>%
    mutate(abs_corr = ifelse(indicator=="abundance",correlation,correlation-1))%>%  ## for presence /abundance model: it highlights the direction of the correlation and adapts the marginal r2: if correlation is negative, r2 is going to be negative also.
    mutate(r2_modif=dplyr::case_when(indicator=='presence'& abs(correlation)>=1~r2, 
                                     indicator=='presence'& abs(correlation)<1~r2*(-1), 
                                     indicator=="abundance"&correlation>=0~r2, indicator=="abundance"&correlation<0~r2*(-1)))

  
  
  correlation_df <- correlation_df %>%
    mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance")))
  
  #correlation_df$label <- factor(correlation_df$label, levels = unique(correlation_df$label[order(correlation_df$correlation)]))
  correlation_df$label <- factor(correlation_df$label, levels = unique(correlation_df$label[order(correlation_df$label)])) ## it keeps only p-value <0.2
  correlation_df <- correlation_df %>% filter(!is.na(correlation),p.value<=0.2) %>%
    mutate(p.value2 = case_when(
      p.value <= 0.001 ~ "***", ## if p-value <0.001 --> ***
      p.value > 0.001 & p.value <= 0.01  ~  "**", ## if 0.01>p-value >0.001 --> **
      p.value > 0.01 & p.value <= 0.05 ~ " *",## if 0.05>p-value >0.01 --> *
      p.value > 0.05 ~ "" ## if 0.2>p-value >0.05 --> just the color
    ))
  
  
  p <- ggplot(correlation_df, aes(buffer, label)) +  ## represent graphicallu the correlation with the r2 modif for different buffer
    geom_tile(aes(fill = r2_modif), color = "white") + 
    facet_grid(type~indicator, scales="free_y", space="free_y") +
    #facet_grid(.~indicator, scales="free_y", space="free_y") +
    xlab(xlabel) + 
    ylab("") +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.x = element_text(size = 10),
          strip.text.y = element_text(size = 10, face = "italic"),
          axis.text.y = element_text(size = 8)
    ) +
    #geom_text(aes(label = ifelse(is.na(correlation), "",paste(round(correlation,2), p.value2))), size = 3)
    geom_text(aes(label = ifelse(is.na(correlation), "", p.value2)), size = 3) + 
    ggtitle(paste0(lc_source," - ", type))
  
  
  
  if(metric_name == "glmm"){ ## To add a legend 
    #  if(indicator == "presence"){
    # p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 1, space = "Lab", name = metric_name, na.value="grey")
    # } else if (indicator == "abundance"){
    p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-0.5,0.5),midpoint = 0, space = "Lab", name = "r2 ajuste", na.value="grey")
    #}
  } else if (metric_name == "Spearman"){
    p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-0.7,0.7), space = "Lab", name = metric_name, na.value="grey")
  }
  
  return(p)
}




