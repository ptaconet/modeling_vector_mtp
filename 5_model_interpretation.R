########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(purrr)  ## Version ‘1.0.1’
library(patchwork) ## Version ‘1.2.0.9000’
library(landscapemetrics) ## Version ‘2.1.4’

source("03_Analysis/modelling_adults/functions.R") #### Need of functiosn which allow the interpretation of 


########################### 
#########'Interpretation of the GLMMS results for the presence and abundance
########################### 

#### Open results of GLMMs
glmm_univ_presence <- read.csv('02_Data/processed_data/glmm_univ_presence.csv', stringsAsFactors = F)
glmm_univ_abundance <- read.csv('02_Data/processed_data/glmm_univ_abundance.csv', stringsAsFactors = F)

#### Adding the indicator
glmm_univ_presence$indicator <- 'presence'
glmm_univ_abundance$indicator <- 'abundance'


########################### 
#########'Macro-meteorological data preceding collection (CMM)
########################### 

#### Meteorological data from the ODEE

univ_glmm_temporal_ode <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>% ## to put together presence and abundance models
  filter(grepl("RFDode|TMINode|TMAXode|TMNode|GDDjour|GDDsemaine|RHmf|WINDmf",term)) %>% ## to select variables of interest: Minimum temperature, maximum temperayure, average temperature, GDD and Rainfall
  filter(!grepl("collection|prec",term)) %>% ## to not select micro climatic data 
  mutate(var = sub('\\_.*', '', term)) %>%
  mutate(label = case_when(var == "RFDode" ~ "Rainfall",
                           var == "TMINode" ~ "Minimum temperature",
                           var == "TMAXode" ~ "Maximum temperature",
                           var == "TMNode" ~ "Average temperature",
                           var == "GDDjour" ~ "GDD daily", 
                           var == "GDDsemaine" ~ "GDD weekly",
                           var == "RHmf" ~ "Relative humidity",
                           var == "WINDmf" ~ "Wind speed")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', term)), time_lag_2 = as.numeric(stringr::str_match( term, '([^_]+)(?:_[^_]+){1}$')[,2])) %>% ## adding the time lag values
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = estimate) %>%
  mutate(correlation = ifelse(p.value<=0.2,correlation,NA)) %>% ## to put NA if p-value not significant (p-value >0.2)
  mutate(r2 = ifelse(p.value<=0.2,r2,NA)) %>% ## to put NA if p-value not significant (p-value >0.2)
  nest(-c(indicator,var))

plots_univ_glmm_temporal_ode <- univ_glmm_temporal_ode %>% ## to use the function to put in CCMs format
  arrange(rev(indicator),factor(var, levels = c("RFDode","TMNode","TMINode","TMAXode","GDDsemaine","GDDjour","RHmf","WINDmf"))) %>% 
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot_r2(correlation_df = ..1, var = ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "glmm"))) %>%
  nest(-indicator) %>%
  mutate(univ_temporal = map(data, ~wrap_plots(.x$univ_temporal, nrow = 1, ncol = 8,guides = "collect"))) %>%
  dplyr::select(-data)

p_meteo_ode <- wrap_plots(plots_univ_glmm_temporal_ode$univ_temporal[1][[1]],plots_univ_glmm_temporal_ode$univ_temporal[2][[1]], ncol = 1, nrow = 2, tag_level = "new") ## to put together the different plots

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/cross_correlation_maps_meteo.pdf",plot = p_meteo_ode, device = "pdf",width = 11, height = 8) ## to save the plot


########################### 
#########'Air quality data preceding collection (CMM)
########################### 


univ_glmm_temporal_pol <-  glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>% ## putting together presence and abundance models 
  filter(grepl("NO|NOX|NO2|PM2.5|PM10|O3",term)) %>% ## to select pollluants
  filter(!grepl("collection|prec",term)) %>%
  mutate(var = sub('\\_.*', '', term)) %>%
  mutate(label = case_when(var == "NO" ~ "Nitrogen monoxide",
                           var == "NO2" ~ "Nitrogen dioxide",
                           var=="PM2.5"~"Particule matter of 2.5",
                           var == "PM10" ~ "Particule matter of 10",
                           var=="O3"~"tropospheric ozone")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', term)), time_lag_2 = as.numeric(stringr::str_match( term, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = estimate) %>%
  mutate(correlation = ifelse(p.value<=0.2,correlation,NA)) %>% ## to select significative correlations with p-value <)=0
  mutate(r2 = ifelse(p.value<=0.2,r2,NA)) %>% ## to select significative correlations with p-value <)=0
  nest(-c(indicator,var))

plots_univ_glmm_temporal_pol <- univ_glmm_temporal_pol %>% ## to use the functions of CCM
  arrange(rev(indicator),factor(var, levels = c("NO","NO2", "PM2.5", "PM10", "O3"))) %>%
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot_r2(correlation_df = ..1, var = ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "glmm"))) %>%
  nest(-indicator) %>%
  mutate(univ_temporal = map(data, ~wrap_plots(.x$univ_temporal, nrow = 1, ncol = 7,guides = "collect"))) %>%
  #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
  dplyr::select(-data)


p_poll <- wrap_plots(plots_univ_glmm_temporal_pol$univ_temporal[1][[1]],plots_univ_glmm_temporal_pol$univ_temporal[2][[1]], ncol = 1, nrow = 2, tag_level = "new") 

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/cross_correlation_maps_polluants.pdf",plot = p_poll, device = "pdf",width = 11, height = 8) ## to save



########################### 
#########'Micro climatic data preceding collection 
########################### 
univ_glmm_microclim <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(p.value<0.2)%>%
  filter(grepl("RFSUM|TMEAN|TMIN|TMAX|RHMEAN|RHMIN|RHMAX|Patmean|Patmin|Patmax|Patm_diff_prev_day",term)) %>%
  filter(grepl("collection|prec",term)) %>%
  mutate(type = sub('\\_.*', '', term)) %>%
  mutate(type = gsub("RFSUM|RFMAX|RFMIN","Rainfall",type)) %>%
  mutate(type = gsub("TMEAN|TMIN|TMAX|TAMP","Temperature",type)) %>%
  mutate(type = gsub("RHMEAN|RHMIN|RHMAX","Humidity",type)) %>%
  mutate(type=gsub("Patmean|Patmin|Patmax|Patm_diff_prev_day|Patm_daily_range", "Patmos", type))%>%
  mutate(type=ifelse(type=="Patm", "Patmos", type))%>%
  mutate(buffer = case_when(grepl("collection",term) ~ "during coll.",
                            grepl("24h_48h",term) ~ "24h and 48h before coll.",
                            grepl("24h",term) ~ "24h",
                            grepl("48h",term) ~ "48h",
                            grepl("1s",term) ~ "1 week",
                            grepl("2s",term) ~ "2 week",
                            grepl("3s",term) ~ "3 week",
                            grepl("4s",term) ~ "4 week",
                            grepl("5s",term) ~ "5 week",
                            grepl("6s",term) ~ "6 week")) %>%
  mutate(term = gsub("collection","during collection",term)) %>%
  mutate(term = gsub("24hprec|24h_prec","24h preceding collection",term)) %>%
  mutate(term = gsub("24h_48h_","24h and 48h preceding collection",term)) %>%
  mutate(term = gsub("48hprec|48h_prec","48h preceding collection",term)) %>%
  mutate(term = gsub("1s_prec|1sprec","the week preceding collection",term)) %>%
  mutate(term = gsub("2s_prec|2sprec","b/w 0 and 2 weeks preceding collection",term)) %>%
  mutate(term = gsub("3s_prec|3sprec","b/w 0 and 3 weeks preceding collection",term)) %>%
  mutate(term = gsub("4s_prec|4sprec","b/w 0 and 4 weeks preceding collection",term)) %>%
  mutate(term = gsub("5s_prec|5sprec","b/w 0 and 5 weeks preceding collection",term)) %>%
  mutate(term = gsub("6s_prec|6sprec","b/w 0 and 6 weeks preceding collection",term)) %>%
  mutate(label = case_when(grepl("MAX|max",term) ~ "Maximum",
                           grepl("MIN|min",term) ~ "Minimum",
                           grepl("AMP",term) ~ "Amplitude",
                           grepl("MEAN|SUM|mean",term) ~ "Average",
                           grepl("diff_prev_day", term)~"Amplitude difference with precedent day",
                           grepl("daily_range", term)~"Amplitude difference with same day")) %>%
  mutate(term = gsub("RFSUM","Cumulative/average",term)) %>%
  mutate(term = gsub("RFMAX","Maximum",term)) %>%
  mutate(term = gsub("RFMIN","Minimum",term)) %>%
  mutate(term = gsub("TMEAN","Cumulative/average",term)) %>%
  mutate(term = gsub("TMIN","Minimum",term)) %>%
  mutate(term = gsub("TMAX","Maximum",term)) %>%
  mutate(term = gsub("TAMP","Amplitude",term)) %>%
  mutate(term = gsub("RHMEAN","Cumulative/average",term)) %>%
  mutate(term = gsub("RHMIN","Minimum",term)) %>%
  mutate(term = gsub("RHMAX","Maximum",term)) %>%
  mutate(term = gsub("Patmean","Cumulative/average",term)) %>%
  mutate(term = gsub("Patmin","Minimum",term)) %>%
  mutate(term = gsub("Patmax","Maximum",term)) %>%
  mutate(term = gsub("Patm_diff_prev_day","Amplitude difference with precedent day",term)) %>%
  mutate(term = gsub("Patm_daily_range","Amplitude difference with same day",term))%>%
  mutate(term = gsub("_"," ",term)) %>%
  mutate(correlation = ifelse(p.value<=0.2,estimate,NA)) %>%
  mutate(r2 = ifelse(p.value<=0.2,r2,NA)) %>%
  mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance"))) %>%
  mutate(label = forcats::fct_relevel(label, c("Minimum","Maximum","Average","Amplitude", "Amplitude difference with precedent day", "Amplitude difference with same day"))) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("during coll.","24h","24h and 48h before coll.","48h","1 week","2 week","3 week","4 week","5 week","6 week")))


univ_glmm_microclim <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>% ## To put together presence and abundance models
  filter(grepl("RFSUM|TMEAN|TMIN|TMAX|RHMEAN|RHMIN|RHMAX|Patmean|Patmin|Patmax|Patm_diff_prev_day",term)) %>% ## To select the variables of interest: relative humidity, temperature, rainfall and atmospheric pressure
  filter(grepl("collection|prec",term))  %>%
  mutate(type = sub('\\_.*', '', term)) %>%
  mutate(type = gsub("RFSUM|RFMAX|RFMIN","Rainfall",type)) %>%
  mutate(type = gsub("TMEAN|TMIN|TMAX|TAMP","Temperature",type)) %>%
  mutate(type = gsub("RHMEAN|RHMIN|RHMAX","Humidity",type)) %>%
  mutate(type=gsub("Patmean|Patmin|Patmax|Patm_diff_prev_day|Patm_daily_range", "Patmos", type))%>%
  mutate(type=ifelse(type=="Patm", "Patmos", type))%>%
  mutate(buffer = case_when(grepl("collection",term) ~ "during coll.", ## to name different time lags
                            grepl("24h_48h",term) ~ "24h and 48h before coll.",
                            grepl("24h",term) ~ "24h",
                            grepl("48h",term) ~ "48h")) %>%
  mutate(label = case_when(grepl("MAX|max",term) ~ "Maximum",
                           grepl("MIN|min",term) ~ "Minimum",
                           grepl("AMP",term) ~ "Amplitude",
                           grepl("MEAN|SUM|mean",term) ~ "Average",
                           grepl("diff_prev_day", term)~"Amplitude difference with precedent day",
                           grepl("daily_range", term)~"Amplitude difference with same day")) %>%
  mutate(term = gsub("RFSUM","Cumulative/average",term)) %>%
  mutate(term = gsub("RFMAX","Maximum",term)) %>%
  mutate(term = gsub("RFMIN","Minimum",term)) %>%
  mutate(term = gsub("TMEAN","Cumulative/average",term)) %>%
  mutate(term = gsub("TMIN","Minimum",term)) %>%
  mutate(term = gsub("TMAX","Maximum",term)) %>%
  mutate(term = gsub("TAMP","Amplitude",term)) %>%
  mutate(term = gsub("RHMEAN","Cumulative/average",term)) %>%
  mutate(term = gsub("RHMIN","Minimum",term)) %>%
  mutate(term = gsub("RHMAX","Maximum",term)) %>%
  mutate(term = gsub("Patmean","Cumulative/average",term)) %>%
  mutate(term = gsub("Patmin","Minimum",term)) %>%
  mutate(term = gsub("Patmax","Maximum",term)) %>%
  mutate(term = gsub("Patm_diff_prev_day","Amplitude difference with precedent day",term)) %>%
  mutate(term = gsub("Patm_daily_range","Amplitude difference with same day",term))%>%
  mutate(term = gsub("_"," ",term)) %>%
  mutate(correlation = ifelse(p.value<=0.2,estimate,NA)) %>% ## to put NA if p-value > 0.2
  mutate(r2 = ifelse(p.value<=0.2,r2,NA)) %>% ## to put NA if p-value > 0.2
  mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance"))) %>%
  mutate(label = forcats::fct_relevel(label, c("Minimum","Maximum","Average","Amplitude", "Amplitude difference with precedent day", "Amplitude difference with same day"))) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("during coll.","24h","24h and 48h before coll.","48h")))

p_microclim <- fun_plot_tile_univ_spatial_r2(univ_glmm_microclim, metric_name = "glmm", indicator = univ_glmm_microclim$term, lc_source = "Microclimatic conditions", type = "", xlabel = "time before collection") ## to use the function to allow graphical interpretation

ggsave(filename = "02_Data/processed_data/microclim.pdf",plot = p_microclim, device = "pdf", width = 11, height = 8) ## to save 


########################### 
#########'landscape metrics with different buffers
###########################

landcover_grouped_veget_data_dict <- read.csv("02_Data/processed_data/03_Landcover_Data/landcover_grouped_veget_data_dic.csv", stringsAsFactors = F) %>% mutate(lc_source = "LCG")
data_dic <- read.csv("02_Data/processed_data/01_Adults_Abundance/data_dictionary.csv", stringsAsFactors = F) 

lsm_data_dic <- landcover_grouped_veget_data_dict ## to create data dictionnary for landscape metrics to interprate different class
  mutate(class = as.character(class))


univ_glmm_lsm <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("lsm",term)) %>% ## to put together presence and abundance models
  filter(grepl("np|te|c_area_mn|pland|shdi",term)) %>%
  mutate(function_name = sub("\\_L.*", "", term), lc_source = word(term, -3, sep = "_"), buffer = word(term, -2, sep = "_"), class = word(term, -1, sep = "_")) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("0","20","50","100","250"))) %>%## different buffer : 20 m, 50 m, 100 and 250 m
  left_join(list_lsm()) %>%
  left_join(lsm_data_dic) %>% ## to join the landscape metrics interpretation 
  rename(correlation = estimate) %>%
  mutate(label = ifelse(level=="landscape","landscape",label)) %>%
  mutate(label = paste0(label, " - ", metric," - ",name)) %>%
  mutate(type=ifelse(grepl("Vegetation", label), "Vegetation variables", "Land cover variables"))%>%
  nest(-lc_source)

plots_univ_glmm_spatial <- univ_glmm_lsm %>%
  #mutate(univ_spatial = pmap(list(data,lc_source,type), ~fun_plot_tile_univ_spatial(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator, lc_source = ..2, type = ..3))) %>%
  mutate(univ_spatial = pmap(list(data,lc_source), ~fun_plot_tile_univ_spatial_r2(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator, lc_source = ..2, type = "", xlabel = "buffer radius around the collection site (meters)"))) %>%
  dplyr::select(-data)


plots_univ_glmm_spatial <- univ_glmm_lsm %>%
  mutate(univ_spatial = pmap(list(data,lc_source), ~fun_plot_tile_univ_spatial_r2(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator, lc_source = ..2, type = "", xlabel = "buffer radius around the collection site (meters)"))) %>% ## to use the function to interprate graphically the GLMMs
  dplyr::select(-data)

pmap(list(plots_univ_glmm_spatial$lc_source,plots_univ_glmm_spatial$univ_spatial),~ggsave(filename = paste0("02_Data/processed_data/",..1,".pdf"),plot = ..2, device = "pdf", width = 7,height = 14)) ## to save

########################### 
#########'Socio economics, environmental data
###########################

fil <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("FIL|ZONE|POP_250_sum",term)) %>%
  mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance"))) %>%
  mutate(estimate = ifelse(indicator=="abundance",estimate,estimate-1)) %>%
  mutate(r2_modif=dplyr::case_when(indicator=='presence'& abs(estimate)>=1~r2, indicator=='presence'& abs(estimate)<1~r2*(-1), 
                                   indicator=="abundance"&estimate>=0~r2, indicator=="abundance"&estimate<0~r2*(-1)))%>%
  filter(p.value<0.2)%>%
  mutate(p.value2 = case_when(
    p.value <= 0.001 ~ "***",
    p.value > 0.001 & p.value <= 0.01  ~  "**",
    p.value > 0.01 & p.value <= 0.05 ~ " *",
    p.value > 0.05 ~ ""
  ), BUF=1)


p <- ggplot(fil, aes(BUF, term)) + 
  geom_tile(aes(fill = r2_modif), color = "white") + 
  facet_grid(~indicator, scales="free_y", space="free_y") +
  #facet_grid(.~indicator, scales="free_y", space="free_y") +
  xlab("") + 
  ylab("") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10, face = "italic"),
        axis.text.y = element_text(size = 8)
  ) +
  #geom_text(aes(label = ifelse(is.na(correlation), "",paste(round(correlation,2), p.value2))), size = 3)
  geom_text(aes(label = ifelse(is.na(estimate), "", p.value2)), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-0.20,0.20),midpoint = 0, space = "Lab", name = "r2 ajuste", na.value="grey")


ggsave(filename = "02_Data/processed_data/filosofi.pdf",plot = p, device = "pdf")

