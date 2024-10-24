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
glmm_univ_presence <- read.csv('02_Data/processed_data/glmm_univ_presence_PT_GDD_POL_R2.csv', stringsAsFactors = F)
glmm_univ_abundance <- read.csv('02_Data/processed_data/glmm_univ_abundance_PT_GDD_POL_R2.csv', stringsAsFactors = F)

#### Adding the indicator
glmm_univ_presence$indicator <- 'presence'
glmm_univ_abundance$indicator <- 'abundance'


########################### 
#########'Macro-meteorological data preceding collection (CMM)
########################### 

#### Meteorological data from the ODEE

univ_glmm_temporal_ode <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>% ## to put together presence and abundance models
  filter(grepl("RFDode|TMINode|TMAXode|TMNode|GDDjour",term)) %>% ## to select variables of interest: Minimum temperature, maximum temperayure, average temperature, GDD and Rainfall
  filter(!grepl("collection|prec",term)) %>% ## to not select micro climatic data 
  mutate(var = sub('\\_.*', '', term)) %>%
  mutate(label = case_when(var == "RFDode" ~ "Rainfall",
                           var == "TMINode" ~ "Minimum temperature",
                           var == "TMAXode" ~ "Maximum temperature",
                           var == "TMNode" ~ "Average temperature",
                           var == "GDDjour" ~ "GDD daily")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', term)), time_lag_2 = as.numeric(stringr::str_match( term, '([^_]+)(?:_[^_]+){1}$')[,2])) %>% ## adding the time lag values
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = estimate) %>%
  mutate(correlation = ifelse(p.value<=0.2,correlation,NA)) %>% ## to put NA if p-value not significant (p-value >0.2)
  mutate(r2 = ifelse(p.value<=0.2,r2,NA)) %>% ## to put NA if p-value not significant (p-value >0.2)
  nest(-c(indicator,var))

plots_univ_glmm_temporal_ode <- univ_glmm_temporal_ode %>% ## to use the function to put in CCMs format
  arrange(rev(indicator),factor(var, levels = c("RFDode","TMNode","TMINode","TMAXode","TAMPode","GDDjour"))) %>% 
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot_r2(correlation_df = ..1, var = ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "glmm"))) %>%
  nest(-indicator) %>%
  mutate(univ_temporal = map(data, ~wrap_plots(.x$univ_temporal, nrow = 1, ncol = 8,guides = "collect"))) %>%
  dplyr::select(-data)

p_meteo_ode <- wrap_plots(plots_univ_glmm_temporal_ode$univ_temporal[1][[1]],plots_univ_glmm_temporal_ode$univ_temporal[2][[1]], ncol = 1, nrow = 2, tag_level = "new") ## to put together the different plots

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/cross_correlation_maps_ode.pdf",plot = p_meteo_ode, device = "pdf",width = 11, height = 8) ## to save the plot

#### Meteorological data from Meteo france

univ_glmm_temporal_mf <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>% ## to put together presence and abundance models
  filter(grepl("RFDmf|TMINmf|TMAXmf|TMNmf|RHmf|WINDmf",term)) %>% ## to select variables of interest: Minimum temperature, maximum temperayure, average temperature, rainfall, wind speed and relative humidity 
  filter(!grepl("collection|prec",term)) %>% # to not select micro climatic data 
  mutate(var = sub('\\_.*', '', term)) %>%
  mutate(label = case_when(var == "RFDmf" ~ "Rainfall",
                           var == "TMINmf" ~ "Minimum temperature",
                           var == "TMAXmf" ~ "Maximum temperature",
                           var == "TMNmf" ~ "Average temperature",
                           var == "RHmf" ~ "Relative humidity",
                           var == "WINDmf" ~ "Wind speed")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', term)), time_lag_2 = as.numeric(stringr::str_match( term, '([^_]+)(?:_[^_]+){1}$')[,2])) %>% ## adding the time lag values
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = estimate) %>%
  mutate(correlation = ifelse(p.value<=0.2,correlation,NA)) %>% ## to put NA if p-value not significant (p-value >0.2)
  mutate(r2 = ifelse(p.value<=0.2,r2,NA)) %>% ## to put NA if p-value not significant (p-value >0.2)
  nest(-c(indicator,var))

 
plots_univ_glmm_temporal_mf <- univ_glmm_temporal_mf %>% ## to use the function to put in CCMs format
  arrange(rev(indicator),factor(var, levels = c("RFDmf","TMNmf","TMINmf","TMAXmf","RHmf","WINDmf"))) %>%
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot_r2(correlation_df = ..1, var = ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "glmm"))) %>%
  nest(-indicator) %>%
  mutate(univ_temporal = map(data, ~wrap_plots(.x$univ_temporal, nrow = 1, ncol = 7,guides = "collect"))) %>%
  dplyr::select(-data)


p_meteo_mf <- wrap_plots(plots_univ_glmm_temporal_mf$univ_temporal[1][[1]],plots_univ_glmm_temporal_mf$univ_temporal[2][[1]], ncol = 1, nrow = 2, tag_level = "new")  ## to put together the different plots
ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/cross_correlation_maps_meteofrance.pdf",plot = p_meteo_mf, device = "pdf", width = 11, height = 8) ## to save the plot

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
                           var == "NOX" ~ "Nitrogen oxides",
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
  arrange(rev(indicator),factor(var, levels = c("NO","NO2","NOX", "PM2.5", "PM10", "O3"))) %>%
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
  bind_rows(glmm_univ_abundance) %>% ## To put together presence and abundance models
  filter(grepl("RFSUM|RFMIN|RFMAX|TMEAN|TMIN|TMAX|TAMP|RHMEAN|RHMIN|RHMAX",term)) %>% ## To select the variables of interest: relative humidity, temperature, and presence of rainfall
  filter(grepl("collection|prec",term)) %>%
  mutate(type = sub('\\_.*', '', term)) %>%
  mutate(type = gsub("RFSUM|RFMAX|RFMIN","Rainfall",type)) %>% ## to add a type for variables: variables related to rainfall
  mutate(type = gsub("TMEAN|TMIN|TMAX|TAMP","Temperature",type)) %>%## to add a type for variables: variables related to temperature
  mutate(type = gsub("RHMEAN|RHMIN|RHMAX","Humidity",type)) %>% ## to add a type for variables: variables related to relative humidity
  mutate(buffer = case_when(grepl("collection",term) ~ "during coll.", ## to name different time lags
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
  mutate(term = gsub("48hprec|48h_prec","48h preceding collection",term)) %>%
  mutate(term = gsub("1s_prec|1sprec","the week preceding collection",term)) %>%
  mutate(term = gsub("2s_prec|2sprec","b/w 0 and 2 weeks preceding collection",term)) %>%
  mutate(term = gsub("3s_prec|3sprec","b/w 0 and 3 weeks preceding collection",term)) %>%
  mutate(term = gsub("4s_prec|4sprec","b/w 0 and 4 weeks preceding collection",term)) %>%
  mutate(term = gsub("5s_prec|5sprec","b/w 0 and 5 weeks preceding collection",term)) %>%
  mutate(term = gsub("6s_prec|6sprec","b/w 0 and 6 weeks preceding collection",term)) %>%
  mutate(label = case_when(grepl("MAX",term) ~ "Maximum",
                            grepl("MIN",term) ~ "Minimum",
                           grepl("AMP",term) ~ "Amplitude",
                           grepl("MEAN|SUM",term) ~ "Average")) %>%
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
  mutate(term = gsub("_"," ",term)) %>%
  mutate(correlation = ifelse(p.value<=0.2,estimate,NA)) %>% ## to put NA if p-value > 0.2
  mutate(r2 = ifelse(p.value<=0.2,r2,NA)) %>% ## to put NA if p-value > 0.2
  mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance"))) %>%
  mutate(label = forcats::fct_relevel(label, c("Minimum","Maximum","Average","Amplitude"))) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("during coll.","24h","48h","1 week","2 week","3 week","4 week","5 week","6 week")))

p_microclim <- fun_plot_tile_univ_spatial_r2(univ_glmm_microclim, metric_name = "glmm", indicator = univ_glmm_microclim$term, lc_source = "Microclimatic conditions", type = "", xlabel = "time before collection") ## to use the function to allow graphical interpretation

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/microclim.pdf",plot = p_microclim, device = "pdf", width = 11, height = 8) ## to save 


########################### 
#########'landscape metrics with different buffers
###########################

landcover_grouped_veget_data_dict <- read.csv("02_Data/processed_data/03_Landcover_Data/landcover_grouped_veget_data_dic.csv", stringsAsFactors = F) %>% mutate(lc_source = "LCG")
data_dic <- read.csv("02_Data/processed_data/01_Adults_Abundance/data_dictionary.csv", stringsAsFactors = F) 

lsm_data_dic <- landcover_grouped_veget_data_dict ## to create data dictionnary for landscape metrics to interprate different class
  mutate(class = as.character(class))

univ_glmm_lsm <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>% ## to put together presence and abundance models
  filter(grepl("lsm_c_pland|lsm_l_shdi",term)) %>% ## to select the variables related to the percentage of every class and the complewxity of the landscape
  mutate(function_name = sub("\\_L.*", "", term), lc_source = word(term, -3, sep = "_"), buffer = word(term, -2, sep = "_"), class = word(term, -1, sep = "_")) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("0","20","50","100","250"))) %>% ## different buffer : 20 m, 50 m, 100 and 250 m
  left_join(list_lsm()) %>%
  left_join(lsm_data_dic) %>% ## to join the landscape metrics interpretation 
  rename(correlation = estimate) %>%
  mutate(label = ifelse(level=="landscape","landscape",label)) %>%
  mutate(label = paste0(label, " - ", metric," - ",name)) %>%
  nest(-lc_source)

plots_univ_glmm_spatial <- univ_glmm_lsm %>%
  mutate(univ_spatial = pmap(list(data,lc_source), ~fun_plot_tile_univ_spatial_r2(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator, lc_source = ..2, type = "", xlabel = "buffer radius around the collection site (meters)"))) %>% ## to use the function to interprate graphically the GLMMs
  dplyr::select(-data)

pmap(list(plots_univ_glmm_spatial$lc_source,plots_univ_glmm_spatial$univ_spatial),~ggsave(filename = paste0("02_Data/processed_data/plots/modelling_adults_abundance/",..1,".pdf"),plot = ..2, device = "pdf", width = 7,height = 14)) ## to save

########################### 
#########'Socio economics data
###########################

fil <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>% ## to put together presence and abundance models
  filter(grepl("FIL",term)) %>% ## to select socio economics variables
  mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance"))) %>%
  mutate(estimate = ifelse(indicator=="abundance",estimate,estimate-1)) %>%
  mutate(r2_modif=dplyr::case_when(indicator=='presence'& abs(estimate)>=1~r2, indicator=='presence'& abs(estimate)<1~r2*(-1),  ## for presence /abundance model: it highlights the direction of the correlation and adapts the marginal r2: if correlation is negative, r2 is going to be negative also.
                                   indicator=="abundance"&estimate>=0~r2, indicator=="abundance"&estimate<0~r2*(-1)))%>%
  filter(p.value<0.2) ## to select only variables significtaives (<0.2)

p2 <- ggplot(fil, aes(y = term, x = r2_modif)) + ## to represent graphically 
  geom_point() + 
  facet_wrap(.~indicator) + 
  ggtitle("FILOSOFI")

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/filosofi.jpg",plot = p2, device = "jpg") ## to save

########################### 
#########'Other spatial variables, like population density
###########################

univ_glmm_other_spat <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%## to put together presence and abundance models
  filter(!grepl("lsm|RFD|TMIN|TMAX|TMN|TAMP|RH|WIND|RFSUM|RFMIN|RFMAX|TMEAN|TMIN|TMAX|RHMEAN|RHMIN|RHMAX|GDD|NO|O3|PM",term)) %>% ## to select every variables which are not pollutant, land cover, meteorological, mciro climate and socio economics
  mutate(buffer = ifelse(!grepl("BATH|BATS", term), word(term, 2, sep = "_"), word(term, 3, sep = "_"))) %>%
  mutate( var =  word(term, 1, sep = "_")) %>%
  filter(!grepl("LCZ",term)) %>%
  filter(!grepl("nearest",buffer)) %>%
  filter(var!="FIL") %>%
  mutate(buffer = ifelse(is.na(buffer), 0, buffer)) %>% ## to indicate if a buffer is used
  mutate(buffer = forcats::fct_relevel(buffer, c("0","20","50","100","250"))) %>%
  left_join(data_dic) %>%
  rename(correlation = estimate) %>%
  mutate(type = "other spatial variables") %>%
  mutate(fun_summarize = case_when(grepl("mean|mn",term) ~ "mean" ,
                                   grepl("sd|stdev",term) ~ "sd",
                                   grepl("nb",term) ~ "nb",
                                   grepl("min",term) ~ "min",
                                   grepl("max",term) ~ "max",
                                   grepl("sum_vol|sum", term) ~ "sum",
                                   TRUE ~ "")
                                   ) %>%
  mutate(label = paste0(label," - ",fun_summarize))
  

p <- fun_plot_tile_univ_spatial_r2(correlation_df = univ_glmm_other_spat, metric_name = "glmm", indicator = univ_glmm_other_spat$indicator, lc_source = "Other spatial variables", type = "", xlabel = "buffer radius around the collection site (meters)")  ## to use the function to interprate graphically the GLMMs

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/other_spatial_var.jpg",plot = p, device = "jpg", width = 7,height = 14) ## To save



