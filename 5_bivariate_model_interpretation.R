library(tidyverse)
library(purrr)
library(patchwork)
library(landscapemetrics)

source("functions.R")


########################
#### GLMM univariate
########################

# open results of GLMMs
glmm_univ_presence <- read.csv('models_output/glmm_univ_presence.csv', stringsAsFactors = F)
glmm_univ_abundance <- read.csv('models_output/glmm_univ_abundance.csv', stringsAsFactors = F)

glmm_univ_presence$indicator <- 'presence'
glmm_univ_abundance$indicator <- 'abundance'


### Macro-meteorological data preceding collection (CMM)

univ_glmm_temporal_ode <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("RFDode|TMINode|TMAXode|TMNode|TAMPode|RHode|WINDode|GDD",term)) %>%
  filter(!grepl("collection|prec",term)) %>%
  mutate(var = sub('\\_.*', '', term)) %>%
  mutate(label = case_when(var == "RFDode" ~ "Rainfall",
                           var == "TMINode" ~ "Minimum temperature",
                           var == "TMAXode" ~ "Maximum temperature",
                           var == "TMNode" ~ "Average temperature",
                           var == "TAMPode" ~ "Temperature amplitude",
                           var == "GDDjour" ~ "GDD daily",
                           var == "GDDacc" ~ "GDD cumulated",
                           var == "GDDbound" ~ "GDD bounded")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', term)), time_lag_2 = as.numeric(stringr::str_match( term, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = estimate) %>%
  mutate(correlation = ifelse(p.value<=0.2,correlation,NA)) %>%
  nest(-c(indicator,var))

plots_univ_glmm_temporal_ode <- univ_glmm_temporal_ode %>%
  arrange(rev(indicator),factor(var, levels = c("RFDode","TMNode","TMINode","TMAXode","TAMPode","GDDjour","GDDacc","GDDbound"))) %>%
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(correlation_df = ..1, var = ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "glmm"))) %>%
  nest(-indicator) %>%
  mutate(univ_temporal = map(data, ~wrap_plots(.x$univ_temporal, nrow = 1, ncol = 8))) %>%
  #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
  dplyr::select(-data)

p_meteo_ode <- wrap_plots(plots_univ_glmm_temporal_ode$univ_temporal[1][[1]],plots_univ_glmm_temporal_ode$univ_temporal[2][[1]], ncol = 1, nrow = 2, tag_level = "new") 
ggsave(filename = "plots/cross_correlation_maps_ode.pdf",plot = p_meteo_ode, device = "pdf")


univ_glmm_temporal_mf <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("RFDmf|TMINmf|TMAXmf|TMNmf|TAMPmf|RHmf|WINDmf",term)) %>%
  filter(!grepl("collection|prec",term)) %>%
  mutate(var = sub('\\_.*', '', term)) %>%
  mutate(label = case_when(var == "RFDmf" ~ "Rainfall",
                           var == "TMINmf" ~ "Minimum temperature",
                           var == "TMAXmf" ~ "Maximum temperature",
                           var == "TMNmf" ~ "Average temperature",
                           var == "TAMPmf" ~ "Temperature amplitude",
                           var == "RHmf" ~ "Relative humidity",
                           var == "WINDmf" ~ "Wind speed")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', term)), time_lag_2 = as.numeric(stringr::str_match( term, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = estimate) %>%
  mutate(correlation = ifelse(p.value<=0.2,correlation,NA)) %>%
  nest(-c(indicator,var))
  

  plots_univ_glmm_temporal_mf <- univ_glmm_temporal_mf %>%
    arrange(rev(indicator),factor(var, levels = c("RFDmf","TMNmf","TMINmf","TMAXmf","TAMPmf","RHmf","WINDmf"))) %>%
    mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(correlation_df = ..1, var = ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "glmm"))) %>%
    nest(-indicator) %>%
    mutate(univ_temporal = map(data, ~wrap_plots(.x$univ_temporal, nrow = 1, ncol = 7))) %>%
    #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
    dplyr::select(-data)


p_meteo_mf <- wrap_plots(plots_univ_glmm_temporal_mf$univ_temporal[1][[1]],plots_univ_glmm_temporal_mf$univ_temporal[2][[1]], ncol = 1, nrow = 2, tag_level = "new") 
ggsave(filename = "plots/cross_correlation_maps_meteofrance.pdf",plot = p_meteo_mf, device = "pdf")


### Micro-climatic data
univ_glmm_microclim <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("RFSUM|RFMIN|RFMAX|TMEAN|TMIN|TMAX|RHMEAN|RHMIN|RHMAX",term)) %>%
  filter(grepl("collection|prec",term)) %>%
  mutate(type = sub('\\_.*', '', term)) %>%
  mutate(type = gsub("RFSUM|RFMAX|RFMIN","Rainfall",type)) %>%
  mutate(type = gsub("TMEAN|TMIN|TMAX","Temperature",type)) %>%
  mutate(type = gsub("RHMEAN|RHMIN|RHMAX","Humidity",type)) %>%
  mutate(buffer = case_when(grepl("collection",term) ~ "during coll.",
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
                            grepl("MEAN|SUM",term) ~ "Average")) %>%
  mutate(term = gsub("RFSUM","Cumulative/average",term)) %>%
  mutate(term = gsub("RFMAX","Maximum",term)) %>%
  mutate(term = gsub("RFMIN","Minimum",term)) %>%
  mutate(term = gsub("TMEAN","Cumulative/average",term)) %>%
  mutate(term = gsub("TMIN","Minimum",term)) %>%
  mutate(term = gsub("TMAX","Maximum",term)) %>%
  mutate(term = gsub("RHMEAN","Cumulative/average",term)) %>%
  mutate(term = gsub("RHMIN","Minimum",term)) %>%
  mutate(term = gsub("RHMAX","Maximum",term)) %>%
  mutate(term = gsub("_"," ",term)) %>%
  mutate(correlation = ifelse(p.value<=0.2,estimate,NA)) %>%
  mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance"))) %>%
  mutate(label = forcats::fct_relevel(label, c("Minimum","Maximum","Average"))) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("during coll.","24h","48h","1 week","2 week","3 week","4 week","5 week","6 week")))

p_microclim <- fun_plot_tile_univ_spatial(univ_glmm_microclim, metric_name = "glmm", indicator = univ_glmm_microclim$term, lc_source = "Microclimatic conditions", type = "", xlabel = "time before collection")

ggsave(filename = "plots/microclim.pdf",plot = p_microclim, device = "pdf")


### Spatial data - landscape metrics

landuse_data_dict <- read.csv("data/processed_data/landuse_data_dic.csv", stringsAsFactors = F) %>% mutate(lc_source = "LUS")
landcover_data_dict <- read.csv("data/processed_data/landcover_data_dic.csv", stringsAsFactors = F) %>% mutate(lc_source = "LCV")
landcover_grouped_veget_data_dict <- read.csv("data/processed_data/landcover_grouped_veget_data_dic.csv", stringsAsFactors = F) %>% mutate(lc_source = "LCG")
data_dic <- read.csv("data_dictionary.csv", stringsAsFactors = F) 

lsm_data_dic <- landuse_data_dict %>% 
  bind_rows(landcover_data_dict) %>%
  bind_rows(landcover_grouped_veget_data_dict) %>%
  mutate(class = as.character(class))

univ_glmm_lsm <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("lsm",term)) %>%
  mutate(function_name = sub("\\_L.*", "", term), lc_source = word(term, -3, sep = "_"), buffer = word(term, -2, sep = "_"), class = word(term, -1, sep = "_")) %>%
  mutate(buffer = forcats::fct_relevel(buffer, c("0","20","50","100","250"))) %>%
  left_join(list_lsm()) %>%
  left_join(lsm_data_dic) %>%
  rename(correlation = estimate) %>%
  mutate(label = ifelse(level=="landscape","landscape",label)) %>%
  mutate(label = paste0(label, " - ", metric," - ",name)) %>%
  #nest(-c(lc_source,type))
  nest(-lc_source)

plots_univ_glmm_spatial <- univ_glmm_lsm %>%
  #mutate(univ_spatial = pmap(list(data,lc_source,type), ~fun_plot_tile_univ_spatial(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator, lc_source = ..2, type = ..3))) %>%
  mutate(univ_spatial = pmap(list(data,lc_source), ~fun_plot_tile_univ_spatial(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator, lc_source = ..2, type = "", xlabel = "buffer radius around the collection site (meters)"))) %>%
  dplyr::select(-data)


#pmap(list(plots_univ_glmm_spatial$lc_source,plots_univ_glmm_spatial$type,plots_univ_glmm_spatial$univ_spatial),~ggsave(filename = paste0("plots/",..1,"_",..2,".jpg"),plot = ..3, device = "jpg"))

pmap(list(plots_univ_glmm_spatial$lc_source,plots_univ_glmm_spatial$univ_spatial),~ggsave(filename = paste0("plots/",..1,".pdf"),plot = ..2, device = "pdf", width = 7,height = 14))


univ_glmm_other_spat <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(!grepl("lsm|RFD|TMIN|TMAX|TMN|TAMP|RH|WIND|RFSUM|RFMIN|RFMAX|TMEAN|TMIN|TMAX|RHMEAN|RHMIN|RHMAX",term)) %>%
  mutate(buffer = ifelse(!grepl("BATH|BATS", term), word(term, 2, sep = "_"), word(term, 3, sep = "_"))) %>%
  mutate( var =  word(term, 1, sep = "_")) %>%
  filter(!grepl("LCZ",term)) %>%
  filter(!grepl("nearest",buffer)) %>%
  filter(var!="FIL") %>%
  mutate(buffer = ifelse(is.na(buffer), 0, buffer)) %>%
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
  

p <- fun_plot_tile_univ_spatial(correlation_df = univ_glmm_other_spat, metric_name = "glmm", indicator = univ_glmm_other_spat$indicator, lc_source = "Other spatial variables", type = "", xlabel = "buffer radius around the collection site (meters)")

ggsave(filename = "plots/other_spatial_var.jpg",plot = p, device = "jpg")

# FILOSOFI
fil <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("FIL",term)) %>%
  mutate(indicator = forcats::fct_relevel(indicator, c("presence","abundance"))) %>%
  mutate(estimate = ifelse(indicator=="abundance",estimate,estimate-1)) %>%
  filter(p.value<0.2)


p2 <- ggplot(fil, aes(y = term, x = estimate)) + 
  geom_point() + 
  facet_wrap(.~indicator) + 
  ggtitle("FILOSOFI")

ggsave(filename = "plots/filosofi.jpg",plot = p2, device = "jpg")


# 
# ########################
# #### Spearman univariate
# ########################
# 
# # open results
# corr_univ_presence <- read.csv('models_output/corr_univ_presence.csv', stringsAsFactors = F)
# corr_univ_abundance <- read.csv('models_output/corr_univ_abundance.csv', stringsAsFactors = F)
# 
# corr_univ_presence$indicator <- 'presence'
# corr_univ_abundance$indicator <- 'abundance'
# 
# ### Meteorological data 
# univ_spearman_temporal <- corr_univ_presence %>%
#   bind_rows(corr_univ_abundance) %>%
#   filter(grepl("RFD|TMIN|TMAX|TMN",Parameter1)) %>%
#   mutate(var = sub('\\_.*', '', Parameter1)) %>%
#   mutate(label = case_when(var == "RFD" ~ "Rainfall",
#                            var == "TMIN" ~ "Minimum temperature",
#                            var == "TMAX" ~ "Maximum temperature",
#                            var == "TMN" ~ "Average temperature")) %>%
#   mutate(time_lag_1 = as.numeric(sub('.*\\_', '', Parameter1)), time_lag_2 = as.numeric(stringr::str_match( Parameter1, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
#   arrange(var, indicator, time_lag_1, time_lag_2) %>%
#   rename(correlation = r) %>%
#   mutate(correlation = ifelse(p<=0.2,correlation,NA)) %>%
#   mutate(abs_corr = abs(correlation)) %>%
#   nest(-c(indicator,var))
# 
# plots_univ_spearman_temporal <- univ_spearman_temporal %>%
#   arrange(rev(indicator),var) %>%
#   mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(correlation_df = ..1, var = ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "Spearman"))) %>%
#   nest(-indicator) %>%
#   mutate(univ_temporal = map(data, ~wrap_plots(.x$univ_temporal, nrow = 1, ncol = 3))) %>%
#   #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
#   dplyr::select(-data)
# 
# wrap_plots(plots_univ_spearman_temporal$univ_temporal[1][[1]],plots_univ_spearman_temporal$univ_temporal[2][[1]], ncol = 1, nrow = 2) 
# 
# ### Spatial data 
# 
# univ_spearman_spatial <- corr_univ_presence %>%
#   bind_rows(corr_univ_abundance) %>%
#   filter(grepl("lsm",Parameter1)) %>%
#   mutate(function_name = sub("\\_L.*", "", Parameter1), lc_source = word(Parameter1, -3, sep = "_"), buffer = word(Parameter1, -2, sep = "_"), class = word(Parameter1, -1, sep = "_")) %>%
#   left_join(list_lsm()) %>%
#   left_join(lsm_data_dic) %>%
#   rename(correlation = r, p.value = p) %>%
#   mutate(label = ifelse(level=="landscape","landscape",label)) %>%
#   #mutate(label = paste0(label," - ",name)) %>%
#   mutate(label = paste0(label, " - ", metric," - ",name)) %>%
#   nest(-lc_source)
# 
# plots_univ_spearman_spatial <- univ_spearman_spatial %>%
#   mutate(univ_spatial = pmap(list(data, indicator), ~fun_plot_tile_univ_spatial(correlation_df = ..1, metric_name = "Spearman", indicator = ..2))) %>%
#   dplyr::select(-data)
