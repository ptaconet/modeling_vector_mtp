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


### Meteorological data 
univ_glmm_temporal <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("RFD|TMIN|TMAX|TMN",term)) %>%
  mutate(var = sub('\\_.*', '', term)) %>%
  mutate(label = case_when(var == "RFD" ~ "Rainfall",
                           var == "TMIN" ~ "Minimum temperature",
                           var == "TMAX" ~ "Maximum temperature",
                           var == "TMN" ~ "Average temperature")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', term)), time_lag_2 = as.numeric(stringr::str_match( term, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = estimate) %>%
  mutate(correlation = ifelse(p.value<=0.2,correlation,NA)) %>%
  nest(-c(indicator,var))


  plots_univ_glmm_temporal <- univ_glmm_temporal %>%
    arrange(rev(indicator),var) %>%
    mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(correlation_df = ..1, var = ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "glmm"))) %>%
    nest(-indicator) %>%
    mutate(univ_temporal = map(data, ~wrap_plots(.x$univ_temporal, nrow = 1, ncol = 4))) %>%  ## mettre nrow = 2 pour CI
    #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
    dplyr::select(-data)

wrap_plots(plots_univ_glmm_temporal$univ_temporal[1][[1]],plots_univ_glmm_temporal$univ_temporal[2][[1]], ncol = 1, nrow = 2) 
  
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
  left_join(list_lsm()) %>%
  left_join(lsm_data_dic) %>%
  rename(correlation = estimate) %>%
  mutate(label = ifelse(level=="landscape","landscape",label)) %>%
  mutate(label = paste0(label, " - ", metric," - ",name)) %>%
  nest(-lc_source)

plots_univ_glmm_spatial <- univ_glmm_lsm %>%
  mutate(univ_spatial = pmap(list(data), ~fun_plot_tile_univ_spatial(correlation_df = ..1, metric_name = "glmm", indicator = ..1$indicator))) %>%
  dplyr::select(-data)

univ_glmm_other_spat <- glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(!grepl("lsm|RFD|TMIN|TMAX|TMN",term)) %>%
  mutate(buffer = ifelse(!grepl("BATH|BATS", term), word(term, 2, sep = "_"), word(term, 3, sep = "_"))) %>%
  mutate( var =  word(term, 1, sep = "_")) %>%
  filter(!grepl("LCZ",term)) %>%
  filter(!grepl("nearest",buffer)) %>%
  filter(var!="FIL") %>%
  mutate(buffer = ifelse(is.na(buffer), 0, buffer)) %>%
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
  

fun_plot_tile_univ_spatial(correlation_df = univ_glmm_other_spat, metric_name = "glmm", indicator = univ_glmm_other_spat$indicator)

# FILOSOFI
glmm_univ_presence %>%
  bind_rows(glmm_univ_abundance) %>%
  filter(grepl("FIL",term)) 







########################
#### Spearman univariate
########################

# open results
corr_univ_presence <- read.csv('models_output/corr_univ_presence.csv', stringsAsFactors = F)
corr_univ_abundance <- read.csv('models_output/corr_univ_abundance.csv', stringsAsFactors = F)

corr_univ_presence$indicator <- 'presence'
corr_univ_abundance$indicator <- 'abundance'

### Meteorological data 
univ_spearman_temporal <- corr_univ_presence %>%
  bind_rows(corr_univ_abundance) %>%
  filter(grepl("RFD|TMIN|TMAX|TMN",Parameter1)) %>%
  mutate(var = sub('\\_.*', '', Parameter1)) %>%
  mutate(label = case_when(var == "RFD" ~ "Rainfall",
                           var == "TMIN" ~ "Minimum temperature",
                           var == "TMAX" ~ "Maximum temperature",
                           var == "TMN" ~ "Average temperature")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', Parameter1)), time_lag_2 = as.numeric(stringr::str_match( Parameter1, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
  arrange(var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = r) %>%
  mutate(correlation = ifelse(p<=0.2,correlation,NA)) %>%
  mutate(abs_corr = abs(correlation)) %>%
  nest(-c(indicator,var))

plots_univ_spearman_temporal <- univ_spearman_temporal %>%
  arrange(rev(indicator),var) %>%
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(correlation_df = ..1, var = ..1$label[1], time_frame = 1, indicator = ..2, metric_name = "Spearman"))) %>%
  nest(-indicator) %>%
  mutate(univ_temporal = map(data, ~wrap_plots(.x$univ_temporal, nrow = 1, ncol = 3))) %>%
  #mutate(univ_temporal = pmap(list(univ_temporal,species,country,indicator), ~..1 + plot_annotation(title = paste(..2,..3,..4, sep = " - "), subtitle = paste("CCM using spearman coefficient"), caption = "Only significant results (pval <= 0.05) are displayed (non-signficant results are greyed out)"))) %>%
  dplyr::select(-data)

wrap_plots(plots_univ_spearman_temporal$univ_temporal[1][[1]],plots_univ_spearman_temporal$univ_temporal[2][[1]], ncol = 1, nrow = 2) 

### Spatial data 

univ_spearman_spatial <- corr_univ_presence %>%
  bind_rows(corr_univ_abundance) %>%
  filter(grepl("lsm",Parameter1)) %>%
  mutate(function_name = sub("\\_L.*", "", Parameter1), lc_source = word(Parameter1, -3, sep = "_"), buffer = word(Parameter1, -2, sep = "_"), class = word(Parameter1, -1, sep = "_")) %>%
  left_join(list_lsm()) %>%
  left_join(lsm_data_dic) %>%
  rename(correlation = r, p.value = p) %>%
  mutate(label = ifelse(level=="landscape","landscape",label)) %>%
  #mutate(label = paste0(label," - ",name)) %>%
  mutate(label = paste0(label, " - ", metric," - ",name)) %>%
  nest(-lc_source)

plots_univ_spearman_spatial <- univ_spearman_spatial %>%
  mutate(univ_spatial = pmap(list(data, indicator), ~fun_plot_tile_univ_spatial(correlation_df = ..1, metric_name = "Spearman", indicator = ..2))) %>%
  dplyr::select(-data)
