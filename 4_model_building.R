library(tidyverse)
library(glmmTMB)
library(purrr)
library(furrr)
library(correlation)

# open dataset containing the dependant and independent variables
df_model <- read.csv("df_model.csv")

df_model <- df_model %>% 
  mutate(PRES_ALBO = ifelse(NB_ALBO_TOT>0,1,0)) %>%
  filter(!is.na(NB_ALBO_TOT))

predictors <- setdiff(colnames(df_model), c("X", "idpointdecapture", "ID_PIEGE", "ID_COLLECTE", "DATE_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "lieu",  "NB_ALBO_TOT","PRES_ALBO"))


#############################
######### univariate modeling
#############################

###### GLMM ######

df_glmm <- df_model %>% 
  #mutate(pointdecapture2 = as.factor(paste0(ZONE,ID_PIEGE))) %>%
  dplyr::select(NB_ALBO_TOT,PRES_ALBO,ID_PIEGE,predictors) %>% 
  mutate(NB_ALBO_TOT = as.character(NB_ALBO_TOT), PRES_ALBO = as.character(PRES_ALBO)) %>%
  mutate_if(is.numeric, ~scale(., center = TRUE, scale = TRUE)) %>%  # we center and scale to be able to compare the magnitudes (centering also helps with allowing easier interpretation of variable coefficients associated with different magnitudes, e.g. when one regressor is measured on a very small order, while another on a very large order.  )
  mutate(NB_ALBO_TOT = as.numeric(NB_ALBO_TOT), PRES_ALBO = as.numeric(PRES_ALBO))

fun_compute_glmm_univ <- function(df_glmm,indicator){
  
  if(indicator == "presence"){
    #glmms_univs <- map(colnames(df[4:ncol(df)]), ~glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = binomial(link = "logit")))
    func <- function(x){
      ret <- glmmTMB(as.formula(paste0("PRES_ALBO ~ ",x," + (1|ID_PIEGE)")), data = df_glmm, family = binomial(link = "logit"))
      return(ret)
    }
  } else if (indicator == "abundance"){
    #glmms_univs <- map(colnames(df[4:ncol(df)]), ~glmmTMB(as.formula(paste0("resp_var ~ ",.x," + (1|codevillage/pointdecapture2)")), data = df, family = truncated_nbinom2))
    func <- function(x){
      ret <- glmmTMB(as.formula(paste0("NB_ALBO_TOT ~ ",x," + (1|ID_PIEGE)")), data = df_glmm, family = truncated_nbinom2)
      return(ret)
    }
  }
  
  possible_a <- possibly(func, otherwise = NA_real_)
  
  glmms_univs <- future_map(colnames(df_glmm[11:ncol(df_glmm)]), possible_a)
  
  glmm_to_rm <- NULL
  glmm_to_rm2 <- NULL
  
  for(i in 1:length(glmms_univs)){
    if(is.na(glmms_univs[[i]])){
      glmm_to_rm <- c(glmm_to_rm,i)
    } else {
      if(is.na(summary(glmms_univs[[i]])$AICtab[1])){
        glmm_to_rm2 <- c(glmm_to_rm2,i)
      }
    }
  }
  
  glmm_to_rm <- c(glmm_to_rm,glmm_to_rm2)
  glmms_univs <- glmms_univs[-glmm_to_rm]
  
  
  func2 <- function(x){
    ret <- broom.mixed::tidy(x, conf.int = TRUE,  exponentiate = ifelse(indicator == "abundance",FALSE,TRUE))
    return(ret)
  }
  
  possible_b <- possibly(func2, otherwise = NULL)
  glmms_univs <- future_map(glmms_univs, possible_b) %>%
    do.call(rbind.data.frame, .) %>%
    filter(effect == "fixed" & term!="(Intercept)")
  
  return(glmms_univs)
}

# presence
glmm_univ_presence <- fun_compute_glmm_univ(df_glmm, "presence")
write.csv(glmm_univ_presence,'models_output/glmm_univ_presence.csv', row.names = F)

# abundance
df_glmm <- df_glmm %>% filter(NB_ALBO_TOT>0)
glmm_univ_abundance <- fun_compute_glmm_univ(df_glmm, "abundance")
write.csv(glmm_univ_abundance,'models_output/glmm_univ_abundance.csv', row.names = F)


####### Spearman correlation #########

df_corr <- df_model %>% 
  dplyr::select(NB_ALBO_TOT,PRES_ALBO,ID_PIEGE,predictors)

fun_compute_cor_univ <- function(df_corr,indicator){
  
  if(indicator == "presence"){
    func <- function(x){
      ret <- correlation::correlation(data.frame(a=df_corr[,x],PRES_ALBO=df_corr$PRES_ALBO,c=df_corr$ID_PIEGE), method = "spearman", multilevel = TRUE, include_factors = TRUE)
      ret$Parameter1 <- x
      return(ret)
    }
  } else if (indicator == "abundance"){
    func <- function(x){
      ret <- correlation::correlation(data.frame(a=df_corr[,x],NB_ALBO_TOT=df_corr$NB_ALBO_TOT,c=df_corr$ID_PIEGE), method = "spearman", multilevel = TRUE, include_factors = TRUE)
      ret$Parameter1 <- x
      return(ret)
    }
  }
  
  possible_a <- possibly(func, otherwise = NA_real_)
  
  corr_univs <- future_map(colnames(df_corr[11:ncol(df_corr)]), possible_a)
  
  corr_to_rm <- NULL
  
  for(i in 1:length(corr_univs)){
    if(is.na(corr_univs[[i]])){
      corr_to_rm <- c(corr_to_rm,i)
    } 
  }
  
  corr_univs <- corr_univs[-corr_to_rm]
  
  corr_univs <-  do.call(rbind.data.frame, corr_univs)
  
  return(corr_univs)
  
}

# presence
corr_univ_presence <- fun_compute_cor_univ(df_corr, "presence")
write.csv(corr_univ_presence,'models_output/corr_univ_presence.csv', row.names = F)

# abundance
df_corr <- df_corr %>% filter(NB_ALBO_TOT>0)
corr_univ_abundance <- fun_compute_cor_univ(df_corr, "abundance")
write.csv(corr_univ_abundance,'models_output/corr_univ_abundance.csv', row.names = F)


#############################
####### multivariate modeling
#############################

## correlation analysis (to remove correlated variables)
## multicollinearity among predictors
fun_multicol <- function(df_model, preds){
  
  predictors_numeric <- df_model %>%
    dplyr::select(preds) %>%
    dplyr::select_if(is.numeric)
  predictors_numeric <- colnames(predictors_numeric)
  predictors_character <- df_model %>%
    dplyr::select(preds) %>%
    dplyr::select_if(is.character)
  predictors_factors <- df_model %>%
    dplyr::select(preds) %>%
    dplyr::select_if(is.factor)
  predictors_character <- unique(c(colnames(predictors_character),colnames(predictors_factors)))
  
  ## multicollinearity among predictors
  lsm_vars <- predictors_numeric[grepl("lsm", predictors_numeric)]
  other_sp_vars_num <- predictors_numeric[!grepl("lsm", predictors_numeric)]
  if(length(lsm_vars) > 1){
    lsm_vars <- fun_multicollinearity_lsm(df_model, lsm_vars)
  } else if(length(lsm_vars) == 0){
    lsm_vars = NULL
  }
  vars_multiv_num <- c(other_sp_vars_num, lsm_vars)
  vars_multiv_num <- fun_multicollinearity(df_model, vars_multiv_num)
  vars_multiv <- c(vars_multiv_num, predictors_character)
  
  return(vars_multiv)
  
}

fun_multicol(df_model,predictors)

## RF computing

