########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(glmmTMB) ## Version ‘1.1.9’
library(purrr) ## Version ‘1.0.1’
library(furrr) ## Version ‘0.3.1’
library(correlation) ## Version ‘0.8.5’
library(caret) ## Version ‘6.0.94’
library(performance) ## Version ‘0.12.3’

########################### Open dataset containing the dependant and independent variables
df_model <- read.csv("02_Data/processed_data/01_Adults_Abundance/df_model.csv")

df_model <- df_model %>% 
  mutate(PRES_ALBO = ifelse(NB_ALBO_TOT>0,1,0)) %>%
  filter(!is.na(NB_ALBO_TOT)) %>%
  mutate(num_session=as.factor(num_session))|>
  mutate(AREA=case_when(ID_PIEGE%in%c("BG_01", "BG_02")~"Aiguelongue",ID_PIEGE%in%c("BG_03", "BG_04", "BG_05")~"Botanical Garden",  ID_PIEGE%in%c("BG_11", "BG_14")~"Lemasson", ID_PIEGE%in%c("BG_12_13")~"Soulas", ID_PIEGE%in%c("BG_15_16")~"Aiguerelles",
                        ID_PIEGE%in%c("BG_21")~"St-Charles", ID_PIEGE%in%c("BG_22")~"Bouisson Bertrand",  ID_PIEGE%in%c("BG_23") ~"Diderot", ID_PIEGE%in%c("BG_24")~"Acapulco"),
         ZONE=case_when(ID_PIEGE%in%c("BG_01", "BG_02","BG_03", "BG_04", "BG_05")~"Park", ID_PIEGE%in%c("BG_11", "BG_14", "BG_12_13", "BG_15_16")~"Residential", ID_PIEGE%in%c("BG_21","BG_22",  "BG_23", "BG_24")~"City Center"))|>
  dplyr::filter(DATE_COLLECTE>"2023-05-01")|>
  mutate(across(
    contains("lsm_c_area_mn_LCG"), 
    ~ . / 0.0001
  ))%>%
  relocate(AREA, SESSION_DAY, JOUR,ZONE, .after = num_session)

predictors <- setdiff(colnames(df_model), c("JOUR","SESSION_DAY","X.1", "X","idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "AREA",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "AREA",  "NB_ALBO_TOT","PRES_ALBO","DATE_POSE","HEURE_COLLECTE","DATE_COLLECTE",  "NB_ALBO_F",  "NB_ALBO_M"))
###########################
#########'Univariate modeling
#########'First stage of analysis: realization of analysis using GLMM between a variable response (presence/abundance) and every predictors, including two random intercepts: location of the trap +  session trapping
#########'For presence models: binomial distribution
#########'For abundance models: strictly positive number of Aedes albopictus female caught in a trap: negative binomial zero truncated
###########################

##### To prepare the data frame for the GLMMM

df_glmm <- df_model %>% 
  dplyr::select(NB_ALBO_F, NB_ALBO_M, NB_ALBO_TOT,PRES_ALBO,ID_PIEGE,AREA,SESSION_DAY, JOUR, num_session,predictors) %>% 
  mutate(NB_ALBO_TOT = as.character(NB_ALBO_TOT), PRES_ALBO = as.character(PRES_ALBO), NB_ALBO_F=as.character(NB_ALBO_F), NB_ALBO_M=as.character(NB_ALBO_M), JOUR=as.character(JOUR)) %>%
  mutate_if(is.numeric, ~scale(., center = TRUE, scale = FALSE)) %>%  # we center and scale to be able to compare the magnitudes (centering also helps with allowing easier interpretation of variable coefficients associated with different magnitudes, e.g. when one regressor is measured on a very small order, while another on a very large order.  )
  mutate(NB_ALBO_TOT = as.numeric(NB_ALBO_TOT), PRES_ALBO = as.numeric(PRES_ALBO), NB_ALBO_F=as.numeric(NB_ALBO_F), NB_ALBO_M=as.numeric(NB_ALBO_M))

##### A function which realizes all GLMMs for presence and all GLMMs for abundance

fun_compute_glmm_univ <- function(df_glmm,indicator, null_model, tolerance){
  
  if(indicator == "presence"){ ## For presence models
    func <- function(x){
      ret <- glmmTMB(as.formula(paste0("PRES_ALBO ~ ",x," + (1|AREA/ID_PIEGE) + (1|num_session)")), data = df_glmm, family = binomial(link = "logit")) ## Realization of GLMM with binomial distribution for each predictor and with 2 random effects
      return(ret)
    }
  } else if (indicator == "abundance"){ ## For abundance models
    func <- function(x){
      ret <- glmmTMB(as.formula(paste0("NB_ALBO_F ~ ",x," + (1|AREA/ID_PIEGE) + (1|num_session)")), data = df_glmm, family = truncated_nbinom2) ## Realization of GLMM with negative binomial distribution zero truncared for each predictor and with 2 random effects
      return(ret)
    }
  }
  
  possible_a <- possibly(func, otherwise = NA_real_) ## Allows to return a numeric missing value NA in stead of an error message for the function func
  
  glmms_univs <- future_map(colnames(df_glmm[5:ncol(df_glmm)]), possible_a) ## Allows to apply the function possible_a to every part of the data frame df_glmm[5:ncol(df_glmm)]: returns a list of GLMMs for presence and abundance for every predictors
  
  ## Allows to delete the missing and the empty models on the list
  glmm_to_rm <- NULL 
  glmm_to_rm2 <- NULL
 
   for(i in 1:length(glmms_univs)){
    ifelse(is.na(glmms_univs[[i]]), glmm_to_rm <- c(glmm_to_rm,i),
           ifelse(is.na(summary(glmms_univs[[i]])$AICtab[1]),c(glmm_to_rm2,i),glmms_univs[[i]]))
  }
  glmm_to_rm <- c(glmm_to_rm,glmm_to_rm2)
  glmms_univs <- glmms_univs[-glmm_to_rm]
  
  ## Function which allows to put graphically and in a tidy way the results of the glmms in a form of dataframe, 
  func2 <- function(x){
    ret <- broom.mixed::tidy(x, conf.int = TRUE,  exponentiate = ifelse(indicator == "abundance",FALSE,TRUE))
    ret$r2<-performance::r2_nakagawa(x, tolerance = tolerance, null_model = null_model)$R2_marginal ## Adding the value for the marginal R2 to evaluate the proportion of the variance explained by the fixed effect 
    return(ret)
  }
  
  possible_b <- possibly(func2, otherwise = NULL) ## function which allows to return NULL in stead of an error message
  glmms_univs <- future_map(glmms_univs, possible_b) ## return a list of table with the results of the glmms
  
  glmms_univs<-do.call(rbind.data.frame, glmms_univs) %>% ## Allows to create a data frame which every line is the results of a model
    filter(effect == "fixed" & term!="(Intercept)") ## Select only the fixed effect on the results of models
  
  return(glmms_univs)
}

##### For presence models: code make take long to run (~10 min)
null_model <- glmmTMB(PRES_ALBO ~ 1 + (1|AREA/ID_PIEGE) + (1|num_session),
                      data = df_glmm, family = binomial(link = "logit"))
tolerance = 1e-12
glmm_univ_presence <- fun_compute_glmm_univ(df_glmm, "presence", null_model, tolerance)
write.csv(glmm_univ_presence,'02_Data/processed_data/glmm_univ_presence.csv', row.names = F)

##### For abundance models: code make take long to run (~10 min)
df_glmm <- df_glmm %>% filter(NB_ALBO_F>0) ## Selecting only the strictly positive number of Aedes albopictus caught in a trap
null_model <- glmmTMB(NB_ALBO_F ~ 1 + (1|AREA/ID_PIEGE) + (1|num_session),
                      data = df_glmm, family = truncated_nbinom2)
glmm_univ_abundance <- fun_compute_glmm_univ(df_glmm, "abundance", null_model, tolerance)
tolerance = 1e-12
write.csv(glmm_univ_abundance,'02_Data/processed_data//glmm_univ_abundance.csv', row.names = F)




