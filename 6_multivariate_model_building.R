########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(caret) ## Version ‘6.0.94’
library(CAST) ## Version ‘1.0.2’
library(ranger) ## Version ‘0.16.0’
library(correlation) ## Version ‘0.8.5’
library(fuzzySim) ## Version ‘4.10.7’

########################### Open dataset containing the dependant and independent variables

df_model <- read.csv("02_Data/processed_data/01_Adults_Abundance/df_model.csv")

df_model <- df_model %>% 
  mutate(PRES_ALBO = ifelse(NB_ALBO_TOT>0,"Presence","Absence")) %>% ## to create a "character" variable for presence or absence of Aedes albopictus
  mutate(PRES_ALBO = fct_relevel(PRES_ALBO,c("Presence","Absence"))) %>%
  mutate(PRES_ALBO_NUMERIC = ifelse(NB_ALBO_TOT>0,1,0)) %>% ## to create a numeric variable for presence or absence of Aedes albopictus
  filter(!is.na(NB_ALBO_TOT))%>%
  filter(!is.na(TMIN_collection),!is.na(RHMIN_24h_prec),!is.na(RHMAX_24h_prec))  %>% ## to delete data where micro climatic variables are not present 
  mutate(RFSUM_collection=ifelse(RFSUM_collection>0.5,1,0)) ## to specify if rainfall is present during the collection  

###########################
#########'Presence model preparation
#########'First step: to select for meteorological and pollutants variables, for every type of variable, the time lag for which the r2 was the highest. Same work is realized for micro climatic, land cover (for each buffer) and socio demographic data.
#########'Second step: to evaluate the correlation between these variables.
#########'Third step: to select the variables not correlated with the highest sense ecological. The first selection is crossed with the other selection done with the VIF with the corSelect function of the fuzzySim package to select variables with the lowest VIF. The final selection is a mixed of both methods.
###########################


##### First step: to select variables for presence models
predictors_presence <- c("RFDode_5_6", 
                         "RHmf_5_6","WINDmf_0_5", "GDDjour_1_1", 
                         "NO_3_3", "PM2.5_0_1", "PM10_0_1","O3_3_3",
                         "RHMEAN_collection", "RHMIN_24h_prec", "RHMAX_24h_prec", "RFSUM_collection", "TMEAN_collection", "TMAX_48h_prec", "TMIN_collection",
                         "lsm_c_pland_LCG_20_13",  # % of low vegetation in 20 m buffer
                         "lsm_c_pland_LCG_50_10",  # % building in 50 m
                         "lsm_c_pland_LCG_20_11",  # road in 20 m
                         "lsm_l_shdi_LCG_100_NA",# Shannon diversity index in 100 m
                          "FIL_Men_pauv", # poor households
                         "POP_250_sum") # density population in 250 m

##### Plot the bivariate relationship between presence and each selected predictor 
p_pres <- df_model %>% 
  dplyr::select(PRES_ALBO_NUMERIC,predictors_presence) %>%
  pivot_longer(-PRES_ALBO_NUMERIC) %>%
  ggplot(aes(y = PRES_ALBO_NUMERIC, x = value)) +
  geom_point() + 
  ylim(c(0,1)) +
  geom_smooth() +
  facet_wrap(.~name, scales = "free") + 
  theme_bw() + 
  ggtitle("Presence albo ~ selected variables")

ggsave("02_Data/processed_data/plots/modelling_adults_abundance/plot_presence_vs_selected_vars_poll.png",p_pres,width = 9.65, height = 6.42, units = "in") ## to save

##### Second step: identify correlated variables that are greater than 0.7 (Pearson correlation coefficient)
m <- cor(df_model[,predictors_presence], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]])


#### Third select: to select final variables using the VIF 
variables_pres_corselect<-fuzzySim::corSelect(df_model, sp.cols="PRES_ALBO_NUMERIC", var.cols=predictors_presence, coeff = TRUE,
                                    cor.thresh =  0.7,
                                    select =  "VIF", family = "binomial",
                                    use = "pairwise.complete.obs", method = "pearson", verbosity = 1)
variables_pres_corselect_fin<-variables_pres_corselect$selected.vars
# ""RFDode_5_6"            "RHmf_5_6"              "NO_3_3"                "PM10_0_1"              "RHMIN_24h_prec"        "RHMAX_24h_prec"        "RFSUM_collection"     
#"lsm_c_pland_LCG_20_13" "lsm_c_pland_LCG_50_10" "lsm_c_pland_LCG_20_11" "lsm_l_shdi_LCG_100_NA" "FIL_Men_pauv"           

## Final variables selections
predictors_presence <- c("RFDode_5_6",  "GDDjour_1_1", "WINDmf_0_5",
                         "RHMAX_24h_prec", 
                         "NO_3_3",
                         "RFSUM_collection",
                         "lsm_c_pland_LCG_20_13",  
                         "lsm_c_pland_LCG_50_10",  
                         "lsm_c_pland_LCG_20_11", 
                         "lsm_l_shdi_LCG_100_NA",
                         "POP_250_sum")
                        
#### Final data frame for the multivariate analysis
df_model_presence <- df_model %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "lieu",  "NB_ALBO_TOT", "PRES_ALBO", predictors_presence) 



###########################
#########'Abundance model preparation
#########'First step: to select for meteorological and pollutants variables, for every type of variable, the time lag for which the r2 was the highest. Same work is realized for micro climatic, land cover (for each buffer) and socio demographic data.
#########'Second step: to evaluate the correlation between these variables.
#########'Third step: to select the variables not correlated with the highest sense ecological. The first selection is crossed with the other selection done with the VIF with the corSelect function of the fuzzySim package to select variables with the lowest VIF. The final selection is a mixed of both methods.
###########################

##### First step: select variables for abundance models
predictors_abundance <- c("RFDode_6_6", 
                          "RHmf_5_6", "WINDmf_0_1",
                          "GDDjour_1_1",
                          "PM2.5_0_1", "PM10_0_1", "O3_0_0","NO2_0_0", "NOX_0_0",
                          "lsm_c_pland_LCG_20_12",  # %  vegetation higher than 3 m at 20 m
                          "lsm_c_pland_LCG_50_13",  # %  vegetation lower than 3 m at 50 m
                          "lsm_c_pland_LCG_50_10",  # % building at  100 m
                          "lsm_c_pland_LCG_100_11",  # % roads at100 m
                          "lsm_l_shdi_LCG_20_NA",   # Shannon diversity index at  20 m
                          "RHMEAN_collection", "RFSUM_48hprec", "TMEAN_collection", "TMIN_collection", "TMAX_collection",#,
                        "FIL_Log_av45" # number of building built before 1945
                         )   



df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT>0) %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "lieu",  "NB_ALBO_TOT", "PRES_ALBO", predictors_abundance)

#####  Plot the bivariate relationship between abundance and each selected predictor 
p_ab <- df_model_abundance %>% 
  dplyr::select(NB_ALBO_TOT,predictors_abundance) %>%
  pivot_longer(-NB_ALBO_TOT) %>%
  ggplot(aes(y = NB_ALBO_TOT, x = value)) +
  geom_point() + 
  geom_smooth() +
  ylim(c(0,80)) +
  facet_wrap(.~name, scales = "free_x") + 
  theme_bw() + 
  ggtitle("Abondance albo ~ variables séléctionnées")

ggsave("02_Data/processed_data/plots/modelling_adults_abundance/plot_abundance_vs_selected_vars_poll.png",p_ab,width = 9.65, height = 7.42, units = "in") ## to save

##### Second step: identify correlated variables that are greater than 0.7 (Pearson correlation coefficient)
m <- cor(df_model_abundance[,predictors_abundance], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]])

#### Third select: to select final variables using the VIF 
variables_abond_corselect<-fuzzySim::corSelect(df_model, sp.cols="NB_ALBO_TOT", var.cols=predictors_abundance, coeff = TRUE,
                                    cor.thresh =  0.7,
                                    select =  "VIF", family = "truncated_nbinom2",
                                    use = "pairwise.complete.obs", method = "pearson", verbosity = 1)
variables_abond_corselect_fin<-variables_abond_corselect$selected.vars

# RFDode_6_6"            "RHmf_5_6"              "WINDmf_0_1"            "PM10_0_1"              "O3_0_0"                "NOX_0_0"               "lsm_c_pland_LCG_50_13"
# "lsm_c_pland_LCG_50_10" "lsm_l_shdi_LCG_20_NA"  "RHMEAN_collection"     "TMAX_collection"       "FIL_Log_av45"             


## Final variables selections
predictors_abundance <- c("RFDode_6_6", "GDDjour_1_1", "WINDmf_0_1",
                          "lsm_c_pland_LCG_20_12",  
                          "lsm_c_pland_LCG_50_13",  
                          "TMAX_collection",
                          "RHMEAN_collection",
                          "NOX_0_0")
 
#### Final data frame for the multivariate analysis
df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT>0) %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "lieu",  "NB_ALBO_TOT", "PRES_ALBO", predictors_abundance)



###########################
#########'Second stage of analysis: multivariate anaylsis using a leave-one-site-out cross validation and a leave-one-session-out cross validation (but juste to valdiate and evaluate the model)
#########' For presence models
###########################

#### First step: to parameter the model: leave-one-site-out cross validation
cv_col <- "lieu"


#### Second step: It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)

indices_cv <- CAST::CreateSpacetimeFolds(df_model_presence, spacevar = cv_col, k = length(unique(unlist(df_model_presence[,cv_col])))) #### Take into acocunt spatil avariability

 ## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv", ## Definition of method sampling: cross validation
                  index = indices_cv$index,  ##  list of elements to sampling
                  indexOut = indices_cv$indexOut,##  list of items to be set aside for each resampling
                  summaryFunction = twoClassSummary,#comboSummary, ## Calcul of ROC and AUC
                  classProbs = TRUE,
                  savePredictions = 'final',
                  verboseIter = FALSE
)


#### Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the ROC
mod_presence <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_ALBO, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE") 


#### Last step: to put predictions on same data frame
df_model_presence$rowIndex <- seq(1,nrow(df_model_presence),1)
df_cv_presence <- mod_presence$pred %>%
  left_join(df_model_presence) %>%
  dplyr::select(pred,Presence,obs,ZONE,lieu,num_session,idpointdecapture, ID_PIEGE) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)

res_multiv_model_presence <- list(model = mod_presence, df_cv = df_cv_presence, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence,"02_Data/processed_data/01_Adults_Abundance/res_multiv_model_presence.rds")


#### Additionnal step: same method but with a cross validation with the numero of sampling session 

## To parameter the model: leave-one-session-out cross validation
cv_col <- "num_session"
## It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)
indices_cv <- CAST::CreateSpacetimeFolds(df_model_presence, spacevar = cv_col, k = length(unique(unlist(df_model_presence[,cv_col])))) 
## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv", 
                  index = indices_cv$index,  
                  indexOut = indices_cv$indexOut, 
                  summaryFunction = twoClassSummary,#
                  classProbs = TRUE,
                  savePredictions = 'final',
                  verboseIter = FALSE
)
## Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the ROC
mod_presence_session <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_ALBO, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE") 
## Adding prediction with session cross validation 
df_cv_presence_session<-mod_presence_session$pred %>%
  left_join(df_model_presence) %>%
  dplyr::select(pred,Presence,obs,ZONE,lieu,num_session,idpointdecapture, ID_PIEGE) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)
res_multiv_model_presence_session <- list(model = mod_presence_session, df_cv = df_cv_presence_session, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence,"02_Data/processed_data/01_Adults_Abundance/res_multiv_model_presence_session.rds")



###########################
#########'Second stage of analysis: multivariate anaylsis using a leave-one-site-out cross validation and a leave-one-session-out cross validation (but juste to valdiate and evaluate the model)
#########' For abundance models
###########################

df_model_abundance$NB_ALBO_TOT <- log(df_model_abundance$NB_ALBO_TOT) 

#### First step: to parameter the model: leave-one-site-out cross validation
cv_col <- "lieu"

#### Second step: It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)
indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, spacevar = cv_col,k = length(unique(unlist(df_model_abundance[,cv_col])))) 

## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv",
                  index = indices_cv$index, 
                  indexOut = indices_cv$indexOut,
                  savePredictions = 'final')


#### Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NB_ALBO_TOT, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")

#### Last step: to put predictions on same data frame
df_model_abundance$rowIndex <- seq(1,nrow(df_model_abundance),1)
df_cv_abundance <- mod_abundance$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,ZONE,lieu,num_session,idpointdecapture, ID_PIEGE)

res_multiv_model_abundance <- list(model = mod_abundance, df_cv = df_cv_abundance, df_mod = df_model_abundance) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance,"02_Data/processed_data/01_Adults_Abundance/res_multiv_model_abundance.rds")

#### Additionnal step: same method but with a cross validation with the numero of sampling session 

## To parameter the model: leave-one-session-out cross validation
cv_col <- "num_session"
##It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)
indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, spacevar = cv_col,k = length(unique(unlist(df_model_abundance[,cv_col])))) 
## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv",
                  index = indices_cv$index, 
                  indexOut = indices_cv$indexOut,
                  savePredictions = 'final')
## realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance_session <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NB_ALBO_TOT, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")
## Adding prediction with session cross validation 
mod_abundance_session$rowIndex <- seq(1,nrow(df_model_abundance),1)
df_cv_abundance_session <- mod_abundance_session$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,ZONE,lieu,num_session,idpointdecapture, ID_PIEGE)
res_multiv_model_abundance_session <- list(model = mod_abundance_session, df_cv = df_cv_abundance_session, df_mod = df_model_abundance)  ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance_session,"02_Data/processed_data/01_Adults_Abundance/res_multiv_model_abundance_art_session.rds")



