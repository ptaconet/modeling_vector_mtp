########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(caret) ## Version ‘6.0.94’
library(CAST) ## Version ‘1.0.2’
library(ranger) ## Version ‘0.16.0’
library(correlation) ## Version ‘0.8.5’
library(fuzzySim) ## Version ‘4.10.7’

########################### Open dataset containing the dependant and independent variables

df_model <- read.csv("02_Data/processed_data/01_Adults_Abundance/df_model.csv")

ddf_model <- df_model %>% 
  mutate(num_session=as.factor(num_session))|>
  mutate(AREA=case_when(ID_PIEGE%in%c("BG_01", "BG_02")~"Aiguelongue",ID_PIEGE%in%c("BG_03", "BG_04", "BG_05")~"Botanical Garden",  ID_PIEGE%in%c("BG_11", "BG_14")~"Lemasson", ID_PIEGE%in%c("BG_12_13")~"Soulas", ID_PIEGE%in%c("BG_15_16")~"Aiguerelles",
                        ID_PIEGE%in%c("BG_21")~"St-Charles", ID_PIEGE%in%c("BG_22")~"Bouisson Bertrand",  ID_PIEGE%in%c("BG_23") ~"Diderot", ID_PIEGE%in%c("BG_24")~"Acapulco"),
         ZONE=as.factor(case_when(ID_PIEGE%in%c("BG_01", "BG_02","BG_03", "BG_04", "BG_05")~"Park", ID_PIEGE%in%c("BG_11", "BG_14", "BG_12_13", "BG_15_16")~"Residential", ID_PIEGE%in%c("BG_21","BG_22",  "BG_23", "BG_24")~"City Center")))|>
  dplyr::filter(DATE_COLLECTE>"2023-05-01")|>
  relocate(AREA, ZONE, .after = num_session) |>
  mutate(PRES_ALBO = ifelse(NB_ALBO_TOT>0,"Presence","Absence")) %>%
  mutate(PRES_ALBO = fct_relevel(PRES_ALBO,c("Presence","Absence"))) %>%
  mutate(PRES_ALBO_NUMERIC = ifelse(NB_ALBO_TOT>0,1,0)) %>%
  filter(!is.na(NB_ALBO_TOT))%>%
  filter(!is.na(TMIN_collection),!is.na(RHMIN_24h_prec),!is.na(RHMAX_24h_prec)) 

df_model<-cbind(df_model, model.matrix(~ZONE - 1, data = df_model))

###########################
#########'Presence model preparation
#########'First step: to select for meteorological and pollutants variables, for every type of variable, the time lag for which the r2 was the highest. Same work is realized for micro climatic, land cover (for each buffer) and socio demographic data.
#########'Second step: to evaluate the correlation between these variables.
#########'Third step: to select the variables not correlated with the highest sense ecological. The first selection is crossed with the other selection done with the VIF with the corSelect function of the fuzzySim package to select variables with the lowest VIF. The final selection is a mixed of both methods.
###########################


##### First step: to select variables for presence models
predictors_presence <- c( "RFDode_6_6",
                          "TMNode_1_1", "TMINode_1_1", "TMAXode_0_1", 
                          "RHmf_6_6", "GDDjour_1_1", "GDDsemaine_1_1",
                          "NO2_0_0", "PM2.5_0_1", "PM10_0_1","O3_3_3",
                          "RHMEAN_24h_48h_prec", "RHMIN_collection", "RHMAX_24h_48h_prec", "RFSUM_collection", "TMEAN_collection", "TMAX_collection", "TMIN_24h_48h_prec",
                          "Patmean_collection","Patmin_collection","Patmax_collection","Patm_diff_prev_day_collection", 
                          "FIL_Men_pauv", 
                          "POP_250_sum",
                          "lsm_c_np_LCG_20_12",
                          "lsm_c_te_LCG_20_12",
                          "lsm_c_te_LCG_20_13",
                          "lsm_c_area_mn_LCG_20_13",
                          "lsm_c_pland_LCG_20_13",
                          "lsm_c_te_LCG_20_11",
                          "lsm_c_area_mn_LCG_20_11",
                          "lsm_c_te_LCG_20_10",
                          "lsm_c_pland_LCG_50_10",
                          "lsm_l_shdi_LCG_100_NA")   

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

ggsave("02_Data/processed_data/plot_presence_vs_selected_vars_poll.png",p_pres,width = 9.65, height = 6.42, units = "in") ## to save

##### Second step: identify correlated variables that are greater than 0.7 (Pearson correlation coefficient)
m <- cor(df_model[,predictors_presence], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]])


#### Third select: to select final variables using the VIF 
calculate_vif<- function(data, target_column) {
  predictors <- setdiff(colnames(data), target_column)
  
  vif_values <- sapply(predictors, function(var) {
   
    formula <- as.formula(paste(target_column, "~ ."))
    lm_fit <- lm(formula, data = data[, c(target_column, predictors), drop = FALSE])
    r_squared <- summary(lm(lm_fit$model[[var]] ~ ., data = lm_fit$model[, -which(names(lm_fit$model) == var)]))$r.squared
    vif_value <- 1 / (1 - r_squared)
    return(vif_value)
  })
  
  return(vif_values)
}

reduce_vif <- function(data, target_column, vif_threshold = 10) {
  working_data <- data
  predictors <- setdiff(colnames(data), target_column)
  
  while (TRUE) {
    vif_values <- calculate_vif(working_data, target_column)
    cat("Current VIF values:\n")
    print(vif_values)
  
    if (all(vif_values < vif_threshold)) break
    
    variable_to_remove <- names(which.max(vif_values))
    cat("Removing variable with high VIF:", variable_to_remove, "\n")

    predictors <- setdiff(predictors, variable_to_remove)
    working_data <- working_data[, c(target_column, predictors), drop = FALSE]
  }
  
  return(working_data)
}

var_selected<-reduce_vif(df_model[,c(predictors_presence, "PRES_ALBO_NUMERIC")],target_column="PRES_ALBO_NUMERIC", vif_threshold = 3)
c<-colnames(var_selected)
predictors_presence <- c("RFDode_6_6",   
                         "NO2_0_0",
                         "RHMEAN_24h_48h_prec", 
                         "TMIN_24h_48h_prec", 
                         "Patm_diff_prev_day_collection",
                         "Patmin_collection",
                         "FIL_Men_pauv" ,
                         "lsm_c_np_LCG_20_12",
                         "lsm_c_te_LCG_20_13",
                         "lsm_c_area_mn_LCG_20_13",
                         "lsm_c_te_LCG_20_11") 
                        
#### Final data frame for the multivariate analysis

df_model_presence <- df_model %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "SESSION_DAY","ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "NB_ALBO_F","NB_ALBO_TOT", "PRES_ALBO","PRES_ALBO_NUMERIC", "AREA", predictors_presence)



###########################
#########'Abundance model preparation
#########'First step: to select for meteorological and pollutants variables, for every type of variable, the time lag for which the r2 was the highest. Same work is realized for micro climatic, land cover (for each buffer) and socio demographic data.
#########'Second step: to evaluate the correlation between these variables.
#########'Third step: to select the variables not correlated with the highest sense ecological. The first selection is crossed with the other selection done with the VIF with the corSelect function of the fuzzySim package to select variables with the lowest VIF. The final selection is a mixed of both methods.
###########################

##### First step: select variables for abundance models
predictors_abundance <- c("RFDode_6_6", 
                          "RHmf_6_6", "WINDmf_0_5",
                          "GDDjour_0_2","TMNode_0_2", "TMINode_0_2","TMAXode_0_2","GDDsemaine_0_2",
                          "PM2.5_0_3", "PM10_0_1", "O3_1_1",
                          "RHMEAN_collection","RHMIN_collection", "RHMAX_24h_prec","RFSUM_24hprec", "TMEAN_collection", "TMIN_collection", "TMAX_collection",
                          "Patm_diff_prev_day_24h_prec", "Patmean_24h_48h_prec", "Patmax_24h_48h_prec", #"Patm_daily_range_collection",
                          "lsm_c_np_LCG_50_12",
                          "lsm_c_te_LCG_50_12",
                          "lsm_c_pland_LCG_20_12",
                          "lsm_c_area_mn_LCG_100_12",
                          "lsm_c_te_LCG_100_13",
                          "lsm_c_pland_LCG_250_13",
                          "lsm_c_area_mn_LCG_50_13",
                          "lsm_c_te_LCG_250_11",
                          "lsm_c_pland_LCG_250_11",
                          "lsm_c_area_mn_LCG_250_11",
                          "lsm_c_te_LCG_100_10",
                          "lsm_c_pland_LCG_100_10",
                          "lsm_c_area_mn_LCG_100_10",
                          "FIL_Log_av45",
                          "FIL_Men_pauv","ZONEPark"
                          
)   


df_model_abundance <- df_model %>%
  filter(NB_ALBO_F>0) %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "SESSION_DAY","ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "AREA",  "NB_ALBO_TOT", "NB_ALBO_F", "PRES_ALBO", predictors_abundance)
#####  Plot the bivariate relationship between abundance and each selected predictor 
p_ab <- df_model_abundance %>% 
  dplyr::select(NB_ALBO_F,predictors_abundance) %>%
  pivot_longer(-NB_ALBO_F) %>%
  ggplot(aes(y = NB_ALBO_F, x = value)) +
  geom_point() + 
  geom_smooth() +
  ylim(c(0,80)) +
  facet_wrap(.~name, scales = "free_x") + 
  theme_bw() + 
  ggtitle("Abondance albo ~ variables séléctionnées")

ggsave("02_Data/processed_data/plot_abundance_vs_selected_vars_poll.png",p_ab,width = 9.65, height = 7.42, units = "in") ## to save

##### Second step: identify correlated variables that are greater than 0.7 (Pearson correlation coefficient)
m <- cor(df_model_abundance[,predictors_abundance], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]])

#### Third select: to select final variables using the VIF 
predictors_abundance <- c("RFDode_6_6",  "WINDmf_0_5",
                                  "GDDsemaine_0_2",
                                  "RHMIN_collection", "RHMAX_24h_prec","RFSUM_24hprec",  "TMAX_collection",
                                  "Patm_diff_prev_day_24h_prec", "Patmean_24h_48h_prec", "Patmax_24h_48h_prec",
                                  "lsm_c_np_LCG_50_12",
                                  "lsm_c_te_LCG_50_12",
                                  "lsm_c_pland_LCG_20_12",
                                  "lsm_c_area_mn_LCG_100_12",
                                  "lsm_c_te_LCG_100_13",
                                  "lsm_c_pland_LCG_250_13",
                                  "lsm_c_area_mn_LCG_50_13",
                                  "lsm_c_te_LCG_250_11",
                                  "lsm_c_pland_LCG_250_11",
                                  "lsm_c_area_mn_LCG_250_11",
                                  "lsm_c_te_LCG_100_10",
                                  "lsm_c_pland_LCG_100_10",
                                  "lsm_c_area_mn_LCG_100_10",
                                  "FIL_Log_av45",
                                  "FIL_Men_pauv","ZONEPark"
                                  
)  

var_selected<-reduce_vif(df_model_abundance[,c(predictors_abundance, 'NB_ALBO_F')],target_column = "NB_ALBO_F", vif_threshold = 3)
c<-colnames(var_selected)
predictors_abundance<-c("RFDode_6_6","WINDmf_0_5","GDDsemaine_0_2","RHMAX_24h_prec","RFSUM_24hprec","TMAX_collection",
                                "Patm_diff_prev_day_24h_prec" ,"lsm_c_pland_LCG_250_13" ,"lsm_c_area_mn_LCG_50_13","lsm_c_te_LCG_250_11" ,"lsm_c_area_mn_LCG_250_11","FIL_Men_pauv" )




df_model_abundance <- df_model %>%
  filter(NB_ALBO_F>0) %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","SESSION_DAY","ID_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "AREA",  "NB_ALBO_TOT", "PRES_ALBO", "NB_ALBO_F", predictors_abundance)

###########################
#########'Second stage of analysis: multivariate anaylsis using a leave-one-site-out cross validation and a leave-one-session-out cross validation (but juste to valdiate and evaluate the model)
#########' For presence models
###########################

#### First step: to parameter the model: leave-one-site-out cross validation
cv_col <- "AREA"

indices_cv <- CAST::CreateSpacetimeFolds(df_model_presence, spacevar = cv_col, k = length(unique(df_model_presence[,cv_col])))
#### Second step: It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)

## First : recursive feature eliminations

rfe_control <- rfeControl(
  functions = rfFuncs,         
  method = "cv",               
  index = indices_cv$index,    
  indexOut = indices_cv$indexOut,
  verbose = FALSE
)

rfe_result <- rfe(
  x = df_model_presence[, predictors_presence],
  y = df_model_presence$PRES_ALBO,
  sizes = c(1:10),            
  rfeControl = rfe_control,
  metric = "Accuracy"               
)


selected_predictors <- predictors(rfe_result)
predictors_presence<-c(selected_predictors, "ID_PIEGE", "SESSION_DAY")

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
  dplyr::select(pred,Presence,obs,ZONE,AREA,num_session,idpointdecapture, ID_PIEGE) %>%
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
  dplyr::select(pred,Presence,obs,ZONE,AREA,num_session,idpointdecapture, ID_PIEGE) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)
res_multiv_model_presence_session <- list(model = mod_presence_session, df_cv = df_cv_presence_session, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence,"02_Data/processed_data/01_Adults_Abundance/res_multiv_model_presence_session.rds")



###########################
#########'Second stage of analysis: multivariate anaylsis using a leave-one-site-out cross validation and a leave-one-session-out cross validation (but juste to valdiate and evaluate the model)
#########' For abundance models
###########################
df_model_abundance$NB_ALBO_F <- log(df_model_abundance$NB_ALBO_F)

cv_col <- "AREA"
indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, spacevar = cv_col, k = length(unique(unlist(df_model_abundance[,cv_col]))))

# Contrôle pour RFE
rfe_control <- rfeControl(
  functions = rfFuncs,           
  method = "cv",                
  index = indices_cv$index,      
  indexOut = indices_cv$indexOut,
  verbose = TRUE                
)
rfe_result <- rfe(
  x = df_model_abundance[, predictors_abundance],
  y = df_model_abundance$NB_ALBO_F,
  sizes = c(1:10),             
  rfeControl = rfe_control,
  metric = "MAE"               
)

selected_predictors <- predictors(rfe_result)
predictors_abundance<-c(selected_predictors, "ID_PIEGE", "SESSION_DAY")

tr <- trainControl(
  method = "cv",
  index = indices_cv$index,
  indexOut = indices_cv$indexOut,
  savePredictions = 'final'
)


#### Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NB_ALBO_F, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")

#### Last step: to put predictions on same data frame
df_model_abundance$rowIndex <- seq(1,nrow(df_model_abundance),1)
df_cv_abundance <- mod_abundance$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,ZONE,AREA,num_session,idpointdecapture, ID_PIEGE)

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
mod_abundance_session <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NB_ALBO_F, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")
## Adding prediction with session cross validation 
mod_abundance_session$rowIndex <- seq(1,nrow(df_model_abundance),1)
df_cv_abundance_session <- mod_abundance_session$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,ZONE,AREA,num_session,idpointdecapture, ID_PIEGE)
res_multiv_model_abundance_session <- list(model = mod_abundance_session, df_cv = df_cv_abundance_session, df_mod = df_model_abundance)  ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance_session,"02_Data/processed_data/01_Adults_Abundance/res_multiv_model_abundance_session.rds")



