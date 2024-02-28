library(tidyverse)
library(caret)
library(CAST)
library(ranger)

# open dataset containing the dependant and independent variables
df_model <- read.csv("df_model.csv")

df_model <- df_model %>% 
  mutate(PRES_ALBO = ifelse(NB_ALBO_TOT>0,"Presence","Absence")) %>%
  mutate(PRES_ALBO = fct_relevel(PRES_ALBO,c("Presence","Absence"))) %>%
  mutate(PRES_ALBO_NUMERIC = ifelse(NB_ALBO_TOT>0,1,0)) %>%
  filter(!is.na(NB_ALBO_TOT)) %>%
   filter(!is.na(TMIN_collection),!is.na(RHMIN_24h_prec),!is.na(RHMAX_24h_prec))  %>%
   mutate(RFSUM_collection=ifelse(RFSUM_collection>0.5,1,0))

##### select variables for presence models
predictors_presence <- c("RFDode_5_6", "TMNode_0_0", "TMINode_2_2", "TMAXode_0_1", "TAMPode_5_5", "RHmf_2_2", "WINDmf_0_5",
                         "RHMEAN_collection", "RHMIN_24h_prec", "RHMAX_24h_prec", "RFSUM_collection", "TMEAN_collection", "TMAX_48h_prec", "TMIN_collection",
                         "lsm_c_pland_LCG_20_13",  # % de landscape de végétation de moins de 3 m à 20 m
                         "lsm_c_pland_LCG_20_10",  # % de landscape de batiments : à 20 m
                         "lsm_c_pland_LCG_20_11",  # % de landscape de routes : à 20 m
                         "lsm_l_shdi_LCG_100_NA",# Indice de diversité du paysage (Shannon diversity index) : 100 m
                         "FIL_Men_pauv", # nomùbre de ménages pauvres
                         "POP_250_sum", # pop somméeà 250 m
                         "DVG", # ditance végétatipon
                         #"HVG_20_min", # ghauteur minimale végétation à 20 m"
                         "BATH_max_250", # hauteur max bati 250 m
                         "BATS_sum_100")   # surface bati sommée à 200 m



# Plot the bivariate relationship between presence and each selected predictor 
p_pres <- df_model %>% 
  dplyr::select(PRES_ALBO_NUMERIC,predictors_presence) %>%
  pivot_longer(-PRES_ALBO_NUMERIC) %>%
  ggplot(aes(y = PRES_ALBO_NUMERIC, x = value)) +
  geom_point() + 
  ylim(c(0,1)) +
  geom_smooth() +
  facet_wrap(.~name, scales = "free") + 
  theme_bw() + 
  ggtitle("Presence albo ~ variables séléctionnées")
  
#ggsave("plot_presence_vs_selected_vars.png",p_pres,width = 9.65, height = 6.42, units = "in")

# identifier les variables qui sont corrélées au dessus de 0.7 (coeff de correlation de pearson) : 
m <- cor(df_model[,predictors_presence], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]])



# on ne conserve que des variables non corrélées entre elles : 
predictors_presence <- c("RFDode_5_6",  "TMINode_2_2", "RHmf_2_2", "WINDmf_0_5",
                                    "RHMIN_24h_prec", "RHMAX_24h_prec", 
                                    "RFSUM_collection",
                                   "lsm_c_pland_LCG_20_13",  # % de landscape de végétation de moins de 3 m à 20 m
                                   "lsm_c_pland_LCG_20_10",  # % de landscape de batiments : à 20 m
                                   "lsm_c_pland_LCG_20_11",  # % de landscape de routes : à 20 m
                                   "lsm_l_shdi_LCG_100_NA",# Indice de diversité du paysage (Shannon diversity index) : 100 m
                                   "FIL_Men_pauv", # nomùbre de ménages pauvres
                                   #"HVG_20_min", # ghauteur minimale végétation à 20 m"
                                   "BATH_max_250") # hauteur max bati 250 m


df_model_presence <- df_model %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "lieu",  "NB_ALBO_TOT", "PRES_ALBO", predictors_presence)


###### select variables for abundance models
predictors_abundance <- c("RFDode_6_6", "TMNode_1_1", "TMINode_2_2","TMAXode_1_1","TAMPode_3_5", "RHmf_4_6", "WINDmf_0_2",
                          "lsm_c_pland_LCG_20_12",  # % de landscape de végétation de plus de 3 m à 20 m
                          "lsm_c_pland_LCG_50_13",  # % de landscape de végétation de moins de 3 m à 50 m
                          "lsm_c_pland_LCG_100_10",  # % de landscape de batiments : à 100 m
                          "lsm_c_pland_LCG_100_11",  # % de landscape de routes : à 100 m
                          "lsm_l_shdi_LCG_20_NA",   # Indice de diversité du paysage (Shannon diversity index) : 20 m
                          "RHMEAN_collection", "RFSUM_48hprec", "TMEAN_collection", "TMIN_collection", "TMAX_collection",
                          "BATS_mn_20", #  surface bati moyenne à 20 m
                          "IMP_0", # impermeabilisation à 0m
                          "BATH_max_50", # Hauteur du bati max à 50 m
                          "HVG_20_min", # hauteur de la végétation minimale à 20 m
                          "DVG", # distance à la végétation
                          "DBT", # distance au bati
                          "FIL_Log_av45")   #  le nombre de logements construits avant 1945



df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT>0) %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "lieu",  "NB_ALBO_TOT", "PRES_ALBO", predictors_abundance)

# Plot the bivariate relationship between abundance and each selected predictor 
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

#ggsave("plot_abundance_vs_selected_vars.png",p_ab,width = 9.65, height = 7.42, units = "in")

# identifier les variables qui sont corrélées au dessus de 0.7 (coeff de correlation de pearson) : 
m <- cor(df_model_abundance[,predictors_abundance], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]])


predictors_abundance <- c("RFDode_6_6", "TMINode_2_2","TAMPode_3_5", "WINDmf_0_2",
                                     "lsm_c_pland_LCG_20_12",  # % de landscape de végétation de plus de 3 m à 20 m
                                     "lsm_c_pland_LCG_50_13",  # % de landscape de végétation de moins de 3 m à 50 m
                                     "lsm_c_pland_LCG_100_10",  # % de landscape de batiments : à 100m
                                      "TMIN_collection",
                                     #"HVG_20_min", # hauteur de la végétation mibniamlke à 20 m
                                     "FIL_Log_av45")   #  le nombre de logements construits avant 1945



df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT>0) %>%
  dplyr::select("idpointdecapture", "ID_PIEGE", "num_session","ID_COLLECTE", "ZONE",  "TYPE_PIEGE", "LATITUDE", "LONGITUDE", "lieu",  "NB_ALBO_TOT", "PRES_ALBO",predictors_abundance)

## Modélisation avec RF

# leave-one-site-out cross validation
cv_col <- "lieu"


# Presence
indices_cv <- CAST::CreateSpacetimeFolds(df_model_presence, spacevar = cv_col, k = length(unique(unlist(df_model_presence[,cv_col])))) 

tr = trainControl(method="cv",
                  index = indices_cv$index, 
                  indexOut = indices_cv$indexOut,
                  summaryFunction = twoClassSummary,#comboSummary,
                  classProbs = TRUE,
                  savePredictions = 'final',
                  verboseIter = FALSE
)

mod_presence <- caret::train(x = df_model_presence[,predictors_presence], y = df_model_presence$PRES_ALBO, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")
#mod_presence <- ffs(predictors = df_model_presence[,predictors_presence], response = df_model_presence$PRES_ALBO, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")

df_model_presence$rowIndex <- seq(1,nrow(df_model_presence),1)

df_cv_presence <- mod_presence$pred %>%
  left_join(df_model_presence) %>%
  dplyr::select(pred,Presence,obs,ZONE,lieu,num_session,idpointdecapture, ID_PIEGE) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)


## Abondance
df_model_abundance$NB_ALBO_TOT <- log(df_model_abundance$NB_ALBO_TOT)

indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, spacevar = cv_col,k = length(unique(unlist(df_model_abundance[,cv_col])))) 

tr = trainControl(method="cv",
                  index = indices_cv$index, 
                  indexOut = indices_cv$indexOut,
                  savePredictions = 'final')

mod_abundance <- caret::train(x = df_model_abundance[,predictors_abundance], y = df_model_abundance$NB_ALBO_TOT, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")
#mod_abundance <- ffs(predictors = df_model_abundance[,predictors_abundance], response = df_model_abundance$NB_ALBO_TOT, method = "ranger", tuneLength = 10, trControl = tr, metric = "MAE", maximize = FALSE,  preProcess = c("center","scale"),importance = "permutation", local.importance = "TRUE")

df_model_abundance$rowIndex <- seq(1,nrow(df_model_abundance),1)

df_cv_abundance <- mod_abundance$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,ZONE,lieu,num_session,idpointdecapture, ID_PIEGE)


res_multiv_model_presence <- list(model = mod_presence, df_cv = df_cv_presence, df_mod = df_model_presence)
saveRDS(res_multiv_model_presence,"res_multiv_model_presence.rds")

res_multiv_model_abundance <- list(model = mod_abundance, df_cv = df_cv_abundance, df_mod = df_model_abundance)
saveRDS(res_multiv_model_abundance,"res_multiv_model_abundance.rds")
