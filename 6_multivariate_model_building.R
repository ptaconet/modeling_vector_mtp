library(tidyverse)
library(caret)

# open dataset containing the dependant and independent variables
df_model <- read.csv("df_model.csv")

df_model <- df_model %>% 
  mutate(PRES_ALBO = ifelse(NB_ALBO_TOT>0,"presence","absence")) %>%
  mutate(PRES_ALBO = fct_relevel(PRES_ALBO,c("absence","presence"))) %>%
  mutate(PRES_ALBO_NUMERIC = ifelse(NB_ALBO_TOT>0,1,0)) %>%
  filter(!is.na(NB_ALBO_TOT))

##### select variables for presence models
predictors_presence <- c("TMAXode_3_4","TMINode_0_1","TMNode_0_0","RFDode_3_3","TMEAN_48h_prec","TMIN_collection","TMAX_48h_prec","RHMEAN_1s_prec",
                         "lsm_c_pland_LCG_20_13",  # % de landscape de végétation de moins de 3 m à 20 m
                         "lsm_c_pland_LCV_20_10",  # % de landscape de batiments : à 20 m
                         "lsm_c_pland_LCV_20_11",  # % de landscape de routes : à 20 m
                         "lsm_l_shdi_LCG_100_NA",  # Indice de diversité du paysage (Shannon diversity index) : 100 m
                         "TAMPode_4_5",
                         "WINDmf_2_3",
                         "RHmf_3_5",
                         "FIL_Men_pauv",          # le nombre de ménages pauvres
                         "POP_50_sum",          # somme de la population à 5m
                         "HVG_0",          # la hauteur de la végétation minimale à 2.5 m environ 
                         "DVG"           # distance à la végétation du piège
                         )  


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
  
ggsave("plot_presence_vs_selected_vars.png",p_pres,width = 9.65, height = 6.42, units = "in")

# identifier les variables qui sont corrélées au dessus de 0.7 (coeff de correlation de pearson) : 
m <- cor(df_model[,predictors_presence], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]])



# on ne conserve que des variables non corrélées entre elles : 
predictors_presence <- c("TMAXode_3_4","TMINode_0_1","RFDode_3_3",
                         "lsm_c_pland_LCG_20_13",  # % de landscape de végétation de moins de 3 m à 20 m
                         "lsm_c_pland_LCV_20_10",  # % de landscape de batiments : à 20 m
                         "lsm_c_pland_LCV_20_11",  # % de landscape de routes : à 20 m
                         "lsm_l_shdi_LCG_100_NA",  # Indice de diversité du paysage (Shannon diversity index) : 100 m
                         "TAMPsy_4_5",
                         "WINDmf_2_3",
                         "RHmf_3_5",
                         "FIL_Men_pauv",          # le nombre de ménages pauvres
                         "POP_50_sum",          # somme de la population à 5m
                         "HVG_0",          # la hauteur de la végétation minimale à 2.5 m environ 
                         "DVG"           # distance à la végétation du piège
)  



###### select variables for abundance models
predictors_abundance <- c("TMAXsy_1_1","TMINsy_0_4","TMNsy_3_4","RFDsy_3_3","TMEAN_collection","TMIN_collection","TMAX_collection",
                         "lsm_c_pland_LCG_20_12",  # % de landscape de végétation de plus de 3 m à 20 m
                         "lsm_c_pland_LCG_50_13",  # % de landscape de végétation de moins de 3 m à 50 m
                         "lsm_c_pland_LCV_100_10",  # % de landscape de batiments : à 100 m
                         "lsm_c_pland_LCV_100_11",  # % de landscape de routes : à 100 m
                         "lsm_l_shdi_LCG_20_NA",   # Indice de diversité du paysage (Shannon diversity index) : 20 m
                         "TAMPsy_3_4",
                         "WINDmf_0_1",
                         "RHmf_0_3",
                         "FIL_Log_av45",                  #  le nombre de logements construits avant 1945
                         "BATS_mn_20",                  #  surface bati sommée à 5m
                         "IMP_20_mean",                 # les sols imperméabilisés en miyenne à 5m,
                         "HVG_20_mean",                 #  la hauteur de la végétation minimale à environ 2 m
                         "DVG",                  #  distance à la végétation au niveau du piège.
                         "DBT"                  #  distance au bati au niveau du piège.
                         
)  


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
  facet_wrap(.~name, scales = "free") + 
  theme_bw() + 
  ggtitle("Abondance albo ~ variables séléctionnées")

ggsave("plot_abundance_vs_selected_vars.png",p_ab,width = 9.65, height = 7.42, units = "in")

# identifier les variables qui sont corrélées au dessus de 0.7 (coeff de correlation de pearson) : 
m <- cor(df_model_abundance[,predictors_abundance], method = "pearson", use = "pairwise.complete.obs")
index <- which(abs(m) > .7 & abs(m) < 1,arr.ind = T) 
df_cor <- subset(as.data.frame(index) , row <= col)
p <- cbind.data.frame(stock1 = rownames(m)[df_cor[,1]], stock2 = colnames(m)[df_cor[,2]])







