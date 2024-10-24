########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(iml) ## Version '0.11.3'
library(patchwork) ## Version ‘1.2.0.9000’
library(precrec) ## Version ‘0.14.4’

########################### Open dataset containing the results of presence and abundance models

#### With site cross validation 
multiv_model_presence <- readRDS("02_Data/processed_data/01_Adults_Abundance/res_multiv_model_presence.rds")
multiv_model_abundance <- readRDS("02_Data/processed_data/01_Adults_Abundance/res_multiv_model_abundance.rds")

model_presence <- multiv_model_presence[[1]] #### sum up of presence model
df_cv_presence <- multiv_model_presence[[2]]#### data frame with prediction 
df_mod_presence <- multiv_model_presence[[3]] #### data frame which was used to build the model

model_abundance <- multiv_model_abundance[[1]] #### sum up of abundance model
df_cv_abundance <- multiv_model_abundance[[2]] #### data frame with prediction 
df_mod_abundance <- multiv_model_abundance[[3]] #### data frame which was used to build the model

#### With session cross validation
multiv_model_presence_session <- readRDS("02_Data/processed_data/01_Adults_Abundance/res_multiv_model_presence_session.rds")
multiv_model_abundance_session <- readRDS("02_Data/processed_data/01_Adults_Abundance/res_multiv_model_abundance_session.rds")


df_cv_presence_session <- multiv_model_presence_session[[2]]#### data frame with prediction 
df_cv_abundance_session <- multiv_model_abundance_session[[2]] #### data frame with prediction 


###########################
#########'Presence model 
#########'First step: Evaluation  plots, with site cross validation and with cross site validation and session cross validation
#########'Second step: Validation with AUC using the site cross validation
#########'Third step: realization of Variable Importance Plots (VIP) (from the site cross validation)
#########'Last step: realization of Partial Dependent Plots (PDP) (from the site cross validation)
###########################


#### First step: Model evaluation plots

## With only site cross validation: plot with observation and prediction for the different site, trap and numero session
plot_eval_presence_model <- df_cv_presence %>%
  dplyr::group_by(ID_PIEGE, lieu,num_session) %>%   
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>% ## to sum up, grouping by trap, location and num session 
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  ggplot(aes(x=num_session, y = value, color = name)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(ID_PIEGE~lieu, scales = "free") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  xlab("entomological survey") +
  ylab("∑ Presence probability") + 
  labs(color='Probability of presence of Ae. Albopictus') + 
  theme(legend.position="bottom") + 
  ggtitle('Presence models : observed vs. predicted values')

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/test/test_Article/presence_evaluation.pdf",plot =plot_eval_presence_model, device = "pdf", width = 11, height = 8)

## With site and session cross validation: plot with observation and prediction for the different site, trap and numero session
df_session_pres<-df_cv_presence_session|> ## grouping in a same dataframe both predictions with site cross validtaion and session cross validation 
  select(idpointdecapture, pred)|>
  rename(pred_session=pred)
df_fin_cv<-merge(df_cv_presence, df_session_pres, by="idpointdecapture")

plot_eval_presence_model <- df_fin_cv %>%
  dplyr::group_by(ID_PIEGE, lieu,num_session) %>%   
  dplyr::summarise(pred = mean(pred), obs = mean(obs), pred_session=mean(pred_session)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs', 'pred_session')) %>%
  mutate(name = case_when(name=="pred"~"Predicted with site leave out",name=='pred_session'~"Predicted with session leave out", name=="obs"~"Observed")) %>% ## to sum up, grouping by trap, location and num session 
  ggplot(aes(x=num_session, y = value, color = name)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(ID_PIEGE~lieu, scales = "free") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","blue","#E69F00"),na.translate = F) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  xlab("entomological survey") +
  ylab("∑ Presence probability") + 
  labs(color='Probability of presence of Ae. Albopictus') + 
  theme(legend.position="bottom") + 
  ggtitle('Presence models : observed vs. predicted values')


#### Second step: Model validation plots: ROC 

AUC = MLmetrics::AUC(df_cv_presence$pred, df_cv_presence$obs) ## To calculate the AUC

precrec_obj <- precrec::evalmod(scores = df_cv_presence$pred, labels = df_cv_presence$obs)

plot_validation_presence <- autoplot(precrec_obj,curvetype = c("ROC")) + 
  ggtitle(paste0("Presence model : ROC curve (AUC = ",round(AUC,2),")")) +     
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/presence_validation.pdf",plot =plot_validation_presence, device = "pdf", width = 11, height = 8) ## to save

#### Third step: VIP 

model = multiv_model_presence$model
df = multiv_model_presence$df_mod
df_cv <-  multiv_model_presence$df_cv

## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance 
imp <- as.data.frame(imp)
imp$var <- rownames(imp)

## To arrange by order of importance and to categorize by type of variable
imp <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance) %>% 
  mutate(type = case_when(var %in% c("GDDjour_1_1","WINDmf_0_5","RFDode_5_6") ~ "Meteorological",
                          var %in% c("RFSUM_collection","RHMAX_24h_prec","RHMIN_24h_prec") ~ "Micro-climatic",
                          var=="lsm_c_pland_LCG_20_13" ~ "Landscape - vegetation",
                          var %in% c("lsm_l_shdi_LCG_100_NA","lsm_c_pland_LCG_20_11","lsm_c_pland_LCG_50_10","BATH_max_250") ~ "Landscape - others",
                          var=="NO_3_3" ~ "polluants",
                          var %in% c("POP_250_sum") ~ "Socio-demographics"))


## To plot the importance of the variables
plot_imp_presence <- ggplot(imp, aes(x = importance , y = label, label = label, fill = type)) +
  geom_bar(position = 'dodge', stat="identity", width = 0.6) + 
  theme_bw() + 
  geom_text(size=3,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines")) + #,aes(fontface=2)
  #   geom_label(size=2, aes(fontface=2), label.padding = unit(0.15, "lines"), x = 0.05, alpha = 0.5) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 7),
        plot.subtitle = element_text(size = 7, face="bold")
  ) +
  ylab("") + 
  xlab("") +
  xlim(NA,max(imp$importance, na.rm = T) + max(imp$importance, na.rm = T)*2.5) +
  labs(title = "Presence model : VIP")

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/presence_VIP.pdf",plot =plot_imp_presence, device = "pdf", width = 11, height = 8) ## to save

#### Last step: PDP 

## To create a function which predicts the probability of presence according different variabes
pred_wrapper_classif <- function(object, newdata) { 
  p <- predict(object, newdata = newdata, type ="prob")[,"Presence"]
  c("avg" = mean(p))
}

pdps <- list()

for(i in 1:length(imp$var)){
  
  pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_classif, train = df) ## array that returns predictions of a variable in a model
  pd$yhat[which(pd$yhat<0)] <-0 
  p <- autoplot(pd, smooth = T)  
  dat1 <- ggplot_build(p)$data[[1]]
  dat2 <- ggplot_build(p)$data[[2]]
  
  if(imp$var[i]!="RFSUM_collection"){ ## for every variable which is not The resence of rainfall
    pdps[[i]] <- ggplot() + 
      geom_line(data = dat1, aes(x = x, y = exp(y)), size = 0.3, colour = "black", alpha = 0.4) +  ## smooth the observed data
      geom_line(data = dat2, aes(x = x, y = y), size = 0.5, colour = "#009E73") +  ## smooth the prediction data
      geom_rug(data = df, aes_string(x = imp$var[i]), sides="b", length = unit(0.05, "npc")) + 
      ylim(c(0,1)) + 
      theme_bw() + 
      xlab(imp$var[i]) + 
      ylab("")
  } else {
    dat1$x <- c("Absence","Presence")
    df[,imp$var[i]][which(df[,imp$var[i]]==1)] <- "Presence"
    df[,imp$var[i]][which(df[,imp$var[i]]==0)] <- "Absence"
    
    pdps[[i]] <- ggplot() + 
      geom_bar(data = dat1, aes(x = x, y = y), size = 0.5, fill = "#009E73", stat = "identity") +  ## indicate the different value of the variable present on the data frame
      geom_rug(data = df, aes_string(x = imp$var[i]), sides="b", length = unit(0.05, "npc")) + 
      ylim(c(0,1)) + 
      theme_bw() + 
      xlab(imp$var[i]) + 
      ylab("")
    
    df[,imp$var[i]][which(df[,imp$var[i]]=="Presence")] <- "1"
    df[,imp$var[i]][which(df[,imp$var[i]]=="Absence")] <- "0"
    df[,imp$var[i]] <- as.numeric(df[,imp$var[i]])
  }
  
}

plot_pdps_presence <- patchwork::wrap_plots(pdps) + plot_annotation(title = "Presence model : PDP") ## put all the plots together

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/test/test_Article/presence_PDP.pdf",plot =plot_pdps_presence, device = "pdf", width = 11, height = 8) ## To save



###########################
#########'Abundance model 
#########'First step: Evaluation  plots, with site cross validation and with cross site validation and session cross validation
#########'Second step: Validation with RMSE using the site cross validation
#########'Third step: realization of Variable Importance Plots (VIP) (from the site cross validation)
#########'Last step: realization of Partial Dependent Plots (PDP) (from the site cross validation)
###########################

#### First step: Model evaluation plots

## With only site cross validation: plot with observation and prediction for the different site, trap and numero session

plot_eval_abundance_model <- df_cv_abundance %>%
  mutate(obs=exp(obs),pred=exp(pred)) %>%
  dplyr::group_by(lieu,num_session) %>%    ## to sum up, grouping by trap, location and num session 
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  ggplot(aes(x=num_session, y = value, color = name)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(.~lieu, scales = "free_y") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  xlab("entomological survey") +
  ylab("mean abundance") + 
  labs(color='Number of Ae. Albopictus') + 
  theme(legend.position="bottom") + 
  ggtitle('Abundance models : observed vs. predicted values by site and entomological survey')

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/abundance_evaluation.pdf",plot =plot_eval_abundance_model, device = "pdf", width = 11, height = 8) ## to save

## With site and session cross validation: plot with observation and prediction for the different site, trap and numero session

df_session_abund<-df_cv_abundance_session|> ## grouping in a same dataframe both predictions with site cross validation and session cross validation 
  select(idpointdecapture, pred)|>
  rename(pred_session=pred)
df_fin_cv<-merge(df_cv_abundance, df_session_abund, by="idpointdecapture")


plot_eval_abundance_model <- df_fin_cv %>%
  mutate(obs=exp(obs),pred=exp(pred), pred_session=exp(pred_session)) %>%
  dplyr::group_by(ID_PIEGE, lieu, num_session) %>%    ## to sum up, grouping by trap, location and num session 
  dplyr::summarise(pred = mean(pred), obs = mean(obs), pred_session=mean(pred_session)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs', 'pred_session')) %>%
  mutate(name = case_when(name=="pred"~"Predicted with site leave out",name=='pred_session'~"Predicted with session leave out", name=="obs"~"Observed")) %>%
  ggplot(aes(x=num_session, y = value, color = name)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(ID_PIEGE~lieu, scales = "free_y") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","blue","#E69F00"),na.translate = F) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9)) +
  xlab("entomological survey") +
  ylab("mean abundance") + 
  labs(color='Number of Ae. Albopictus') + 
  theme(legend.position="bottom") + 
  ggtitle('Abundance models : observed vs. predicted values by site and entomological survey')


#### Second step: Model validation plots: visually with the RMSE

df_cv_abundance2 <- df_cv_abundance %>% ## separation of prediction in different groups according to the number predicted to better wizualisation
  mutate(obs = exp(obs)) %>%
  mutate(pred = exp(pred)) %>%
  mutate(rep = abs(obs - pred)/obs) %>%
  mutate(residuals = obs - pred) %>% 
  mutate(groups = case_when(
    obs>=1 & obs<=3 ~ "1-3",
    obs>3 & obs<=10 ~ "4-10",
    obs>10 & obs<=20 ~ "11-20",
    obs>20 ~ ">20"
  )) %>%
  mutate(groups = fct_relevel(groups, c("1-3","4-10","11-20",">20")))

df_metrics_perf <- df_cv_abundance2 %>% ## to evaluate different type of metrics
  group_by(groups) %>%
  summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
            mse =  round(MLmetrics::MSE(y_true = obs ,y_pred = pred),2),
            rmse =  round(MLmetrics::RMSE(y_true = obs ,y_pred = pred),2),
            mape =  round(MLmetrics::MAPE(y_true = obs ,y_pred = pred),2),
            r2 =  round(MLmetrics::R2_Score(y_pred = pred,y_true = obs),2),
            n=n()) %>%
  as_tibble()

## To represent visually the RMSE according to the different category of prediction 
plot_validation_abundance <- ggplot() + 
  geom_violin(data = df_cv_abundance2, aes(x=groups , y=residuals)) + 
  stat_summary(data = df_cv_abundance2, aes(x=groups , y=residuals), fun=median, geom="point", size=2, color="black") +
  theme_bw() + 
  xlab("Observed counts") + 
  ylab("Residuals (obs - pred)") + 
  geom_label(data = df_metrics_perf,
             size = 2.5,
             mapping = aes(x = groups, y = max(df_cv_abundance2$residuals,na.rm = T), label = paste0('RMSE = ',rmse,'\nn = ',n),
                           vjust = 1)) +
  ggtitle("Abundance model: RMSE by count class") + 
  geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/abundance_validation.pdf",plot =plot_validation_abundance, device = "pdf", width = 11, height = 8) ## predictions

#### Third step: VIP  

model = multiv_model_abundance$model
df = multiv_model_abundance$df_mod
df_cv <-  multiv_model_abundance$df_cv

## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance
imp <- as.data.frame(imp)
imp$var <- rownames(imp)

## To arrange by order of importance and to categorize by type of variable
imp <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance) %>% 
  mutate(type = case_when(var %in% c("RFDode_6_6","GDDjour_1_1","WINDmf_0_1") ~ "Meteorological",
                          var %in% c("RHMEAN_collection","TMAX_collection") ~ "Micro-climatic",
                          var %in% c("lsm_c_pland_LCG_20_12","lsm_c_pland_LCG_50_13") ~ "Landscape - vegetation",
                          var %in% c("lsm_c_pland_LCG_100_10") ~ "Landscape - others",
                          var%in%c("NOX_0_0")~"Polluants"))


## To plot the importance of variables
plot_imp_abundance <- ggplot(imp, aes(x = importance , y = label, label = label, fill = type)) +
  geom_bar(position = 'dodge', stat="identity", width = 0.6) + 
  theme_bw() + 
  geom_text(size=3,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines")) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 7),
        plot.subtitle = element_text(size = 7, face="bold")
  ) +
  ylab("") + 
  xlab("") +
  xlim(NA,max(imp$importance, na.rm = T) + max(imp$importance, na.rm = T)*2.5) +
  labs(title = "Abundance model : VIP")

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/abundance_VIP.pdf",plot =plot_imp_abundance, device = "pdf", width = 11, height = 8) ## To save

#### Last step: PDP  

## To create a function which predicts the abundance according different variables

pred_wrapper_reg <- function(object, newdata) {
  p <- predict(object, newdata = newdata)
  c("avg" = mean(p))
  #c("avg" = mean(p), "avg-1sd" = mean(p) - sd(p), "avg+1sd" = mean(p) + sd(p))
}

pdps <- list()
for(i in 1:length(imp$var)){
  pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_reg, train = df)
  pd$yhat[which(pd$yhat<0)] <-0 
  p <- autoplot(pd, smooth = T)  
  dat1 <- ggplot_build(p)$data[[1]]
  dat2 <- ggplot_build(p)$data[[2]]
  pdps[[i]] <- ggplot() + 
    geom_line(data = dat1, aes(x = x, y = exp(y)), size = 0.3, colour = "black", alpha = 0.4) +   ## smooth the observed data
    geom_line(data = dat2, aes(x = x, y = exp(y)), size = 0.5, colour = "#009E73") +  # smooth the prediction data
    geom_rug(data = df, aes_string(x = imp$var[i]), sides="b", length = unit(0.05, "npc")) + 
    theme_bw() + 
    xlab(imp$var[i]) + 
    ylab("") + 
    ylim(c(4,11))
  
}

plot_pdps_abundance <- patchwork::wrap_plots(pdps) + plot_annotation(title = "Abundance model : PDP") ## To put all variables together

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/abundance_PDP.pdf",plot =plot_pdps_abundance, device = "pdf", width = 11, height = 8) ## To save



