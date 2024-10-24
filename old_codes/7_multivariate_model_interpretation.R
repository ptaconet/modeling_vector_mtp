library(tidyverse)
library(iml)
library(patchwork)
library(precrec)

multiv_model_presence <- readRDS("res_multiv_model_presence.rds")
multiv_model_abundance <- readRDS("res_multiv_model_abundance.rds")

model_presence <- multiv_model_presence[[1]]
df_cv_presence <- multiv_model_presence[[2]]
df_mod_presence <- multiv_model_presence[[3]]

model_abundance <- multiv_model_abundance[[1]]
df_cv_abundance <- multiv_model_abundance[[2]]
df_mod_abundance <- multiv_model_abundance[[3]]

## Model evaluation plots

plot_eval_presence_model <- df_cv_presence %>%
  dplyr::group_by(ID_PIEGE,lieu,num_session) %>%   
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>%
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
  labs(color='Number of Ae. Albopictus') + 
  theme(legend.position="bottom") + 
  ggtitle('Presence models : observed vs. predicted values')


AUC = MLmetrics::AUC(df_cv_presence$pred, df_cv_presence$obs)

precrec_obj <- precrec::evalmod(scores = df_cv_presence$pred, labels = df_cv_presence$obs)
plot_validation_presence <- autoplot(precrec_obj,curvetype = c("ROC")) + 
  ggtitle(paste0("Presence model : ROC curve (AUC = ",round(AUC,2),")")) +     
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))


plot_validation_presence+plot_eval_presence_model


## abundance

plot_eval_abundance_model <- df_cv_abundance %>%
  mutate(obs=exp(obs),pred=exp(pred)) %>%
  dplyr::group_by(lieu,num_session) %>%   
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


df_cv_abundance2 <- df_cv_abundance %>%
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


df_metrics_perf <- df_cv_abundance2 %>%
  group_by(groups) %>%
  summarise(mae = round(MLmetrics::MAE(y_true = obs ,y_pred = pred),2),
            mse =  round(MLmetrics::MSE(y_true = obs ,y_pred = pred),2),
            rmse =  round(MLmetrics::RMSE(y_true = obs ,y_pred = pred),2),
            mape =  round(MLmetrics::MAPE(y_true = obs ,y_pred = pred),2),
            r2 =  round(MLmetrics::R2_Score(y_true = obs ,y_pred = pred),2),
            n=n()) %>%
  as_tibble()


plot_validation_abundance <- ggplot() + 
  geom_violin(data = df_cv_abundance2, aes(x=groups , y=residuals)) + 
  #geom_jitter(data = df, aes(x=groups , y=residuals), position = position_jitter(width = .15), size = 0.3) + 
  stat_summary(data = df_cv_abundance2, aes(x=groups , y=residuals), fun=median, geom="point", size=2, color="black") +
  theme_bw() + 
  xlab("Observed counts") + 
  ylab("Residuals (obs - pred)") + 
  geom_label(data = df_metrics_perf,
             size = 2.5,
             mapping = aes(x = groups, y = max(df_cv_abundance2$residuals,na.rm = T), label = paste0('R2MSE = ',rmse,'\nn = ',n),
                           vjust = 1)) +
  ggtitle("Abundance model: R squared by count class") + 
  geom_hline(yintercept=0, linetype="dashed") + 
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))


plot_eval_abundance_model2 <- df_cv_abundance %>%
     mutate(obs=exp(obs),pred=exp(pred)) %>%
     mutate(num_session = as.character(num_session)) %>%
     ggplot(aes(x=obs,y=pred, colour = num_session)) +
     geom_point() +
     #scale_x_sqrt(limits = c(0,100)) +
     #scale_y_sqrt(limits = c(0,100)) +
     theme_bw() +
     geom_smooth(method = "lm", se = F) +
     facet_wrap(~lieu , scales = 'free') + 
  ggtitle("Predictive performance of the abundance model - by site and entomological survey")


plot_eval_abundance_model2 <- df_cv_abundance %>%
  mutate(obs=exp(obs),pred=exp(pred)) %>%
  mutate(num_session = as.character(num_session)) %>%
  ggplot(aes(x=obs,y=pred, colour = num_session)) +
  geom_point() +
  #scale_x_sqrt(limits = c(0,100)) +
  #scale_y_sqrt(limits = c(0,100)) +
  theme_bw() +
  geom_smooth(method = "lm", se = F) +
  ggtitle("Predictive performance of the abundance model - by entomological survey")


plot_validation_abundance+plot_eval_abundance_model

# Variable importance plot and pdp : presence
model = multiv_model_presence$model
df = multiv_model_presence$df_mod
df_cv <-  multiv_model_presence$df_cv

imp <- model$finalModel$variable.importance
imp <- as.data.frame(imp)
imp$var <- rownames(imp)

imp <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance) %>% 
  mutate(type = case_when(var %in% c("TMINode_2_2","WINDmf_0_5","RFDode_5_6","RHmf_2_2") ~ "Meteorological",
                          var %in% c("RFSUM_collection","RHMAX_24h_prec","RHMIN_24h_prec") ~ "Micro-climatic",
                          var=="lsm_c_pland_LCG_20_13" ~ "Landscape - vegetation",
                          var %in% c("lsm_l_shdi_LCG_100_NA","lsm_c_pland_LCG_20_11","lsm_c_pland_LCG_20_10","BATH_max_250") ~ "Landscape - others",
                          var %in% c("FIL_Men_pauv") ~ "Socio-economical"))


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

# pdp
pred_wrapper_classif <- function(object, newdata) {
  p <- predict(object, newdata = newdata, type ="prob")[,"Presence"]
  c("avg" = mean(p))
}

pdps <- list()

for(i in 1:length(imp$var)){
  
pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_classif, train = df)
pd$yhat[which(pd$yhat<0)] <-0 
p <- autoplot(pd, smooth = T)  
dat1 <- ggplot_build(p)$data[[1]]
dat2 <- ggplot_build(p)$data[[2]]

if(imp$var[i]!="RFSUM_collection"){
pdps[[i]] <- ggplot() + 
  geom_line(data = dat1, aes(x = x, y = exp(y)), size = 0.3, colour = "black", alpha = 0.4) + 
  geom_line(data = dat2, aes(x = x, y = y), size = 0.5, colour = "#009E73") + 
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
    geom_bar(data = dat1, aes(x = x, y = y), size = 0.5, fill = "#009E73", stat = "identity") + 
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

plot_pdps_presence <- patchwork::wrap_plots(pdps) + plot_annotation(title = "Presence model : PDP")

plot_imp_presence+plot_pdps_presence

# Variable importance plot and pdp : abundance
model = multiv_model_abundance$model
df = multiv_model_abundance$df_mod
df_cv <-  multiv_model_abundance$df_cv

imp <- model$finalModel$variable.importance
imp <- as.data.frame(imp)
imp$var <- rownames(imp)

imp <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance) %>% 
  mutate(type = case_when(var %in% c("RFDode_6_6","TMINode_2_2","TAMPode_3_5","WINDmf_0_2") ~ "Meteorological",
                          var %in% c("TMIN_collection") ~ "Micro-climatic",
                          var %in% c("lsm_c_pland_LCG_20_12","lsm_c_pland_LCG_50_13") ~ "Landscape - vegetation",
                          var %in% c("lsm_c_pland_LCG_100_10") ~ "Landscape - others",
                          var %in% c("FIL_Log_av45") ~ "Socio-economical"))


plot_imp_abundance <- ggplot(imp, aes(x = importance , y = label, label = label, fill = type)) +
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
  #labs(subtitle = paste0(gsub("physiological_resistance_","",indicator),"\n",species,"\ntimeframe = ",period_interv,"\nAUC = ",ifelse(indicator %in% c("presence","exophagy","early_biting","late_biting","early_late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1"),round(max_metric,2),""),"AUC2 = ",ifelse(indicator %in% c("presence","exophagy","early_biting","late_biting","early_late_biting","physiological_resistance_kdrw","physiological_resistance_kdre","physiological_resistance_ace1"),round(auc,2)),"\nVariable importance"))
  labs(title = "Abundance model : VIP")


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
    geom_line(data = dat1, aes(x = x, y = exp(y)), size = 0.3, colour = "black", alpha = 0.4) + 
    geom_line(data = dat2, aes(x = x, y = exp(y)), size = 0.5, colour = "#009E73") + 
    geom_rug(data = df, aes_string(x = imp$var[i]), sides="b", length = unit(0.05, "npc")) + 
    theme_bw() + 
    xlab(imp$var[i]) + 
    ylab("") + 
    ylim(c(4,11))
  
}

plot_pdps_abundance <- patchwork::wrap_plots(pdps) + plot_annotation(title = "Abundance model : PDP")


plot_imp_abundance+plot_pdps_abundance


# Local importance

# 
# a <- as.data.frame(multiv_model_presence$model$finalModel$variable.importance.local)
# localvarimp <- cbind(df_mod_presence %>% dplyr::select(num_session, lieu),a) %>%
#   mutate(num_session= as.factor(num_session)) %>%
#   group_by(lieu) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#   as_tibble()
# 
# n = localvarimp$lieu
#   
# localvarimp <- localvarimp %>% dplyr::select(-lieu)
# 
# localvarimp_mat <- as.matrix(sapply(localvarimp, as.numeric))
# 
# rownames(localvarimp_mat) <- n
# 
# pheatmap::pheatmap(t(localvarimp_mat), cluster_cols = FALSE, cluster_rows = FALSE)
