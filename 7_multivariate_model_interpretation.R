library(tidyverse)
library(iml)


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
  dplyr::group_by(lieu,num_session) %>%   
  dplyr::summarise(pred = mean(pred), obs = mean(obs)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  ggplot(aes(x=num_session, y = value, color = name)) +
  geom_point() + 
  geom_line() + 
  facet_wrap(.~lieu, scales = "free") + 
  theme_bw() + 
  scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F) + 
  xlab("entomological survey") +
  ylab("∑ Presence probability") + 
  labs(color='Number of Ae. Albopictus') + 
  theme(legend.position="bottom")



plot_eval_abundance_model <- df_cv_abundance %>%
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
  xlab("entomological survey") +
  ylab("abundance") + 
  labs(color='Number of Ae. Albopictus') + 
  theme(legend.position="bottom")



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
  arrange(-importance)


plot_imp_presence <- ggplot(imp, aes(x = importance , y = label, label = label)) +
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
  labs(subtitle = "Variable importance")

X = df_mod_presence %>% dplyr::select(model_presence$finalModel$xNames)
model = Predictor$new(model_presence, data = X, y = df_mod_presence$PRES_ALBO)
effect = FeatureEffects$new(model)
effect$plot(features = model_presence$finalModel$xNames)

imp <- FeatureImp$new(model, loss = "ce")



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
  arrange(-importance)


plot_imp_abundance <- ggplot(imp, aes(x = importance , y = label, label = label)) +
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
  labs(subtitle = "Variable importance")

X = df_mod_abundance %>% dplyr::select(model_abundance$finalModel$xNames)
model = Predictor$new(model_abundance, data = X, y = df_mod_abundance$PRES_ALBO)
effect = FeatureEffects$new(model)
effect$plot(features = model_abundance$finalModel$xNames)




a <- as.data.frame(multiv_model_abundance$model$finalModel$variable.importance.local)
localvarimp <- cbind(df_mod_abundance %>% dplyr::select(num_session, lieu),a) %>%
  mutate(num_session= as.factor(num_session)) %>%
  group_by(lieu) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
  as_tibble()

n = localvarimp$lieu
  
localvarimp <- localvarimp %>% dplyr::select(-lieu)

localvarimp_mat <- as.matrix(sapply(localvarimp, as.numeric))

rownames(localvarimp_mat) <- n

pheatmap::pheatmap(t(localvarimp_mat), cluster_cols = FALSE, cluster_rows = FALSE)
