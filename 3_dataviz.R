library(tidyverse)
library(lubridate)

df_model <- read.csv("df_model.csv") %>% 
  mutate(DATE_COLLECTE = as.Date(DATE_COLLECTE))%>%
  mutate(PRES_ALBO = ifelse(NB_ALBO_TOT>0,1,0)) %>%
  mutate(week = floor_date(DATE_COLLECTE, "weeks"))

## boxplots nb albo capturés en fonction du temps
# tous sites confondus
ggplot(df_model, aes(x = df_model$week, y = df_model$NB_ALBO_TOT, group = df_model$week)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(3), cex=0.2)

# en fonction des sites
ggplot(df_model, aes(x = df_model$week, y = df_model$NB_ALBO_TOT, group = df_model$week)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(3), cex=0.2) + 
  facet_wrap(.~lieu)
  
# en fonction des pièges
ggplot(df_model, aes(x = df_model$week, y = df_model$NB_ALBO_TOT, group = df_model$week)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(3), cex=0.2) + 
  facet_wrap(lieu~ID_PIEGE)

## Boxplots Albo ~ conditions météo
df_meteo <- read.csv("data/processed_data/meteo_macro.csv", stringsAsFactors = F) %>% 
  mutate(date = as.Date(date)) %>%
  filter(date > min(df_model$DATE_COLLECTE) - 30, date < max(df_model$DATE_COLLECTE) + 30) %>%
  mutate(week = floor_date(date, "weeks")) %>%
  group_by(week) %>%
  summarise(precipitations = sum(precipitations, na.rm = T), tmin = mean(tmin, na.rm = T), tmax = mean(tmax, na.rm = T), tmean = mean(tmean, na.rm = T)) 
  

scaleFactor <- max(df_meteo$precipitations, na.rm = T) / max(df_model$NB_ALBO_TOT, na.rm = T)

plot_albo_precipitations <-  ggplot() + 
  geom_line(aes(x = df_meteo$week, y = df_meteo$precipitations), size = 0.5, show.legend = FALSE, color='steelblue') +
  geom_boxplot(aes(x = df_model$week, y = df_model$NB_ALBO_TOT * scaleFactor, group = df_model$week), show.legend = FALSE, outlier.shape=NA) + 
  geom_jitter(aes(x = df_model$week, y = df_model$NB_ALBO_TOT * scaleFactor, group = df_model$week), position=position_jitter(3), cex=0.2) + 
  scale_y_continuous(name = "precipitations", sec.axis = sec_axis(~./scaleFactor, name = "nb albo")) +
  scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal(base_size = 10) +
  ggtitle("Precipitations")


scaleFactor <- max(df_meteo$tmax, na.rm = T) / max(df_model$NB_ALBO_TOT, na.rm = T)

plot_albo_temperature <-  ggplot() + 
  geom_line(aes(x = df_meteo$week, y = df_meteo$tmin), size = 0.5, show.legend = FALSE, color='steelblue') +
  geom_line(aes(x = df_meteo$week, y = df_meteo$tmax), size = 0.5, show.legend = FALSE, color='red') +
  geom_line(aes(x = df_meteo$week, y = df_meteo$tmean), size = 0.5, show.legend = FALSE, color='blue') +
  geom_boxplot(aes(x = df_model$week, y = df_model$NB_ALBO_TOT * scaleFactor, group = df_model$week), show.legend = FALSE, outlier.shape=NA) + 
  geom_jitter(aes(x = df_model$week, y = df_model$NB_ALBO_TOT * scaleFactor, group = df_model$week), position=position_jitter(3), cex=0.2) + 
  scale_y_continuous(name = "temperature min and max", sec.axis = sec_axis(~./scaleFactor, name = "nb albo")) +
  scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal(base_size = 10) +
  ggtitle("temperature")


ggplot(df_model, aes(x = RFSUM_collection	, y = NB_ALBO_TOT)) + geom_point()  + geom_smooth(method = "lm") #+ facet_wrap(.~lieu)
