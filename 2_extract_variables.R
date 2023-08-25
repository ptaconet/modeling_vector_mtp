library(terra)
library(landscapemetrics)
library(sf)
library(tidyverse)
library(exactextractr)
library(raster)
library(furrr)
library(lubridate)

pieges <- st_read("localisation_piege_provisoire.gpkg") %>% 
  filter(TYPE_PIEGE == 'bg-sentinel')

df_releves_pieges <- read.csv("collecte_BG_labo.csv", stringsAsFactors = F, sep = ";") %>%
  mutate(DATE_COLLECTE = parse_date(DATE_COLLECTE,"%d/%m/%Y")) %>%
  mutate(ID_PIEGE = case_when(ID_PIEGE %in% c("BG_12","BG_13") ~ "BG_12_13",
                              ID_PIEGE %in% c("BG_15","BG_16") ~ "BG_15_16",
                              TRUE ~ ID_PIEGE)) %>%
  group_by(ID_PIEGE, DATE_COLLECTE, NB_ALBO_TOT) %>%
  summarise(NB_ALBO_TOT = sum(NB_ALBO_TOT)) %>%
  as_tibble() %>%
  mutate(ID_COLLECTE = seq(1,nrow(.),1))



landuse_rast <- raster("data/processed_data/landuse.tif")
#landuse_data_dict <- read.csv("data/processed_data/landuse_data_dic.csv", stringsAsFactors = F)
landcover_rast <- raster("data/processed_data/landcover.tif")
#landcover_data_dict <- read.csv("data/processed_data/landcover_data_dic.csv", stringsAsFactors = F)
landcover_grouped_veget_rast <- raster("data/processed_data/landcover_grouped_veget.tif")
#landcover_grouped_veget_data_dict <- read.csv("data/processed_data/landcover_grouped_veget_data_dic.csv", stringsAsFactors = F)
vegetation_rast <- raster("data/processed_data/vegetation.tif")
vegetation_vect <- st_read("data/MMM_MMM_VegFine/MMM_MMM_VegFine.shp")
#vegetation_data_dict <- read.csv("data/processed_data/vegetation_data_dic.csv", stringsAsFactors = F)
mnt <- rast("data/processed_data/mnt.tif")
mns <- rast("data/processed_data/mns.tif")
mne <- rast("data/processed_data/mne.tif")
mne_veget <- rast("data/processed_data/mne_veget.tif")
impermeabilite <- rast("data/processed_data/impermeabilite.tif")
pop <- st_read("data/processed_data/pop.gpkg")
filosofi <- st_read("data/processed_data/filosofi.gpkg")
bati <- st_read("data/processed_data/bati.gpkg")
lcz <- st_read("data/processed_data/lcz.gpkg") %>% st_make_valid()
eau <- st_read("data/processed_data/point_eau.gpkg") %>% mutate(V_EAU = as.numeric(as.character(V_EAU)))
meteo <- read.csv("data/processed_data/meteo_macro.csv", stringsAsFactors = F)


buffer_sizes <- c(20,50,100,250)
lag_max <- 42

plan(multiprocess)
options(future.globals.maxSize= 20000*1024^2)

## calcul des métriques paysagères

# occupation du sol - 3 classes (végétation sup 3m, végétation inf 3m, bati, routes, autres)

df_lsm_landcover_veget <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  furrr::future_map_dfr(~landscapemetrics::sample_lsm(landscape = landcover_grouped_veget_rast,
                                                      y =  st_transform(pieges, raster::crs(landcover_grouped_veget_rast)),
                                                      plot_id = pieges$ID_PIEGE,
                                                      what = c("lsm_c_ca","lsm_c_area_mn", "lsm_c_area_sd", "lsm_c_contig_mn", "lsm_c_contig_sd", "lsm_c_ed", "lsm_c_frac_mn", "lsm_c_frac_sd", "lsm_c_pd", "lsm_c_pland" , "lsm_l_area_mn", "lsm_l_area_sd", "lsm_l_cohesion", "lsm_l_contag" ,"lsm_l_condent", "lsm_l_ed", "lsm_l_ent", "lsm_l_frac_mn", "lsm_l_frac_sd", "lsm_l_pd", "lsm_l_pr", "lsm_l_prd", "lsm_l_shdi", "lsm_l_shei", "lsm_l_sidi", "lsm_l_siei"),
                                                      shape = "circle",
                                                      size = .,
                                                      all_classes = T),
                        .id = "buffer")

# landcover_grouped_veget_rast <- landcover_grouped_veget_rast %>%
#   left_join(landcover_grouped_veget_data_dict)

# occupation du sol

df_lsm_landcover <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  furrr::future_map_dfr(~landscapemetrics::sample_lsm(landscape = landcover_rast,
                                               y =  st_transform(pieges, raster::crs(landcover_rast)),
                                               plot_id = pieges$ID_PIEGE,
                                               what = c("lsm_c_ca","lsm_c_area_mn", "lsm_c_area_sd", "lsm_c_contig_mn", "lsm_c_contig_sd", "lsm_c_ed", "lsm_c_frac_mn", "lsm_c_frac_sd", "lsm_c_pd", "lsm_c_pland" , "lsm_l_area_mn", "lsm_l_area_sd", "lsm_l_cohesion", "lsm_l_contag" ,"lsm_l_condent", "lsm_l_ed", "lsm_l_ent", "lsm_l_frac_mn", "lsm_l_frac_sd", "lsm_l_pd", "lsm_l_pr", "lsm_l_prd", "lsm_l_shdi", "lsm_l_shei", "lsm_l_sidi", "lsm_l_siei"),
                                               shape = "circle",
                                               size = .,
                                               all_classes = T),
                 .id = "buffer")

# df_lsm_landcover <- df_lsm_landcover %>%
#   left_join(landcover_data_dict)


# utilisation du sol

df_lsm_landuse <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  furrr::future_map_dfr(~landscapemetrics::sample_lsm(landscape = landuse_rast,
                                                      y =  st_transform(pieges, raster::crs(landuse_rast)),
                                                      plot_id = pieges$ID_PIEGE,
                                                      what = c("lsm_c_ca","lsm_c_area_mn", "lsm_c_area_sd", "lsm_c_contig_mn", "lsm_c_contig_sd", "lsm_c_ed", "lsm_c_frac_mn", "lsm_c_frac_sd", "lsm_c_pd", "lsm_c_pland" , "lsm_l_area_mn", "lsm_l_area_sd", "lsm_l_cohesion", "lsm_l_contag" ,"lsm_l_condent", "lsm_l_ed", "lsm_l_ent", "lsm_l_frac_mn", "lsm_l_frac_sd", "lsm_l_pd", "lsm_l_pr", "lsm_l_prd", "lsm_l_shdi", "lsm_l_shei", "lsm_l_sidi", "lsm_l_siei"),
                                                      shape = "circle",
                                                      size = .,
                                                      all_classes = T),
                        .id = "buffer")

# df_lsm_landuse <- df_lsm_landuse %>%
#   left_join(landuse_data_dict)

# impermeabilisation des sols
# dans les zones tampon : 
pieges_proj <- st_transform(pieges,terra::crs(impermeabilite))

IMP <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~exactextractr::exact_extract(impermeabilite,
                                               st_buffer(st_as_sf(pieges_proj),.),
                                               fun = c("mean","stdev"),
                                               append_cols = "ID_PIEGE"),
                 .id = "buffer")

# au niveau du piège : 
IMP2 <- terra::extract(impermeabilite, pieges_proj)
pieges_proj$ID <- as.numeric(rownames(pieges_proj))
IMP2 <- left_join(IMP2,pieges_proj) %>%
  dplyr::select(ID_PIEGE, impermeabilite)


## hauteur de la végétation
# dans les zones tampon : 
pieges_proj <- st_transform(pieges,terra::crs(mne_veget))

HVG <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~exactextractr::exact_extract(mne_veget,
                                               st_buffer(st_as_sf(pieges_proj),.),
                                               fun = c("mean","stdev","min","max"),
                                               append_cols = "ID_PIEGE"),
                 .id = "buffer")

# au niveau du piège : 
HVG2 <- terra::extract(mne_veget,st_as_sf(pieges_proj))
HVG2$mns[which(is.na(HVG2$mns))]<-0
pieges_proj$ID <- as.numeric(rownames(pieges_proj))
HVG2 <- left_join(HVG2,pieges_proj) %>%
  dplyr::select(ID_PIEGE, mns)


# hauteur moyenne des batiments, végétation, infrastructures, et écart type (-> relief plat ou accidenté )
pieges_proj <- st_transform(pieges,terra::crs(mne))

MNE <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~exactextractr::exact_extract(mne,
                                               st_buffer(st_as_sf(pieges_proj),.),
                                               fun = c("mean","stdev"),
                                               append_cols = "ID_PIEGE"),
                 .id = "buffer")


# LCZ  (pour remplacer la variable "ZONE")
pieges_proj <- st_transform(pieges,terra::crs(lcz))
LCZ <- sf::st_intersection(pieges_proj,lcz) %>% st_drop_geometry()

# filosofi
pieges_proj <- st_transform(pieges,terra::crs(filosofi))
df_filosofi <- sf::st_intersection(pieges_proj,filosofi) %>% 
  st_drop_geometry() %>%
  dplyr::select(ID_PIEGE, Men, Men_pauv, Ind_snv, Log_av45, Log_45_70, Log_70_90, Log_ap90, Log_soc)

# hauteur du bati dans les zones tampon
pieges_proj <- st_transform(pieges,terra::crs(bati))

df_hauteur_bati <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(pieges_proj,.), sf::st_centroid(bati)),
                 .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,ID_PIEGE) %>%
  summarise(BATN = n(), BATH_mn = mean(hauteur,na.rm=T), BATH_sd = sd(hauteur, na.rm = T), BATH_min = min(hauteur, na.rm = T), BATH_max = max(hauteur, na.rm = T)) %>%
  complete(buffer, ID_PIEGE,  fill = list(BATN = 0, BATH_mn = NA, BATH_sd = NA, BATH_min = NA, BATH_max = NA)) %>%
  as_tibble()


# distance au batiment le plus proche
nearest <- st_nearest_feature(pieges_proj,bati)
dist <- st_distance(pieges_proj, bati[nearest,], by_element=TRUE) %>% as.numeric()
df_dist_bat_plus_proche <- data.frame(ID_PIEGE = pieges$ID_PIEGE, DBT = dist)

# distance au patch de végétation le plus proche
pieges_proj <- st_transform(pieges,terra::crs(vegetation_vect))
nearest <- st_nearest_feature(pieges_proj,vegetation_vect)
dist <- st_distance(pieges_proj, vegetation_vect[nearest,], by_element=TRUE) %>% as.numeric()
df_dist_veg_plus_proche <- data.frame(ID_PIEGE = pieges$ID_PIEGE, DVG = dist)


# surface de bati dans les zones tampon
df_surf_bati <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(pieges_proj,.), bati),
                 .id = "buffer") %>%
  mutate(aire = as.numeric(st_area(geom))) %>%
  st_drop_geometry() %>%
  mutate(buffer = as.numeric(buffer)) %>%
  group_by(buffer,ID_PIEGE) %>%
  summarise(BATS_sum = sum(aire,na.rm=T), BATS_mn = mean(aire,na.rm=T)) %>%
  complete(buffer, ID_PIEGE,  fill = list(BATS_sum = 0, BATS_mn = 0)) %>%
  as_tibble()


# population
pieges_proj <- st_transform(pieges,terra::crs(pop))

POP <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(pieges_proj,.), pop),
                 .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,ID_PIEGE) %>%
  summarise(sum = sum(pop_2016), sd = sd(pop_2016)) %>%
  complete(buffer, ID_PIEGE,  fill = list(sum = 0, sd = 0)) %>%
  as_tibble()

## points d'eau à proximité
pieges_proj <- st_transform(pieges,terra::crs(eau))

GLP_nb <-  buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(pieges_proj,.), eau),
                                         .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,ID_PIEGE) %>%
  summarise(nb = n(), sum_vol = sum(V_EAU)) %>%
  complete(buffer, ID_PIEGE,  fill = list(nb = 0, sum_vol = 0)) %>%
  as_tibble()

 

nearest <- st_nearest_feature(pieges_proj,eau)
dist <- st_distance(pieges_proj, eau[nearest,], by_element=TRUE) %>% as.numeric()
df_dist_eau_plus_proche <- data.frame(ID_PIEGE = pieges$ID_PIEGE, DEAU = dist)

GLP_type_nearest <- pieges_proj %>%
  bind_cols( eau[nearest,]) %>%
  left_join(df_dist_eau_plus_proche) %>%
  dplyr::select(ID_PIEGE, EXPO_SOLEIL, CLASSE_GLP, NATURE_GLP,  TYPE_GLP, DESCRIPT_GLP, TEMPO_GLP, V_EAU, ASPECT_EAU,  DEAU )

## données temporelles (météo)

# meteo <- meteo %>%
#   group_by(jour) %>%
#   summarise(RFD = sum(rr3, na.rm = T), HUM = mean(u)) %>%
#   mutate(RFD = ifelse(RFD < 0, 0, RFD)) %>%
#   rename(date = jour)

meteo <- meteo %>%
  rename(RFD = precipitations, TMIN = tmin, TMAX = tmax, TMN = tmean) %>%
  filter(!is.na(RFD), !is.na(TMIN), !is.na(TMAX), !is.na(TMN))

df_releves_pieges2 <- df_releves_pieges %>%
  #mutate(DATE_POSE = parse_date(DATE_POSE,"%d/%m/%Y")) %>%
  dplyr::select(ID_COLLECTE, ID_PIEGE, DATE_COLLECTE)


df_meteo_pieges <- data.frame(ID_COLLECTE = numeric(),ID_PIEGE = character(), date = character(), stringsAsFactors = F)
for(i in 1:nrow(df_releves_pieges2)){
  for(j in 0:lag_max){
    df_meteo_pieges <- rbind(df_meteo_pieges,
                             data.frame(ID_COLLECTE = df_releves_pieges2$ID_COLLECTE[i],
                                        ID_PIEGE = df_releves_pieges2$ID_PIEGE[i],
                                        date = as.character(as.Date(df_releves_pieges2$DATE_COLLECTE[i]-j)),
                                        lag_n = j,
                                        stringsAsFactors = F))
  }
}

# summarizing to weeks
df_meteo_pieges2 <- df_meteo_pieges %>% 
  left_join(meteo) %>%
  pivot_longer( !(ID_COLLECTE:lag_n), names_to = "var", values_to = 'val') %>%
  mutate(idpointdecapture = paste0(ID_PIEGE,"_",ID_COLLECTE))


fun_summarize_week <- function(df_meteo_pieges2,var_to_summarize){
  
  if(var_to_summarize=="RFD"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      group_by(idpointdecapture,lag_n = lubridate::week(date)) %>%
      summarise(val=sum(val, na.rm = T),date = min(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(n()-1,0,-1)) %>%
      mutate(var = var_to_summarize) %>%
      as_tibble()
  } else {
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      group_by(idpointdecapture,lag_n = lubridate::week(date)) %>%
      summarise(val=mean(val, na.rm = T),date = min(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(n()-1,0,-1)) %>%
      mutate(var = var_to_summarize) %>%
      as_tibble()
  }
  return(df_meteo_pieges2_summarize)
  
}

df_meteo_pieges_summ <- fun_summarize_week(df_meteo_pieges2,"RFD") %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TMIN")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TMAX")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TMN"))

# function to create the data.frame for CCM
fun_ccm_df <- function(df_timeseries, varr, function_to_apply){
  
  df_timeseries_wide <- df_timeseries %>%
    filter(var==varr) %>%
    dplyr::select(-c("date","var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr,"_"))
  
  max_col <- ncol(df_timeseries_wide)
  
  for(i in 2:(max_col-1)){
    for(j in (i+1):max_col){
      column_name <- paste0(colnames(df_timeseries_wide[i]),"_",(j-2))
      if(function_to_apply=="mean"){
        df_timeseries_wide[column_name] <- rowMeans(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="sum"){
        df_timeseries_wide[column_name] <- rowSums(df_timeseries_wide[,i:j], na.rm = T)
      }
    }
  }
  
  for(i in 2:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }
  
  return(df_timeseries_wide)
  
}


df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ,"RFD","sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ,"TMIN","mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ,"TMAX","mean")
df_meteo_pieges_summ_wide4 <- fun_ccm_df(df_meteo_pieges_summ,"TMN","mean")

df_meteo_pieges_summ_wide <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide3) %>%
  left_join(df_meteo_pieges_summ_wide4)

###### restructuration des tables

metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

df_lsm_landcover_veget <- df_lsm_landcover_veget %>%
  dplyr::select(-c(id,percentage_inside)) %>%
  rename(val=value,pixval=class) %>%
  mutate(ID_PIEGE=plot_id) %>%
  mutate(buffer=as.numeric(buffer)) %>%
  mutate(layer_id = "LCG") %>%
  dplyr::select(ID_PIEGE,buffer,pixval,level,metric,val,layer_id) %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  #pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%  
  pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_") %>%
  mutate_at(vars(contains(c('lsm_c_area','lsm_c_pland','lsm_c_ca'))),funs(replace_na(.,0))) %>%
  mutate(ID_PIEGE = as.character(ID_PIEGE))


df_lsm_landcover <- df_lsm_landcover %>%
  dplyr::select(-c(id,percentage_inside)) %>%
  rename(val=value,pixval=class) %>%
  mutate(ID_PIEGE=plot_id) %>%
  mutate(buffer=as.numeric(buffer)) %>%
  mutate(layer_id = "LCV") %>%
  dplyr::select(ID_PIEGE,buffer,pixval,level,metric,val,layer_id) %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  #pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%  
  pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_") %>%
  mutate_at(vars(contains(c('lsm_c_area','lsm_c_pland','lsm_c_ca'))),funs(replace_na(.,0))) %>%
  mutate(ID_PIEGE = as.character(ID_PIEGE))


df_lsm_landuse <- df_lsm_landuse %>%
  dplyr::select(-c(id,percentage_inside)) %>%
  rename(val=value,pixval=class) %>%
  mutate(ID_PIEGE=plot_id) %>%
  mutate(buffer=as.numeric(buffer)) %>%
  mutate(layer_id = "LUS") %>%
  dplyr::select(ID_PIEGE,buffer,pixval,level,metric,val,layer_id) %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  #pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%  
  pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_") %>%
  mutate_at(vars(contains(c('lsm_c_area','lsm_c_pland','lsm_c_ca'))),funs(replace_na(.,0))) %>%
  mutate(ID_PIEGE = as.character(ID_PIEGE))


IMP <- IMP %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "IMP_{buffer}_{.value}") %>%#, values_fill = list(val = 0))  
  mutate(ID_PIEGE = as.character(ID_PIEGE))

IMP2 <- IMP2 %>% 
  rename(IMP_0 = impermeabilite) %>%
  mutate(ID_PIEGE = as.character(ID_PIEGE))

HVG <- HVG %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "HVG_{buffer}_{.value}") %>%#, values_fill = list(val = 0))  
  mutate(ID_PIEGE = as.character(ID_PIEGE))

HVG2 <- HVG2 %>% 
  rename(HVG_0 = mns) %>%
  mutate(ID_PIEGE = as.character(ID_PIEGE))

MNE <- MNE %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "ELE_{buffer}_{.value}") %>%#, values_fill = list(val = 0))  
  mutate(ID_PIEGE = as.character(ID_PIEGE))

LCZ = LCZ %>%
  mutate(ID_PIEGE = as.character(ID_PIEGE)) %>%
  dplyr::select(ID_PIEGE, lib19_ni_1) %>%
  rename(LCZ = lib19_ni_1)
 
df_filosofi = df_filosofi %>%
   mutate(ID_PIEGE = as.character(ID_PIEGE)) %>%
  dplyr::select(-c("ZONE", "TYPE_PIEGE", "LATITUDE", "LONGITUDE"))
colnames(df_filosofi) <- paste0('FIL_',colnames(df_filosofi))
colnames(df_filosofi)[1] <- 'ID_PIEGE'

df_dist_bat_plus_proche = df_dist_bat_plus_proche %>%
   mutate(ID_PIEGE = as.character(ID_PIEGE))
 
df_dist_veg_plus_proche = df_dist_veg_plus_proche %>%
  mutate(ID_PIEGE = as.character(ID_PIEGE))

df_hauteur_bati <- df_hauteur_bati %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "{.value}_{buffer}") %>%#, values_fill = list(val = 0))  
  mutate(ID_PIEGE = as.character(ID_PIEGE))

df_surf_bati <- df_surf_bati %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "{.value}_{buffer}") %>%#, values_fill = list(val = 0))  
  mutate(ID_PIEGE = as.character(ID_PIEGE))

POP <- POP %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "POP_{buffer}_{.value}") %>%#, values_fill = list(val = 0))  
  mutate(ID_PIEGE = as.character(ID_PIEGE))

GLP_nb <- GLP_nb %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "GLP_{buffer}_{.value}") %>%#, values_fill = list(val = 0))  
  mutate(ID_PIEGE = as.character(ID_PIEGE))

colnames(GLP_type_nearest) <- paste0("GLP_nearest_",colnames(GLP_type_nearest))
colnames(GLP_type_nearest)[1] <- "ID_PIEGE"


df_meteo_pieges_summ_wide <- df_meteo_pieges_summ_wide %>%
  mutate(ID_PIEGE = sub('_[^_]*$', '', idpointdecapture)) %>%
  mutate(ID_COLLECTE = as.numeric(sub('.*\\_', '', idpointdecapture))) %>%
  left_join(df_releves_pieges2) %>%
  relocate(ID_PIEGE,ID_COLLECTE,DATE_COLLECTE, .after = idpointdecapture)
  

# df_meteo_pieges <- df_meteo_pieges %>% 
#   left_join(meteo) %>%
#   dplyr::select(-c(ID_PIEGE,date)) %>%
#   pivot_wider(names_from = lag_n, values_from = -c(lag_n,ID_COLLECTE), names_glue = "{.value}_{lag_n}", values_fn = mean) %>%
#   left_join(df_releves_pieges) %>%
#   relocate(ID_PIEGE, DATE_COLLECTE, .after = ID_COLLECTE)

# join all data to create 1 big dataset

df_model <- df_meteo_pieges_summ_wide %>%
  left_join(df_releves_pieges) %>%
  left_join(pieges %>% st_drop_geometry(), by = "ID_PIEGE") %>%
  left_join(df_lsm_landuse) %>%
  left_join(df_lsm_landcover) %>%
  left_join(df_lsm_landcover_veget) %>%
  left_join(IMP) %>%
  left_join(IMP2) %>%
  left_join(HVG) %>%
  left_join(HVG2) %>%
  left_join(MNE) %>%
  left_join(LCZ) %>%
  left_join(df_filosofi) %>%
  left_join(df_dist_bat_plus_proche) %>%
  left_join(df_dist_veg_plus_proche) %>%
  left_join(df_hauteur_bati) %>%
  left_join(df_surf_bati) %>%
  left_join(POP) %>%
  left_join(GLP_nb) %>%
  left_join(GLP_type_nearest) %>%
  relocate(NB_ALBO_TOT, .after = DATE_COLLECTE) %>%
  relocate(ZONE, TYPE_PIEGE, LATITUDE, LONGITUDE, lieu, .after = DATE_COLLECTE)
  
write.csv(df_model,"df_model.csv")
