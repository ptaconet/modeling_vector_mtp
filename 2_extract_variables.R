library(terra)
library(landscapemetrics)
library(sf)
library(tidyverse)
library(exactextractr)
library(raster)
library(furrr)
library(lubridate)

pieges <- st_read("localisation_piege_provisoire.gpkg")
df_releves_pieges <- read.csv("collecte_BG_labo_mai_juin_V3.csv", stringsAsFactors = F, sep = ";")


occ_sol_rast <- raster("data/processed_data/occ_sol.tif")
#occ_sol_data_dict <- read.csv("data/processed_data/occ_sol_data_dic.csv", stringsAsFactors = F)
vegetation_rast <- raster("data/processed_data/vegetation.tif")
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
meteo <- read.csv("data/processed_data/meteo_macro.csv", stringsAsFactors = F)
  
buffer_sizes <- c(20,50,100,250)
lag_max <- 42

plan(multiprocess)
options(future.globals.maxSize= 20000*1024^2)

## calcul des métriques paysagères

# végétation

df_lsm_vegetation <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  furrr::future_map_dfr(~landscapemetrics::sample_lsm(landscape = vegetation_rast,
                                               y =  st_transform(pieges, raster::crs(vegetation_rast)),
                                               plot_id = pieges$ID_PIEGE,
                                               what = c("lsm_c_pland","lsm_l_ent"),
                                               shape = "circle",
                                               size = .),
                 .id = "buffer")

# df_lsm_vegetation <- df_lsm_vegetation %>%
#   left_join(vegetation_data_dict)


# occupation du sol

df_lsm_occsol <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  furrr::future_map_dfr(~landscapemetrics::sample_lsm(landscape = occ_sol_rast,
                                                      y =  st_transform(pieges, raster::crs(occ_sol_rast)),
                                                      plot_id = pieges$ID_PIEGE,
                                                      what = c("lsm_c_pland","lsm_l_ent"),
                                                      shape = "circle",
                                                      size = .),
                        .id = "buffer")

# df_lsm_occsol <- df_lsm_occsol %>%
#   left_join(occ_sol_data_dict)

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
df_filosofi <- sf::st_intersection(pieges_proj,filosofi) %>% st_drop_geometry()

# hauteur du bati dans les zones tampon
pieges_proj <- st_transform(pieges,terra::crs(bati))

df_hauteur_bati <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(pieges_proj,.), sf::st_centroid(bati)),
                 .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,ID_PIEGE) %>%
  summarise(num_batiments = n(), mean_hauteur = mean(hauteur,na.rm=T), sd_hauteur = sd(hauteur, na.rm = T), min_hauteur = min(hauteur, na.rm = T), max_hauteur = max(hauteur, na.rm = T)) %>%
  complete(buffer, ID_PIEGE,  fill = list(num_batiments = 0, mean_hauteur = NA, sd_hauteur = NA, min_hauteur = NA, max_hauteur = NA)) %>%
  as_tibble()


# distance au batiment le plus proche
nearest <- st_nearest_feature(pieges_proj,bati)
dist <- st_distance(pieges_proj, bati[nearest,], by_element=TRUE) %>% as.numeric()
df_dist <- data.frame(ID_PIEGE = pieges$ID_PIEGE, DBT = dist)


# surface de bati dans les zones tampon
df_surf_bati <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(pieges_proj,.), bati),
                 .id = "buffer") %>%
  mutate(aire = as.numeric(st_area(geom))) %>%
  st_drop_geometry() %>%
  mutate(buffer = as.numeric(buffer)) %>%
  group_by(buffer,ID_PIEGE) %>%
  summarise(tot_surf = sum(aire,na.rm=T), mean_surf = mean(aire,na.rm=T)) %>%
  complete(buffer, ID_PIEGE,  fill = list(tot_surf = 0, mean_surf = 0)) %>%
  as_tibble()


# population
pieges_proj <- st_transform(pieges,terra::crs(pop))

POP <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(pieges_proj,.), pop),
                 .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,ID_PIEGE) %>%
  summarise(somme = sum(pop_2016), sd = sd(pop_2016)) %>%
  complete(buffer, ID_PIEGE,  fill = list(somme = 0, sd = 0)) %>%
  as_tibble()


## données temporelles (météo)

meteo <- meteo %>%
  group_by(jour) %>%
  summarise(RFD = sum(rr3, na.rm = T), HUM = mean(u)) %>%
  mutate(RFD = ifelse(RFD < 0, 0, RFD)) %>%
  rename(date = jour)

df_releves_pieges <- df_releves_pieges %>%
  mutate(DATE_POSE = parse_date(DATE_POSE,"%d/%m/%Y")) %>%
  mutate(DATE_COLLECTE = parse_date(DATE_COLLECTE,"%d/%m/%Y")) %>%
  dplyr::select(ID_COLLECTE, ID_PIEGE, DATE_COLLECTE)


df_meteo_pieges <- data.frame(ID_COLLECTE = character(), ID_PIEGE = character(), date = character(), stringsAsFactors = F)
for(i in 1:nrow(df_releves_pieges)){
  for(j in 0:lag_max){
    df_meteo_pieges <- rbind(df_meteo_pieges,
                             data.frame(ID_COLLECTE = df_releves_pieges$ID_COLLECTE[i],
                                        ID_PIEGE = df_releves_pieges$ID_PIEGE[i],
                                        date = as.character(as.Date(df_releves_pieges$DATE_COLLECTE[i]-j)),
                                        lag_n = j,
                                        stringsAsFactors = F))
  }
}


###### restructuration des tables

metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

df_lsm_vegetation <- df_lsm_vegetation %>%
  dplyr::select(-c(id,percentage_inside)) %>%
  rename(val=value,pixval=class) %>%
  mutate(ID_PIEGE=plot_id) %>%
  mutate(buffer=as.numeric(buffer)) %>%
  mutate(layer_id = "VEG") %>%
  dplyr::select(ID_PIEGE,buffer,pixval,level,metric,val,layer_id) %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%
  mutate_all(funs(replace_na(.,0)))


df_lsm_occsol <- df_lsm_occsol %>%
  dplyr::select(-c(id,percentage_inside)) %>%
  rename(val=value,pixval=class) %>%
  mutate(ID_PIEGE=plot_id) %>%
  mutate(buffer=as.numeric(buffer)) %>%
  mutate(layer_id = "OCS") %>%
  dplyr::select(ID_PIEGE,buffer,pixval,level,metric,val,layer_id) %>%
  left_join(metrics_defs) %>%
  dplyr::select(-c(level,metric,name,type)) %>%
  pivot_wider(names_from = c(function_name,layer_id,buffer,pixval), values_from = val, names_sep = "_", values_fill = list(val = 0)) %>%
  mutate_all(funs(replace_na(.,0)))


IMP <- IMP %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "IMP_{buffer}_{.value}",)#, values_fill = list(val = 0))  

IMP2 <- IMP2 %>% 
  rename(IMP_0 = impermeabilite)

HVG <- HVG %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "HVG_{buffer}_{.value}",)#, values_fill = list(val = 0))  

HVG2 <- HVG2 %>% 
  rename(HVG_0 = mns)

MNE <- MNE %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "ELE_{buffer}_{.value}",)#, values_fill = list(val = 0))  

# LCZ = LCZ
# df_filosofi = sf_filosofi
# df_dist = df_dist

df_hauteur_bati <- df_hauteur_bati %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "BAT_{buffer}_{.value}",)#, values_fill = list(val = 0))  

df_surf_bati <- df_surf_bati %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "BAT_{buffer}_{.value}",)#, values_fill = list(val = 0))  

POP <- POP %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "POP_{buffer}_{.value}",)#, values_fill = list(val = 0))  

df_meteo_pieges <- df_meteo_pieges %>% 
  left_join(meteo) %>%
  dplyr::select(-c(ID_PIEGE,date)) %>%
  pivot_wider(names_from = lag_n, values_from = -c(lag_n,ID_COLLECTE), names_glue = "{.value}_{lag_n}", values_fn = mean) %>%
  left_join(df_releves_pieges) %>%
  relocate(ID_PIEGE, DATE_COLLECTE, .after = ID_COLLECTE)
  
  
# join all data to create 1 big dataset

