library(terra)
library(landscapemetrics)
library(sf)
library(tidyverse)
library(exactextractr)
library(raster)
library(furrr)

pieges <- st_read("localisation_piege_provisoire.gpkg")


occ_sol_rast <- raster("data/processed_data/occ_sol.tif")
vegetation_rast <- raster("data/processed_data/vegetation.tif")
mnt <- rast("data/processed_data/mnt.tif")
mns <- rast("data/processed_data/mns.tif")
mne <- rast("data/processed_data/mne.tif")
mne_veget <- rast("data/processed_data/mne_veget.tif")
impermeabilite <- rast("data/processed_data/impermeabilite.tif")
pop <- st_read("data/processed_data/pop.gpkg")
filosofi <- st_read("data/processed_data/filosofi.gpkg")
bati <- st_read("data/processed_data/bati.gpkg")
lcz <- st_read("data/processed_data/lcz.gpkg")

buffer_sizes <- c(20,50,100,250)

plan(multiprocess)
options(future.globals.maxSize= 20000*1024^2)

## calcul des métriques paysagères
# végétation

df_lsm_vegetation <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  furrr::future_map_dfr(~landscapemetrics::sample_lsm(landscape = vegetation_rast,
                                               y =  st_transform(pieges, crs(vegetation_rast)),
                                               plot_id = pieges$ID_PIEGE,
                                               what = c("lsm_c_pland","lsm_l_ent"),
                                               shape = "circle",
                                               size = .),
                 .id = "buffer")


# occupation du sol

df_lsm_occsol <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  furrr::future_map_dfr(~landscapemetrics::sample_lsm(landscape = occ_sol_rast,
                                                      y =  st_transform(pieges, crs(occ_sol_rast)),
                                                      plot_id = pieges$ID_PIEGE,
                                                      what = c("lsm_c_pland","lsm_l_ent"),
                                                      shape = "circle",
                                                      size = .),
                        .id = "buffer")


# impermeabilisation des sols
# dans les zones tampon : 
pieges_proj <- st_transform(pieges,crs(impermeabilite))

IMP <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~exactextractr::exact_extract(impermeabilite,
                                               st_buffer(st_as_sf(pieges_proj),.),
                                               fun = c("mean","stdev"),
                                               append_cols = "ID_PIEGE"),
                 .id = "buffer") %>%
  mutate(var = "IMP")

# au niveau du piège : 
IMP2 <- extract(impermeabilite, pieges_proj)
pieges_proj$ID <- as.numeric(rownames(pieges_proj))
IMP2 <- left_join(IMP2,pieges_proj) %>%
  dplyr::select(ID_PIEGE, impermeabilite)


## hauteur de la végétation
# dans les zones tampon : 
pieges_proj <- st_transform(pieges,crs(mne_veget))

HVG <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~exactextractr::exact_extract(mne_veget,
                                               st_buffer(st_as_sf(pieges_proj),.),
                                               fun = c("mean","stdev"),
                                               append_cols = "ID_PIEGE"),
                 .id = "buffer") %>%
  mutate(var = "HVG")

# au niveau du piège : 
HVG2 <- extract(mne_veget,st_as_sf(pieges_proj))
HVG2$mns[which(is.na(HVG2$mns))]<-0
pieges_proj$ID <- as.numeric(rownames(pieges_proj))
HVG2 <- left_join(HVG2,pieges_proj) %>%
  dplyr::select(ID_PIEGE, mns)

# LCZ
pieges_proj <- st_transform(pieges,crs(lcz))
LCZ <- sf::st_intersection(pieges_proj,lcz)

# filosofi
pieges_proj <- st_transform(pieges,crs(filosofi))
df_filosofi <- sf::st_intersection(pieges_proj,filosofi)

# hauteur du bati
pieges_proj <- st_transform(pieges,crs(bati))

df_hauteur_bati <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  st_intersection(st_centroid(bati)) %>%
  group_by(ID_PIEGE) %>%
  summarise(mean_hauteur = mean(hauteur,na.rm=T), sd_hauteur = sd(hauteur, na.rm = T), min_hauteur = min(hauteur, na.rm = T), max_hauteur = max(hauteur, na.rm = T)) %>%
  st_drop_geometry()


# distance au bati
nearest <- st_nearest_feature(pieges_proj,bati)
dist <- st_distance(pieges_proj, bati[nearest,], by_element=TRUE) %>% as.numeric()
df_dist <- data.frame(id_piege = pieges$ID_PIEGE, DBT = dist)

# population
pieges_proj <- st_transform(pieges,crs(pop))

POP <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  purrr::map_dfr(~sf::st_intersection(st_buffer(pieges_proj,.), pop),
                 .id = "buffer") %>%
  st_drop_geometry() %>%
  group_by(buffer,ID_PIEGE) %>%
  summarise(POP = sum(pop_2016)) %>%
  complete(buffer, ID_PIEGE,  fill = list(POP = 0))
