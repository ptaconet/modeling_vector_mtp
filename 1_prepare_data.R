library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(fasterize)
library(rgdal)
library(lubridate)
library(readODS)

## ouverture des données de localisation des pièges
pieges <- st_read("localisation_piege_provisoire.gpkg")

## création de la zone d'intéret (= bounding box autour de la location des pièges)
roi <- sf::st_as_sf(pieges, coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_bbox()

roi[1]<-roi[1]-0.01 #xmin
roi[2]<-roi[2]-0.01 #ymin
roi[3]<-roi[3]+0.01 #xmin
roi[4]<-roi[4]+0.01 #ymin

roi <- roi %>%
  sf::st_as_sfc() %>%
  sf::st_sf() %>%
  st_transform(32631)


## ouverture des données spatiales + découpage selon zone d'intérêt
vegetation <-  st_read("data/MMM_MMM_VegFine/MMM_MMM_VegFine.shp")
occ_sol <-  st_read("data/FR010L2_MONTPELLIER_UA2018_v013/Data/FR010L2_MONTPELLIER_UA2018_v013.gpkg")
pop <-  st_read("data/MMM_MMM_PopFine/MMM_MMM_PopFine.shp")
filosofi <-  st_read("data/Filosofi2015_carreaux_200m_gpkg/Filosofi2015_carreaux_200m_metropole.gpkg")
impermeabilite <- merge(raster("data/IMD_2018_010m_fr_03035_v020/DATA/IMD_2018_010m_E38N23_03035_v020.tif"),raster("data/IMD_2018_010m_fr_03035_v020/DATA/IMD_2018_010m_E38N22_03035_v020.tif"))
temp_diurne <-  raster("data/MMM_MMM_ThermoSatelliteJour/image_satellite_jour_LANDSAT8_22072019.tif")
temp_nocturne <-  raster("data/MMM_MMM_ThermoSatelliteNuit/image_satellite_nuit_ASTER_06072015.tif")
lcz <-  st_read("data/MM_MMM_LCZ/MOS_CLIMAT_M3M_LCZ.shp")
bati <- st_read("data/open_data_millesime_2022-10-d_dep34_gpkg/gpkg/bdnb.gpkg",layer = "batiment_construction")
mns <- list.files("data/VilleMTP_MTP_MNS/02_MNS_50cm", pattern = "tif$", full.names = T) %>% lapply(raster)
mns_to_keep <- NULL
for(i in 1:length(mns)){
 if(!is.null(intersect(extent(mns[[i]]),extent(st_transform(roi,crs(mns[[1]])))))){
   mns_to_keep <- c(mns_to_keep,mns[[i]]@file@name)
 }
}
gdalUtils::gdalwarp(srcfile=mns_to_keep,dstfile="data/processed_data/mns.tif", overwrite = T)


mnt <- list.files("data/mnt_1m", full.names = T) %>% lapply(raster)
mnt_to_keep <- NULL
for(i in 1:length(mnt)){
  crs(mnt[[i]]) <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 
  if(!is.null(intersect(extent(mnt[[i]]),extent(st_transform(roi,crs(mnt[[1]])))))){
    mnt_to_keep <- c(mnt_to_keep,mnt[[i]]@file@name)
  }
}
gdalUtils::gdalwarp(srcfile=mnt_to_keep,dstfile="data/processed_data/mnt.tif", t_srs = "EPSG:2154", overwrite = T)

vegetation <- vegetation %>% st_make_valid() %>% st_crop(st_transform(roi,st_crs(vegetation))) %>% st_cast("POLYGON")
occ_sol <- occ_sol %>% st_crop(st_transform(roi,st_crs(occ_sol))) %>% st_cast("MULTIPOLYGON")
pop <- pop %>% st_crop(st_transform(roi,st_crs(pop)))
filosofi <- filosofi %>% st_crop(st_transform(roi,st_crs(filosofi))) %>% st_cast("MULTIPOLYGON")
impermeabilite <- impermeabilite %>% crop(st_transform(roi,crs(impermeabilite))) 
temp_diurne <- temp_diurne %>% crop(st_transform(roi,crs(temp_diurne)))
temp_nocturne <- temp_nocturne %>% crop(st_transform(roi,crs(temp_nocturne)))
bati <- bati %>% st_crop(st_transform(roi,st_crs(bati))) %>% st_cast("POLYGON")

writeRaster(impermeabilite,"data/processed_data/impermeabilite.tif")

write_sf(pop,"data/processed_data/pop.gpkg")
write_sf(filosofi,"data/processed_data/filosofi.gpkg")
write_sf(bati,"data/processed_data/bati.gpkg")
write_sf(lcz,"data/processed_data/lcz.gpkg")

### conversion des données vectorielles en raster pour calcul métrique paysagères
## vegetation

r <- raster(vegetation, res = 0.5, crs = '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
vegetation_rast <- fasterize(vegetation, r, field="COD_VEG_n", background = 0)

writeRaster(vegetation_rast,"data/processed_data/vegetation.tif", overwrite=TRUE)

# raster attribute table
vegetation_data_dic <- st_drop_geometry(vegetation) %>% mutate_if(is.factor, as.character)
vegetation_data_dic <- unique(vegetation_data_dic[c("COD_VEG_n", "LIB_VEG_n")])
colnames(vegetation_data_dic) <- c("class","label")
vegetation_data_dic <- rbind(vegetation_data_dic,c(0,"Surface non végétalisée"))
write.csv(vegetation_data_dic,"data/processed_data/vegetation_data_dic.csv", row.names = F)

## utilisation du sol
r <- raster(occ_sol, res = 0.5, crs = '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
occ_sol$code_2018=as.numeric(as.character(occ_sol$code_2018))
occ_sol_rast <- fasterize(occ_sol, r, field="code_2018")

writeRaster(occ_sol_rast,"data/processed_data/landuse.tif")

# raster attribute table
occ_sol_data_dic <- st_drop_geometry(occ_sol) %>% mutate_if(is.factor, as.character)
occ_sol_data_dic <- unique(occ_sol_data_dic[c("code_2018", "class_2018")])
colnames(occ_sol_data_dic) <- c("class","label")
write.csv(occ_sol_data_dic,"data/processed_data/landuse_data_dic.csv",row.names = F)

## occupation du sol (vegetation, bati, routes, autres)
r <- raster(vegetation, res = 0.5, crs = '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

vegetation_rast <- fasterize(vegetation, r, field="COD_VEG_n")
occ_sol$code_2018=as.numeric(as.character(occ_sol$code_2018))
utisol_rast <- fasterize(st_transform(occ_sol,st_crs(r)), r, field="code_2018")
bati_rast <- fasterize(st_transform(bati,st_crs(r)), r, field = "code_departement_insee")

bati_rast[bati_rast == 1] <- 10
utisol_rast[utisol_rast == 12220] <- 11
utisol_rast[utisol_rast != 11] <- 0

vegetation_rast[is.na(vegetation_rast)] <- bati_rast[is.na(vegetation_rast)]
vegetation_rast[is.na(vegetation_rast)] <- utisol_rast[is.na(vegetation_rast)]

landcover_rast <- vegetation_rast
writeRaster(landcover_rast,"data/processed_data/landcover.tif")

# raster attribute table
landcover_data_dic <- st_drop_geometry(vegetation) %>% mutate_if(is.factor, as.character)
landcover_data_dic <- unique(landcover_data_dic[c("COD_VEG_n", "LIB_VEG_n")])
colnames(landcover_data_dic) <- c("class","label")
landcover_data_dic <- rbind(landcover_data_dic,c(0,"Autres"))
landcover_data_dic <- rbind(landcover_data_dic,c(10,"Batiments"))
landcover_data_dic <- rbind(landcover_data_dic,c(11,"Routes"))

write.csv(landcover_data_dic,"data/processed_data/landcover_data_dic.csv", row.names = F)

# même couche mais en regroupant certaines classes de végétation 
landcover_rast[landcover_rast %in% c(1,2,3,4,5)] <- 12
landcover_rast[landcover_rast %in% c(6,7,8,9)] <- 13

writeRaster(landcover_rast,"data/processed_data/landcover_grouped_veget.tif")

# raster attribute table
landcover_grouped_data_dic <- landcover_data_dic %>% filter(class %in% c(0,10,11))
landcover_grouped_data_dic <- rbind(landcover_grouped_data_dic,c(12,"Vegetation sup_3m"))
landcover_grouped_data_dic <- rbind(landcover_grouped_data_dic,c(13,"Vegetation inf_3m"))
write.csv(landcover_grouped_data_dic,"data/processed_data/landcover_grouped_veget_data_dic.csv", row.names = F)

## modèle numérique d'élévation

mnt <- terra::rast("data/processed_data/mnt.tif")
mns <- terra::rast("data/processed_data/mns.tif")

mnt2 <- terra::resample(mnt, mns, "near")

# mnt2 <- disagg(mnt, fact=c(50, 50))
# mns <- terra::crop(mns, mnt2)

mne <- mns-mnt2
mne[mne < 0] <- 0

terra::writeRaster(mne,"data/processed_data/mne.tif", overwrite = T)

# raster de la hauteur de la végétation uniquement
vegetation_rast <- terra::rast("data/processed_data/vegetation.tif")
vegetation_rast2 <- terra::resample(vegetation_rast, mne, "near")

vegetation_rast2[vegetation_rast2 == 0] <- NA

mne_veget <- terra::mask(mne,vegetation_rast2)

terra::writeRaster(mne_veget,"data/processed_data/mne_veget.tif", overwrite = T)


## données météo
# df_meteofrance <- list.files("data/METEO_SYNOP", full.names = T) %>%
#   purrr::map_dfr(.,~read.csv(., sep = ";", stringsAsFactors = F, na.strings = "mq")) %>%
#   filter(numer_sta == 07643) %>% # code station meteo montpellier
#   mutate(date = parse_date_time(date,"ymdHMS")) %>%
#   mutate(jour = as_date(date)) %>%
#   mutate(temp = t - 273.15) %>%
#   group_by(jour) %>%
#   summarise(precipitations = sum(rr3, na.rm = T), tmin = min(temp, na.rm = T), tmax = max(temp, na.rm = T), tmean = mean(temp, na.rm = T))

# write.csv(df_meteofrance,"data/processed_data/meteo_macro_meteofrance.csv",row.names = F)

# https://odee.herault.fr/index.php/component/phocadownload/category/36-climatologie?download=5054:donnees-climato-dept34
  df_meteo_dpt <- read.csv('/home/ptaconet/modeling_vector_mtp/data/Donnees_Climato_Dept34/Donnees/Station_202_20210526_H.csv', sep = ";",stringsAsFactors = F, na.strings = "", dec = ",", col.names = c('date',"heure","precipitations","temp")) %>%
  mutate(date = parse_date_time(date,"%d/%m/%Y")) %>%
  group_by(date) %>%
  summarise(precipitations = sum(precipitations, na.rm = T), tmin = min(temp, na.rm = T), tmax = max(temp, na.rm = T), tmean = mean(temp, na.rm = T))

write.csv(df_meteo_dpt,"data/processed_data/meteo_macro.csv",row.names = F)


## Données mîcroclimatiques

df_metadata <- read.csv("data/donnees_microclim/BG/donnees_pose_collecte_dl.csv", sep = ";", quote = "", stringsAsFactors = F)
df_metadata$NOM_DOSSIER <- gsub("\"","",df_metadata$NOM_DOSSIER)
df_metadata$lien_fichier <- paste0("data/donnees_microclim/BG/",df_metadata$NOM_DOSSIER,"/",df_metadata$NOM_FICHIER,".ods")
df_metadata <- df_metadata %>% filter(!is.na(NOM_FICHIER))
df_metadata$DATE_POSE <- as.Date(paste0(substr(df_metadata$DATE_POSE,7,10),"-",substr(df_metadata$DATE_POSE,4,5),"-",substr(df_metadata$DATE_POSE,1,2)))
df_metadata$DATE_HEURE_POSE <- ymd_hms(paste(df_metadata$DATE_POSE,df_metadata$HEURE_POSE,":00"))
df_metadata <- df_metadata %>% relocate(DATE_HEURE_POSE, .after = HEURE_POSE)
df_metadata$DATE_RETRAIT <- as.Date(paste0(substr(df_metadata$DATE_RETRAIT,7,10),"-",substr(df_metadata$DATE_RETRAIT,4,5),"-",substr(df_metadata$DATE_RETRAIT,1,2)))
df_metadata$DATE_HEURE_RETRAIT <- ymd_hms(paste(df_metadata$DATE_RETRAIT,df_metadata$HEURE_RETRAIT,":00"))
df_metadata <- df_metadata %>% relocate(DATE_HEURE_RETRAIT, .after = HEURE_RETRAIT)


fun_format_df_microclim <- function(path_to_df_microclim){
  
  df_microclim_format <- readODS::read_ods(path_to_df_microclim, skip = 3)
  df_microclim_format <- df_microclim_format[,1:5]
  colnames(df_microclim_format) <- c("date","heure","temperature","humidite","pointderosee")
  df_microclim_format$date <- as.Date(paste0(substr(df_microclim_format$date,7,10),"-",substr(df_microclim_format$date,4,5),"-",substr(df_microclim_format$date,1,2)))
  df_microclim_format$date_heure <- ymd_hms(paste(df_microclim_format$date,df_microclim_format$heure))
  df_microclim_format <- df_microclim_format %>% relocate(date_heure, .after = heure)
  
  return(df_microclim_format)
}

df_microclim <- NULL

for(i in 1:nrow(df_metadata)){
  
  th_df_microclim <- fun_format_df_microclim(df_metadata$lien_fichier[i])
  th_df_microclim$ID_PIEGE <- df_metadata$ID_BG[i]
  th_df_microclim$ID_COLLECTE_1 <- df_metadata$ID_COLLECTE_1[i]
  th_df_microclim$ID_COLLECTE_2 <- df_metadata$ID_COLLECTE_2[i]
  th_df_microclim$ID_COLLECTE_3 <- df_metadata$ID_COLLECTE_3[i]
  th_df_microclim$ID_COLLECTE_4 <- df_metadata$ID_COLLECTE_4[i]
  
  df_microclim <- rbind(df_microclim,th_df_microclim)
  
  
}

write.csv(df_microclim,"data/processed_data/microclim.csv", row.names = F)

