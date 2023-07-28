library(tidyverse)
library(sf)
library(raster)
library(rgdal)
library(fasterize)
library(rgdal)


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


mnt <- list.files("data/RGEALTI_2-0_5M_ASC_LAMB93-IGN69_D034_2022-12-16/RGEALTI/1_DONNEES_LIVRAISON_2023-01-00223/RGEALTI_MNT_5M_ASC_LAMB93_IGN69_D034", full.names = T) %>% lapply(raster)
mnt_to_keep <- NULL
for(i in 1:length(mnt)){
  crs(mnt[[i]]) <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 
  if(!is.null(intersect(extent(mnt[[i]]),extent(st_transform(roi,crs(mnt[[1]])))))){
    mnt_to_keep <- c(mnt_to_keep,mnt[[i]]@file@name)
  }
}
gdalUtils::gdalwarp(srcfile=mnt_to_keep,dstfile="data/processed_data/mnt.tif", t_srs = "EPSG:2154", overwrite = T)


occ_sol <- occ_sol %>% st_crop(st_transform(roi,st_crs(occ_sol))) %>% st_cast("MULTIPOLYGON")
pop <- pop %>% st_crop(st_transform(roi,st_crs(pop)))
filosofi <- filosofi %>% st_crop(st_transform(roi,st_crs(filosofi))) %>% st_cast("MULTIPOLYGON")
impermeabilite <- impermeabilite %>% crop(st_transform(roi,crs(impermeabilite))) 
temp_diurne <- temp_diurne %>% crop(st_transform(roi,crs(temp_diurne)))
temp_nocturne <- temp_nocturne %>% crop(st_transform(roi,crs(temp_nocturne)))
bati <- bati %>% st_crop(st_transform(roi,st_crs(bati)))

writeRaster(impermeabilite,"data/processed_data/impermeabilite.tif")

write_sf(pop,"data/processed_data/pop.gpkg")
write_sf(filosofi,"data/processed_data/filosofi.gpkg")
write_sf(bati,"data/processed_data/bati.gpkg")
write_sf(lcz,"data/processed_data/lcz.gpkg")

## conversion des données vectorielles en raster pour calcul métrique paysagères
# vegetation
r <- raster(vegetation, res = 1, crs = '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
vegetation_rast <- fasterize(vegetation, r, field="COD_VEG_n", background = 0)
vegetation_rast <- crop(vegetation_rast, st_transform(roi,crs(vegetation_rast)))

writeRaster(vegetation_rast,"data/processed_data/vegetation.tif")

# occupation du sol
r <- raster(occ_sol, res = 1, crs = '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
occ_sol$code_2018=as.numeric(as.character(occ_sol$code_2018))
occ_sol_rast <- fasterize(occ_sol, r, field="code_2018")

writeRaster(occ_sol_rast,"data/processed_data/occ_sol.tif")

# modèle numérique d'élévation

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
