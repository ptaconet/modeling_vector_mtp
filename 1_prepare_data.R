########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(sf) ## Version ‘1.0.16’
library(raster) ## Version ‘3.6.26’
library(rgdal) ## Version ‘1.6.7’
library(fasterize) ## Version ‘1.0.5’
library(lubridate) ## Version ‘1.9.3’
library(readODS) ## Version ‘2.3.0’


########################### Spatial data: cleaning, splitting according to area of interest and transformation
#########'The objectives are:  (i) to create a new land cover layer of Montpellier with different open access data bases, which indicate the presence of buildings, of low and high evegtatuion and of roads
#########'(ii) to adapt and to crop the other spatial data to the projection and to the bounding box around the traps.
########################### 

##### Opening trap location
pieges <- st_read("02_Data/raw_data/01_Adults_Abundance_Longevity/P02_TRAPS_LOCATION.gpkg")

#### Creation of the boundinx box around the traps
roi <- sf::st_as_sf(pieges, coords = c("longitude", "latitude"), crs = 4326) %>%
  sf::st_bbox()

roi[1]<-roi[1]-0.01 #xmin
roi[2]<-roi[2]-0.01 #ymin
roi[3]<-roi[3]+0.01 #xmin
roi[4]<-roi[4]+0.01 #ymin

roi <- roi %>% ## conversion of bbox roi to sf dataframe
  sf::st_as_sfc() %>%
  sf::st_sf() %>%
  st_transform(32631) ## good projection 

##### Opening environmental data

## Opening the layer of Vegetation from the Open Data of Montpellier: https://data.montpellier3m.fr/story/cartographie-interactive-de-la-vegetation-fine-de-montpellier-mediterranee-metropole
vegetation <-  st_read("02_Data/raw_data/07_Environmental_Data/07_Montpellier_Vegetation/Montpellier_Vegetation_Fine.shp")   
## Opening the Urban Atlas de Montpellier of 2018 of Copernicus
occ_sol <-  st_read("02_Data/raw_data/07_Environmental_Data/07_Montpellier_Urban_Atlas/07_Montpellier_Urban_Atlas_Data/07_Montpellier_Urban_Atlas.gpkg") 
## Opening the National Data Base of the Building (BDNB): https://www.data.gouv.fr/fr/datasets/base-de-donnees-nationale-des-batiments/
bati <- st_read("02_Data/raw_data/07_Environmental_Data/07_Montpellier_BDNB/07_Montpellier_BDNB_Data/07_Montpellier_BDNB_Data.gpkg",layer = "batiment_construction") 


#### Opening socio-demographic data

## Opening the layer of population density of Montpellier: https://data.montpellier3m.fr/dataset/distribution-fine-de-la-population
pop <-  st_read("02_Data/raw_data/08_Demographic_Economic_Data/08_Montpellier_Population_Density/08_Montpellier_Population_Density_Data.shp") ## Donnes de densite de population
## Opening the layer of economical data from the National Insitute of Economics and Statistics (INSEE): https://www.insee.fr/fr/statistiques/4176290?sommaire=4176305
filosofi <-  st_read("02_Data/raw_data/08_Demographic_Economic_Data/08_Montpellier_Socio_Economics/08_Montpellier_Socio_Economics_Data.gpkg")

  
#### Division by area of interest (bounding box) 

## For the vegetation layer
vegetation <- vegetation %>% st_make_valid() %>% st_crop(st_transform(roi,st_crs(vegetation))) %>% st_cast("POLYGON")
## For Urban Atlas  
occ_sol <- occ_sol %>% st_crop(st_transform(roi,st_crs(occ_sol))) %>% st_cast("MULTIPOLYGON")
## For population density 
pop <- pop %>% st_crop(st_transform(roi,st_crs(pop)))
## For economics data
filosofi <- filosofi %>% st_crop(st_transform(roi,st_crs(filosofi))) %>% st_cast("MULTIPOLYGON") 
## For the buildings
bati <- bati %>% st_crop(st_transform(roi,st_crs(bati))) %>% st_cast("POLYGON")

#### Registering new spatial data
## Population density layer
write_sf(pop,"02_Data/processed_data/pop.gpkg",overwrite=TRUE)
## Socio economics layer
write_sf(filosofi,"02_Data/processed_data/filosofi.gpkg",overwrite=TRUE)
## Builing layer
bati$code_departement_insee<-as.numeric(bati$code_departement_insee)
write_sf(bati,"02_Data/processed_data/bati.gpkg", overwrite=T)

#### Conversion of vector data to raster for landscape metrics, creation of associated attribute tables and registration

## For the vegetation layer
r <- raster(vegetation, res = 0.5, crs = '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs') ## conversion to a raster
vegetation_rast <- fasterize(vegetation, r, field="COD_VEG_n", background = 0)  ## creates a raster with 0 for empty cells (background)
writeRaster(vegetation_rast,"02_Data/processed_data/03_Landcover_Data/vegetation.tif", overwrite=TRUE)
## Vegetation attribute table
vegetation_data_dic <- st_drop_geometry(vegetation) %>% mutate_if(is.factor, as.character)
vegetation_data_dic <- unique(vegetation_data_dic[c("COD_VEG_n", "LIB_VEG_n")])
colnames(vegetation_data_dic) <- c("class","label")
vegetation_data_dic <- rbind(vegetation_data_dic,c(0,"Surface non végétalisée"))
write.csv(vegetation_data_dic,"02_Data/processed_data/03_Landcover_Data/vegetation_data_dic.csv", row.names = F)


## For Urban Atlas
r <- raster(occ_sol, res = 0.5, crs = '+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs') ## conversion to a raster
occ_sol$code_2018=as.numeric(as.character(occ_sol$code_2018))
occ_sol_rast <- fasterize(occ_sol, r, field="code_2018")
writeRaster(occ_sol_rast,"02_Data/processed_data/03_Landcover_Data/landuse.tif", overwrite=TRUE)
## Land cover Urban Atlas attribute table
occ_sol_data_dic <- st_drop_geometry(occ_sol) %>% mutate_if(is.factor, as.character)
occ_sol_data_dic <- unique(occ_sol_data_dic[c("code_2018", "class_2018")])
colnames(occ_sol_data_dic) <- c("class","label")
write.csv(occ_sol_data_dic,"02_Data/processed_data/03_Landcover_Data/landuse_data_dic.csv",row.names = F)

#### Creation of a new land cover layer with vegetation, buildings and roads with all vegetation values

## Transformation of Urban Atlas Raster, Vegetation and buildings
r <- raster(vegetation, res = 0.5, crs = '+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
vegetation_rast <- fasterize(vegetation, r, field="COD_VEG_n")
occ_sol$code_2018=as.numeric(as.character(occ_sol$code_2018))
utisol_rast <- fasterize(st_transform(occ_sol,st_crs(r)), r, field="code_2018")
bati_rast <- fasterize(st_transform(bati,st_crs(r)), r, field = "code_departement_insee")
## Modification of raster values
bati_rast[bati_rast == 34] <- 10 ## Selection of buildings: number of pixels where there are buildings = 34 from the BDNB
utisol_rast[utisol_rast == 12220] <- 11 ## Selection of roads from Urban Atlas 
utisol_rast[utisol_rast != 11] <- 0 ## Mets 0 si non route
vegetation_rast[is.na(vegetation_rast)] <- bati_rast[is.na(vegetation_rast)] ## In the case of the vegetation layer, installation of buildings at their location if there is no vegetation. 
vegetation_rast[is.na(vegetation_rast)] <- utisol_rast[is.na(vegetation_rast)] ## In the case of the vegetation layer, installation of roads at their location if there is no vegetation and roads.
landcover_rast <- vegetation_rast
## New Raster attribute table
landcover_data_dic <- st_drop_geometry(vegetation) %>% mutate_if(is.factor, as.character)
landcover_data_dic <- unique(landcover_data_dic[c("COD_VEG_n", "LIB_VEG_n")]) ## Different values for different vegetation categories
colnames(landcover_data_dic) <- c("class","label")
landcover_data_dic <- rbind(landcover_data_dic,c(0,"Autres")) ## Others categories : 0
landcover_data_dic <- rbind(landcover_data_dic,c(10,"Batiments")) ## Categorie building : 10
landcover_data_dic <- rbind(landcover_data_dic,c(11,"Routes")) ## Categorie roads : 11
write.csv(landcover_data_dic,"02_Data/processed_data/landcover_data_dic.csv", row.names = F)

#### Creation of a similar layer but grouping vegetation values into two layers: high and low
## Grouping of vegetation data 
landcover_rast[landcover_rast %in% c(1,2,3,4,5)] <- 12 ## For high vegetation (more than 3 meters) : 12
landcover_rast[landcover_rast %in% c(6,7,8,9)] <- 13 ## For low vegetation (less than 3 meters) : 13
writeRaster(landcover_rast,"02_Data/processed_data/03_Landcover_Data/landcover_grouped_veget.tif", overwrite=T)
## Raster attribute table
landcover_grouped_data_dic <- landcover_data_dic %>% filter(class %in% c(0,10,11))
landcover_grouped_data_dic <- rbind(landcover_grouped_data_dic,c(12,"Vegetation sup_3m"))
landcover_grouped_data_dic <- rbind(landcover_grouped_data_dic,c(13,"Vegetation inf_3m"))
write.csv(landcover_grouped_data_dic,"02_Data/processed_data/l03_Landcover_Data/andcover_grouped_veget_data_dic.csv", row.names = F)


########################### Meteorological data: cleaning, selection of study period and transformation
#########'The objective is to collect different meteorologicla variables from open access databases
###########################

#### Meteorological meteo SYNOP from Meteo France: https://donneespubliques.meteofrance.fr/
df_meteofrance <- list.files("02_Data/raw_data/09_Climatic_Data/09_Montpellier_SYNOP/09_Montpellier_SYNOP_Data/09_SYNOP_Data", full.names = T) %>%
  purrr::map_dfr(.,~read.csv(., sep = ";", stringsAsFactors = F, na.strings = "mq")) %>%
  filter(numer_sta == 07643) %>% # selection of the Montpellier station, in Fréjorques
  mutate(date = parse_date_time(date,"ymdHMS")) %>%
  mutate(jour = as_date(date)) %>%
  mutate(temp = t - 273.15) %>%
  group_by(jour) %>%
  summarise(precipitations = sum(rr3, na.rm = T), tmin = min(temp, na.rm = T), tmax = max(temp, na.rm = T), tmean = mean(temp, na.rm = T), tamp = max(temp, na.rm = T) - min(temp, na.rm = T), rh = mean(u, na.rm = T), wind = mean(ff, na.rm = T))  %>% ## selection of daily rainfall, minimal temperature, mean temperaure, maximal temperayure, the difference of temperature amplitude, teh relative humidity and teh wind speed
  mutate(precipitations = ifelse(precipitations<0,0,precipitations)) %>%
  rename(date=jour)
write.csv(df_meteofrance,"02_Data/processed_data/09_Climatic_Data/meteo_macro_synop.csv",row.names = F)


#### Amotspheric pressure data from : Meteo France: https://donneespubliques.meteofrance.fr/
df_meteofrance_pression <- list.files("02_Data/raw_data/09_Climatic_Data/09_Montpellier_SYNOP/09_Montpellier_SYNOP_Data/09_SYNOP_Data", full.names = T) %>%
  purrr::map_dfr(.,~read.csv(., sep = ";", stringsAsFactors = F, na.strings = "mq")) %>%
  filter(numer_sta == 07643) %>% # code station meteo montpellier
  mutate(date = parse_date_time(date,"ymdHMS")) %>%
  mutate(jour = as_date(date)) %>%
  mutate(temp = t - 273.15) %>%
  group_by(date) %>%
  summarise(patm=pres, pmer=pmer)  

write.csv(df_meteofrance_pression,"02_Data/processed_data/meteo_pression_synop.csv",row.names = F)



#### Departmental meteorological data from ODEE: https://odee.herault.fr/index.php/component/phocadownload/category/36-climatologie?download=5054:donnees-climato-dept34

df_meteo_dpt <- read.csv('02_Data/raw_data/09_Climatic_Data/09_Montpellier_ODEE/09_Montpellier_ODEE_Data/Station_202_20210526_H.csv', sep = ";",stringsAsFactors = F, na.strings = "", dec = ",", col.names = c('date',"heure","precipitations","temp")) %>% ## selection of station of Chateau d'o in Montpellier
  mutate(date = parse_date_time(date,"%d/%m/%Y")) %>%
  group_by(date) %>%
  summarise(precipitations = sum(precipitations, na.rm = T), tmin = min(temp, na.rm = T), tmax = max(temp, na.rm = T), tmean = mean(temp, na.rm = T), tamp = max(temp, na.rm = T) - min(temp, na.rm = T)) ## Selection of hourly rainfall, minimum, maximum and mean temperature 
write.csv(df_meteo_dpt,"02_Data/processed_data/09_Climatic_Data//meteo_macro_dpt.csv",row.names = F)


########################### Micro climatic data 
#########'The objectives (i) to correctly open the micro climatic data 
#########'(ii) to select the hour and the date of interests .
###########################

#### Opening the metadata of the file which allows to correctly open the micro climatic files of every data logger
df_metadata <- read.csv("02_Data/raw_data/06_Dataloggers_Microclimate/P02_DATALOGGERS-MICROCLIMATE_FIELD_DATA.csv", sep = ";", quote = "", stringsAsFactors = F)
df_metadata$NOM_DOSSIER <- gsub("\"","",df_metadata$NOM_DOSSIER)
df_metadata$lien_fichier <- paste0("02_Data/raw_data/06_Dataloggers_Microclimate/",df_metadata$NOM_DOSSIER,"/",df_metadata$NOM_FICHIER,".ods")
df_metadata <- df_metadata %>% filter(!is.na(NOM_FICHIER))
df_metadata$DATE_POSE <- as.Date(paste0(substr(df_metadata$DATE_POSE,7,10),"-",substr(df_metadata$DATE_POSE,4,5),"-",substr(df_metadata$DATE_POSE,1,2)))
df_metadata$DATE_HEURE_POSE <- ymd_hms(paste(df_metadata$DATE_POSE,df_metadata$HEURE_POSE,":00"))
df_metadata <- df_metadata %>% relocate(DATE_HEURE_POSE, .after = HEURE_POSE)
df_metadata$DATE_RETRAIT <- as.Date(paste0(substr(df_metadata$DATE_RETRAIT,7,10),"-",substr(df_metadata$DATE_RETRAIT,4,5),"-",substr(df_metadata$DATE_RETRAIT,1,2)))
df_metadata$DATE_HEURE_RETRAIT <- ymd_hms(paste(df_metadata$DATE_RETRAIT,df_metadata$HEURE_RETRAIT,":00"))
df_metadata <- df_metadata %>% relocate(DATE_HEURE_RETRAIT, .after = HEURE_RETRAIT)

#### Formatting microclimatic data files
fun_format_df_microclim <- function(path_to_df_microclim){ ## function which allows to open correctly the microclimatic data files with the path found in the meta data file
  
  df_microclim_format <- readODS::read_ods(path_to_df_microclim, skip = 3) ## openning the files in ODS format
  df_microclim_format <- df_microclim_format[,1:5]
  colnames(df_microclim_format) <- c("date","heure","temperature","humidite","pointderosee") ## selection of teh date, hour, temperature, humidity and point de rose
  df_microclim_format$date <- as.Date(paste0(substr(df_microclim_format$date,7,10),"-",substr(df_microclim_format$date,4,5),"-",substr(df_microclim_format$date,1,2))) ## good format of date 
  df_microclim_format$date_heure <- ymd_hms(paste(df_microclim_format$date,df_microclim_format$heure))
  df_microclim_format <- df_microclim_format %>% relocate(date_heure, .after = heure) ## good hours
  
  return(df_microclim_format)
}

df_microclim <- NULL

for(i in 1:nrow(df_metadata)){ ## opening the files
  
  th_df_microclim <- fun_format_df_microclim(df_metadata$lien_fichier[i])
  th_df_microclim$ID_PIEGE <- df_metadata$ID_BG[i]
  th_df_microclim$ID_COLLECTE_1 <- df_metadata$ID_COLLECTE_1[i]
  th_df_microclim$ID_COLLECTE_2 <- df_metadata$ID_COLLECTE_2[i]
  th_df_microclim$ID_COLLECTE_3 <- df_metadata$ID_COLLECTE_3[i]
  th_df_microclim$ID_COLLECTE_4 <- df_metadata$ID_COLLECTE_4[i]
  
  df_microclim <- rbind(df_microclim,th_df_microclim)
  
  
}

df_microclim$humidite<-dplyr::case_when(df_microclim$humidite>100~100,df_microclim$humidite<=100~df_microclim$humidite) ## cleaning the data if the relative humidity is more than 100%
write.csv(df_microclim,"02_Data/processed_data/09_Climatic_Data/microclim.csv", row.names = F)





