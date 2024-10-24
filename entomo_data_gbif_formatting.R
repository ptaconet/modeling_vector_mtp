library(tidyverse)
library(sf)

pieges_lieu <- st_read("localisation_piege_provisoire.gpkg")  %>%
  st_drop_geometry() %>%
  dplyr::select(ID_PIEGE,lieu)

pieges <- st_read("P02_TRAPS_LOCATION.gpkg") %>% 
  filter(TYPE_PIEGE == 'bg-sentinel') %>%
  mutate(LATITUDE=as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)) %>%
  left_join(pieges_lieu) %>%
  st_drop_geometry() %>%
  mutate(lieu = ifelse(ID_PIEGE %in% c("BG_12","BG_13"),"Alco",lieu)) %>%
  mutate(lieu = ifelse(ID_PIEGE %in% c("BG_15","BG_16"),"Hotel de Ville",lieu)) %>%
  dplyr::select(-ZONE)

df_releves_pieges_raw <- read.csv("P02_BG-ADULTS_LABO_DATA.csv", stringsAsFactors = F, sep = ";") %>%
  mutate(HEURE_COLLECTE = ifelse(is.na(HEURE_COLLECTE),"10:00",HEURE_COLLECTE)) %>%
  rename(ID_COLLECTE_STRING = ID_COLLECTE) %>%
  mutate(DATE_POSE = parse_date(DATE_POSE,"%d/%m/%Y")) %>%
  mutate(DATE_COLLECTE = parse_date(DATE_COLLECTE,"%d/%m/%Y")) %>%
  mutate(across(everything(), ~ gsub("\\s*\\([^\\)]+\\)", "", .))) %>%
  left_join(pieges, by = "ID_PIEGE") %>%
  group_by(ID_PIEGE,DATE_POSE) %>%
  mutate(row_number = row_number()) %>%
  ungroup() %>%
  mutate(DATE_POSE = ifelse(row_number==2,as.character(as.Date(DATE_POSE)+1),as.character(as.Date(DATE_POSE)))) %>%
  rename(NB_ALBO_F_PP = NB_ALBO_PP, NB_ALBO_F_GRAVIDE = NB_ALBO_GRAVIDE, NB_ALBO_F_P_ND= NB_ALBO_P_ND, NB_ALBO_F_PT_PP=NB_ALBO_PT_PP ,NB_ALBO_F_PT_NP=NB_ALBO_PT_NP) %>%
  arrange(DATE_COLLECTE)

df_releves_pieges_raw$consecutive <- c(NA,diff(as.Date(df_releves_pieges_raw$DATE_COLLECTE)) %in% c(0,1))
df_releves_pieges_raw$consecutive[1] = FALSE
df_releves_pieges_raw$num_session = cumsum(df_releves_pieges_raw$consecutive==FALSE)
df_releves_pieges_raw$consecutive=NULL

# final column names : eventID	samplingProtocol	samplingEffort	sampleSizeValue	sampleSizeUnit	eventDate	LocationID	decimalLatitude	decimalLongitude	geodeticDatum	country	countryCode	locality stateProvince	coordinatePrecision	institutionCode

events <- df_releves_pieges_raw %>%
  rename(eventID = ID_COLLECTE_STRING, LocationID = ID_PIEGE) %>%
  mutate(eventDate = paste0(DATE_POSE,"T",HEURE_COLLECTE,"+0200/",DATE_COLLECTE,"T",HEURE_COLLECTE,"+0200")) %>%  
  mutate(LATITUDE = round(LATITUDE,3), LONGITUDE = round(LONGITUDE, 3)) %>%
  rename(decimalLatitude = LATITUDE, decimalLongitude = LONGITUDE,  verbatimLocality  = lieu, fieldNotes = REMARQUE) %>%
  mutate(year = lubridate::year(DATE_COLLECTE), 
         month = lubridate::month(DATE_COLLECTE),
         startDayOfYear = lubridate::yday(DATE_POSE),
         endDayOfYear = lubridate::yday(DATE_COLLECTE),
         samplingProtocol = "Biogents (c) BG Pro mosquito trap, https://eu.biogents.com/bg-pro/" ,
         samplingEffort = "24 hours of trapping", 
         sampleSizeValue = "24", 
         sampleSizeUnit = "hours", 
         geodeticDatum = "WGS84", 
         continent = "Europe",
         country = "France", 
         countryCode = "FR", 
         municipality = "Montpellier",
         stateProvince = "Occitanie",
         county = "Herault",
         coordinateUncertaintyInMeters = "100", 
         institutionCode = "Université de Montpellier | Institut de Recherche pour le Développement",
         habitat = case_when( ZONE =="parc" ~ "urban park",
                              ZONE == "pavillon" ~ "residential",
                              ZONE == "urbain" ~ "urban"),
         verbatimEventDate = paste0("trapping session n°",num_session)) %>%
  dplyr::select(eventID,	continent, country,	countryCode,	municipality, verbatimLocality, stateProvince	,county,	LocationID,decimalLatitude,	decimalLongitude,	geodeticDatum,coordinateUncertaintyInMeters, 	habitat, verbatimEventDate,eventDate,year, month, startDayOfYear, endDayOfYear,institutionCode, 	samplingProtocol,	samplingEffort,	sampleSizeValue,	sampleSizeUnit, fieldNotes )

# https://dwc.tdwg.org/terms/#occurrence
occurrence <- df_releves_pieges_raw %>%
  rename(eventID = ID_COLLECTE_STRING) %>%
  dplyr::select(eventID,NB_ALBO_TOT:NB_MOUS_IND_F) %>%
  pivot_longer(!eventID,  names_to = "mosq", values_to = "count") %>%
  dplyr::filter(mosq %in% c("NB_ALBO_M",
                              "NB_ALBO_F",
                              "NB_CUPIP_M",
                              "NB_CUPIP_F",
                              "NB_CUHOR_M",
                              "NB_CUHOR_F",
                              "NB_CULAN_M",
                              "NB_CULAN_F",
                              "NB_CULLA_M",
                              "NB_CULLA_F")) %>%
  mutate(mosq = gsub("NB_","",mosq)) %>%
  mutate(basisOfRecord = "HumanObservation",
         recordedBy = "Colombine Bartholomée | Coralie Grail | Mathilde Mercat",
         recordedByID = "https://orcid.org/0000-0001-7291-5195",
         scientificName = case_when(grepl("ALBO",mosq) ~ "Aedes albopictus (Skuse, 1895)",
                                    grepl("CUPIP",mosq) ~ "Culex pipiens Linnaeus, 1758",
                                    grepl("CUHOR",mosq) ~ "Culex hortensis Ficalbi, 1889",
                                    grepl("CULAN",mosq) ~ "Culiseta annulata (Schrank, 1776)",
                                    grepl("CULLA",mosq) ~ "Culiseta longiareolata (Macquart, 1838)",
                                    TRUE ~ NA),
         genericName = case_when(grepl("ALBO",mosq) ~ "Aedes albopictus",
                                 grepl("CUPIP",mosq) ~ "Culex pipiens",
                                 grepl("CUHOR",mosq) ~ "Culex hortensis",
                                 grepl("CULAN",mosq) ~ "Culiseta annulata",
                                 grepl("CULLA",mosq) ~ "Culiseta longiareolata",
                                 TRUE ~ NA),
         kingdom = "Animalia",
         phylum	= "Arthropoda",
         class = "Insecta",
         order = "Diptera",
         family = "Culicidae",
         genus = case_when(grepl("ALBO",mosq) ~ "Aedes",
                           grepl("CUPIP",mosq) ~ "Culex",
                           grepl("CUHOR",mosq) ~ "Culex",
                           grepl("CULAN",mosq) ~ "Culiseta",
                           grepl("CULLA",mosq) ~ "Culiseta",
                           TRUE ~ NA),
         lifeStage = "adult",
         organismQuantityType = "individuals", 
         individualCount = as.numeric(count),
         organismQuantity = individualCount,
         taxonRank = "species",
         nameAccordingTo = "Integrated Taxonomic Information System, https://www.itis.gov/, accessed on 21 october 2024",
         identifiedBy = "",  # a compléter
         identificationReferences = "http://www.medilabsecure.com/entomology-tools-0/moskeytool",
         identificationRemarks  = "morphological identification using the Moskeytool software",
         sex = case_when(grepl("_M",mosq) ~ "male",
                         grepl("_F",mosq) ~ "female",
                         grepl("SEX_ND",mosq) ~ NA,
                         TRUE ~ NA)
  ) %>%
  filter(!is.na(individualCount)) %>%
  mutate(occurrenceID = paste0(eventID,"_",mosq)) %>%
  dplyr::select(-mosq) %>%
  dplyr::select(-count) %>%
  relocate(occurrenceID,1)
  

### create 'extended_measurement_fact' table to store attributes for mosquito data (see https://rs.gbif.org/extension/obis/extended_measurement_or_fact.xml)
ExtendedMeasurementOrFact <- df_releves_pieges_raw %>%
  rename(eventID = ID_COLLECTE_STRING) %>%
  dplyr::select(eventID,NB_ALBO_TOT:NB_MOUS_IND_F) %>%
  pivot_longer(!eventID,  names_to = "mosq", values_to = "count") %>%
  dplyr::filter(mosq %in% c("NB_ALBO_F_G",
                            "NB_ALBO_F_NG",
                            #"NB_ALBO_F_DIS",
                            "NB_ALBO_F_NP",
                            "NB_ALBO_F_PP",
                            "NB_ALBO_F_GRAVIDE",
                            #"NB_ALBO_F_P_ND",
                            #"NB_ALBO_F_PT_PP",
                            #"NB_ALBO_F_PT_NP",
                            "NB_CUPIP_F_G",
                            "NB_CUPIP_F_NG",
                            "NB_CUHOR_F_G",
                            "NB_CUHOR_F_NG",
                            "NB_CULAN_F_G",
                            "NB_CULAN_F_NG",
                            "NB_CULLA_F_G",
                            "NB_CULLA_F_NG")) %>%
  mutate(mosq=gsub("NB_","",mosq)) %>%
  mutate(mosq2=gsub("\\_F.*","",mosq)) %>%
  mutate(mosq2=paste0(mosq2,"_F")) %>%
  mutate(occurrenceID = paste0(eventID,"_",mosq2)) %>%
  dplyr::select(-mosq2) %>%
  left_join(occurrence) %>%
  dplyr::filter(individualCount>0) %>%
  mutate(measurementID = paste0(eventID,"_",mosq)) %>%
  rename(measurementValue = count) %>%
  mutate(measurementValue = gsub(" semi-gravide)","",measurementValue)) %>%
  mutate(measurementValue=as.numeric(measurementValue)) %>%
  dplyr::filter(!is.na(measurementValue)) %>%
  mutate(measurementType = case_when(grepl("_G",mosq) ~ "gorged or semi-gorged", # with human blood ?
                                     grepl("_NG",mosq) ~ "not gorged",
                                     grepl("_NP",mosq) ~ "nulliparous",
                                     grepl("_PP",mosq) ~ "parous",
                                     grepl("_GRAVIDE",mosq) ~ "gravid")) %>%
  mutate(measurementUnit = "Number of collected female individuals", 
         measurementMethod = "",  # à compléter
         measurementDeterminedBy = "") %>% # à compléter
  dplyr::select(measurementID,eventID,occurrenceID,genericName,measurementType,measurementValue,measurementUnit,measurementDeterminedBy,measurementMethod)
  
  
# MeasurementOrFact = données environnementales  (lié à l'event)
## occ sol buffer 50m et 100m + micro climat (pendant la collecte + 24h + 48h) + meteo (T°, vent, precipitations le mois precedent)
MeasurementOrFact <- read.csv("MeasurementOrFact_raw.csv") %>%
  dplyr::select(ID_COLLECTE_STRING,contains("lsm_c_pland_LCG_50"),contains("lsm_c_pland_LCG_100"),contains("_collection"),contains("24h"),contains("48h"),"RFDode_0_4","TMINode_0_4","TMAXode_0_4","WINDmf_0_4","RHmf_0_4") %>%
  rename(eventID = ID_COLLECTE_STRING) %>%
  pivot_longer(!eventID,  names_to = "Type", values_to = "measurementValue") %>%
  filter(!(Type %in% c("lsm_c_pland_LCG_50_NaN","lsm_c_pland_LCG_100_NaN"))) %>%
  mutate(measurementType = case_when(Type=="lsm_c_pland_LCG_50_0" ~ "% of landscape occupied by the class '' within a 50 m radius buffer zone around the trap",
                                     Type=="lsm_c_pland_LCG_50_10" ~ "% of landscape occupied by the class '' within a 50 m radius buffer zone around the trap",
                                     Type=="lsm_c_pland_LCG_50_11" ~ "% of landscape occupied by the class '' within a 50 m radius buffer zone around the trap",
                                     Type=="lsm_c_pland_LCG_50_12" ~ "% of landscape occupied by the class '' within a 50 m radius buffer zone around the trap",
                                     Type=="lsm_c_pland_LCG_50_13" ~ "% of landscape occupied by the class '' within a 50 m radius buffer zone around the trap",
                                     Type=="lsm_c_pland_LCG_100_0" ~ "% of landscape occupied by the class '' within a 100 m radius buffer zone around the trap",
                                     Type=="lsm_c_pland_LCG_100_10" ~ "% of landscape occupied by the class '' within a 100 m radius buffer zone around the trap",
                                     Type=="lsm_c_pland_LCG_100_11" ~ "% of landscape occupied by the class '' within a 100 m radius buffer zone around the trap",
                                     Type=="lsm_c_pland_LCG_100_12" ~ "% of landscape occupied by the class '' within a 100 m radius buffer zone around the trap",
                                     Type=="lsm_c_pland_LCG_100_13" ~ "% of landscape occupied by the class '' within a 100 m radius buffer zone around the trap",
                                     Type=="RFSUM_collection" ~ "Cumulated rainfall during the sampling event",
                                     Type=="RFMIN_collection" ~ "Hourly minimum rainfall during the sampling event",
                                     Type=="RFMAX_collection" ~ "Hourly maximum rainfall during the sampling event",
                                     Type=="TMEAN_collection" ~ "Average temperature during the sampling event",
                                     Type=="TMIN_collection" ~ "Hourly minimum temperature during the sampling event",
                                     Type=="TMAX_collection" ~ "Hourly maximum temperature during the sampling event",
                                     Type=="RHMEAN_collection" ~ "Average relative humidity during the sampling event",
                                     Type=="RHMIN_collection" ~ "Hourly minimum relative humidity during the sampling event",
                                     Type=="RHMAX_collection" ~ "Hourly maximum relative humidity during the sampling event",
                                     Type=="RFSUM_24hprec" ~ "Cumulated rainfall over the 24 hours preceding the sampling event",
                                     Type=="RFMIN_24hprec" ~ "Hourly minimum rainfall over the 24 hours preceding the sampling event",
                                     Type=="RFMAX_24hprec" ~ "Hourly maximum rainfall over the 24 hours preceding the sampling event",
                                     Type=="TMEAN_24h_prec" ~ "Average temperature over the 24 hours preceding the sampling event",
                                     Type=="TMIN_24h_prec" ~ "Hourly minimum temperature over the 24 hours preceding the sampling event",
                                     Type=="TMAX_24h_prec" ~ "Hourly maximum temperature over the 24 hours preceding the sampling event",
                                     Type=="RHMEAN_24h_prec" ~ "Average relative humidity over the 24 hours preceding the sampling event",
                                     Type=="RHMIN_24h_prec" ~ "Hourly minimum relative humidity over the 24 hours preceding the sampling event",
                                     Type=="RHMAX_24h_prec" ~ "Hourly maximum relative humidity over the 24 hours preceding the sampling event",
                                     Type=="RFSUM_48hprec" ~ "Cumulated rainfall over the 48 hours preceding the sampling event",
                                     Type=="RFMIN_48hprec" ~ "Hourly minimum rainfall over the 48 hours preceding the sampling event",
                                     Type=="RFMAX_48hprec" ~ "Hourly maximum rainfall over the 48 hours preceding the sampling event",
                                     Type=="TMEAN_48h_prec" ~ "Average temperature over the 48 hours preceding the sampling event",
                                     Type=="TMIN_48h_prec" ~ "Hourly minimum temperature over the 48 hours preceding the sampling event",
                                     Type=="TMAX_48h_prec" ~ "Hourly maximum temperature over the 48 hours preceding the sampling event",
                                     Type=="RHMEAN_48h_prec" ~ "Average relative humidity over the 48 hours preceding the sampling event",
                                     Type=="RHMIN_48h_prec" ~ "Hourly minimum relative humidity over the 48 hours preceding the sampling event",
                                     Type=="RHMAX_48h_prec" ~ "Hourly maximum relative humidity over the 48 hours preceding the sampling event",
                                     Type=="RFDode_0_4" ~ "Cumulated rainfall over the month preceding collection",
                                     Type=="TMINode_0_4" ~ "Daily minimum temperature over the month preceding collection",
                                     Type=="TMAXode_0_4" ~ "Daily maximum temperature over the month preceding collection",
                                     Type=="TMNode_0_4" ~ "Average temperature over the month preceding collection",
                                     Type=="WINDmf_0_4" ~ "Average wind speed over the month preceding collection",
                                     Type=="RHmf_0_4" ~ "Average relative humidity over the month preceding collection")) %>%
  mutate(measurementRemarks = case_when(grepl("lsm", Type) ~ "Landscape conditions in the close environment of each event",
                                        grepl("ode|mf", Type) ~ "Meteorological conditions on the weeks preceding the event",
                                        grepl("prec|collection", Type) ~ "Micro-climate in the close environment of each event"
  )) %>%
  mutate(measurementUnit = case_when(grepl("landscape|humidity", measurementType) ~ "%", 
                                     grepl("wind speed", measurementType) ~ "meters/second", 
                                     grepl("temperature", measurementType) ~ "Celsius degrees", 
                                     grepl("rainfall", measurementType) ~ "cumulated millimeters"))  %>%
  mutate(measurementMethod = case_when(grepl("lsm", Type) ~ "Derived from a land cover layer created by combining data from three open-access land cover layers (https://data.montpellier3m.fr/dataset/vegetation-fine-2019, https://land.copernicus.eu/en/products/urban-atlas/urban-atlas-2018, https://www.data.gouv.fr/fr/datasets/base-de-donnees-nationale-des-batiments/)",
                                       grepl("ode|mf", Type)  ~ "Open data from either Meteo France or the Observatoire Départemental de l’Eau et de l’Environnement",
                                       grepl("prec|collection", Type) ~ "Hygro Button data logger attached to each trap")) %>%
  arrange(eventID,measurementRemarks) %>% 
  group_by(eventID) %>%
  mutate(index2 = row_number()) %>%
  ungroup() %>%
  mutate(measurementID = paste0(eventID,"_",index2)) %>%
  dplyr::select(measurementID, eventID,measurementType, measurementRemarks, measurementValue, measurementUnit, measurementMethod)
  

  
write.csv(events,"data_gbif/events.csv", row.names = F)
write.csv(occurrence,"data_gbif/occurrences.csv", row.names = F)
write.csv(ExtendedMeasurementOrFact,"data_gbif/ExtendedMeasurementOrFact.csv", row.names = F)
write.csv(MeasurementOrFact,"data_gbif/MeasurementOrFact.csv", row.names = F)
