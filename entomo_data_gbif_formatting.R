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
  rename(NB_ALBO_F_PP = NB_ALBO_PP, NB_ALBO_F_GRAVIDE = NB_ALBO_GRAVIDE, NB_ALBO_F_P_ND= NB_ALBO_P_ND, NB_ALBO_F_PT_PP=NB_ALBO_PT_PP ,NB_ALBO_F_PT_NP=NB_ALBO_PT_NP)


# final column names : EventID	samplingProtocol	samplingEffort	sampleSizeValue	sampleSizeUnit	eventDate	LocationID	decimalLatitude	decimalLongitude	geodeticDatum	country	countryCode	locality stateProvince	coordinatePrecision	institutionCode

events <- df_releves_pieges_raw %>%
  rename(EventID = ID_COLLECTE_STRING, LocationID = ID_PIEGE) %>%
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
         country = "France", 
         countryCode = "FR", 
         municipality = "Montpellier",
         stateProvince = "Occitanie",
         county = "Herault",
         coordinateUncertaintyInMeters = "100", 
         institutionCode = "Université de Montpellier | IRD",
         habitat = case_when( ZONE =="parc" ~ "urban park",
                              ZONE == "pavillon" ~ "residential",
                              ZONE == "urbain" ~ "urban")) %>%
  dplyr::select(EventID,	country,	countryCode,	municipality, verbatimLocality, stateProvince	,county,	LocationID,decimalLatitude,	decimalLongitude,	geodeticDatum,coordinateUncertaintyInMeters, 	habitat, eventDate,year, month, startDayOfYear, endDayOfYear,institutionCode, 	samplingProtocol,	samplingEffort,	sampleSizeValue,	sampleSizeUnit, fieldNotes )

# https://dwc.tdwg.org/terms/#occurrence
occurrence <- df_releves_pieges_raw %>%
  rename(EventID = ID_COLLECTE_STRING) %>%
  dplyr::select(EventID,NB_ALBO_TOT:NB_MOUS_IND_F) %>%
  pivot_longer(!EventID,  names_to = "mosq", values_to = "count") %>%
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
         sex = case_when(grepl("_M",mosq) ~ "male",
                         grepl("_F",mosq) ~ "female",
                         grepl("SEX_ND",mosq) ~ NA,
                         TRUE ~ NA)
  ) %>%
  filter(!is.na(individualCount)) %>%
  mutate(occurrenceID = paste0(EventID,"_",mosq)) %>%
  dplyr::select(-mosq) %>%
  dplyr::select(-count)
  

# ExtendedMeasurementOrFact = données sur le statut physiologique des femelles (lié à l'occurence) 
ExtendedMeasurementOrFact <- df_releves_pieges_raw %>%
  rename(EventID = ID_COLLECTE_STRING) %>%
  dplyr::select(EventID,NB_ALBO_TOT:NB_MOUS_IND_F) %>%
  pivot_longer(!EventID,  names_to = "mosq", values_to = "count") %>%
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
  mutate(occurrenceID = paste0(EventID,"_",mosq2)) %>%
  dplyr::select(-mosq2) %>%
  left_join(occurrence) %>%
  dplyr::filter(individualCount>0) %>%
  mutate(measurementID = paste0(EventID,"_",mosq)) %>%
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
  dplyr::select(measurementID,EventID,occurrenceID,genericName,measurementType,measurementValue,measurementUnit,measurementDeterminedBy,measurementMethod)
  
  
# MeasurementOrFact = données environnementales  (lié à l'event)
## occ sol buffer 50m et 100m + micro climat (pendant la collecte + 24h + 48h) + meteo (T°, vent, precipitations le mois precedent)
MeasurementOrFact <- 

  

