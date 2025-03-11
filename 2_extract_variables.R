########################### Opening packages

library(terra) ## Version ‘1.7.78’
library(landscapemetrics) ## Version ‘2.1.4’
library(sf) ## Version ‘1.0.16’
library(tidyverse) ## Version ‘2.0.0’
library(exactextractr) ## Version ‘0.10.0’
library(raster) ## Version ‘3.6.26’
library(furrr) ## Version ‘0.3.1’
library(lubridate) ## Version ‘1.9.3’

########################### Opening of data

#### Opening trap location
colnames(pieges)
pieges <- st_read("02_Data/raw_data/01_Adults_Abundance_Longevity/P02_TRAPS_LOCATION.gpkg") %>% 
  filter(TYPE_PIEGE == 'bg-sentinel') |>
  mutate(ID_PIEGE = case_when(ID_PIEGE %in% c("BG_12","BG_13") ~ "BG_12_13",  ## Aggregation of traps less than 50 m apart because of possible interference between them 
                              ID_PIEGE %in% c("BG_15","BG_16") ~ "BG_15_16", ## Aggregation of traps less than 50 m apart because of possible interference between them 
                              TRUE ~ ID_PIEGE))|>
  as.data.frame()|>
  group_by(ID_PIEGE)|> 
  summarise(LATITUDE=mean(as.numeric(LATITUDE)), LONGITUDE=mean(as.numeric(LONGITUDE)))|>
              # To recreate points after data aggregation
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)
  ## selection of BG PRO trap

#### Opening of sampling entomological data: numebr of mosquitos collected, number of males, females, etc

df_releves_pieges_raw <- read.csv("02_Data/raw_data/01_Adults_Abundance_Longevity/P02_BG-ADULTS_LABO_DATA.csv", stringsAsFactors = F, sep = ";")
df_releves_pieges_raw <- df_releves_pieges_raw %>%
  mutate(HEURE_COLLECTE = ifelse(is.na(HEURE_COLLECTE),"10:00",HEURE_COLLECTE)) %>%
  rename(ID_COLLECTE_STRING = ID_COLLECTE) %>%
  mutate(DATE_POSE = parse_date(DATE_POSE,"%d/%m/%Y")) %>%
  mutate(DATE_COLLECTE = parse_date(DATE_COLLECTE,"%d/%m/%Y")) %>%
  mutate(ID_PIEGE = case_when(ID_PIEGE %in% c("BG_12","BG_13") ~ "BG_12_13",  ## Aggregation of traps less than 50 m apart because of possible interference between them 
                              ID_PIEGE %in% c("BG_15","BG_16") ~ "BG_15_16", ## Aggregation of traps less than 50 m apart because of possible interference between them 
                              TRUE ~ ID_PIEGE))
  

df_releves_pieges <- df_releves_pieges_raw %>% ## Aggregation of data with traps and adding ID_COLLECTE
  group_by(ID_PIEGE, DATE_POSE, DATE_COLLECTE) %>%
  summarise(NB_ALBO_TOT = round(mean(NB_ALBO_TOT, na.rm=T)),NB_ALBO_F= round(mean(NB_ALBO_F, na.rm=T)), NB_ALBO_M = round(mean(NB_ALBO_M, na.rm=T))) %>%
  as_tibble() %>%
  mutate(ID_COLLECTE = seq(1,nrow(.),1)) %>%
  arrange(DATE_COLLECTE) %>%
  filter(!is.na(DATE_COLLECTE))


## Creation of a numero of session, similar if two days of sampling are consecutive and the numero of sampling day 
df_releves_pieges$consecutive <- c(NA,diff(as.Date(df_releves_pieges$DATE_COLLECTE)) %in% c(0,1))
df_releves_pieges$consecutive[1] = FALSE
df_releves_pieges$num_session = cumsum(df_releves_pieges$consecutive==FALSE)
df_releves_pieges$consecutive=NULL
df_releves_pieges$JOUR<-NA
df_releves_pieges <- df_releves_pieges %>%
  left_join(df_releves_pieges_raw %>% dplyr::select(ID_PIEGE, DATE_POSE, DATE_COLLECTE, HEURE_COLLECTE)) %>%
  group_by_at(vars(-HEURE_COLLECTE)) %>%
  filter(row_number()==1) %>%
  arrange(ID_COLLECTE) %>%
  as_tibble()

df<-df_releves_pieges[1,]

for (i in unique(df_releves_pieges$num_session)){
  session<-df_releves_pieges|>
    filter(num_session==i)
  date<-unique(session$DATE_COLLECTE)
  session$JOUR<-dplyr::case_when(session$DATE_COLLECTE==max(date)~2,session$DATE_COLLECTE==min(date)~1)
  df<-rbind(df, session)
}

df<-df[-1,]
df<-df|>
  mutate(SESSION_DAY=paste(num_session, by='_',JOUR))

df_releves_pieges<-df
#### Openning land cover data

## Landcove rwith grouped vegetation
landcover_grouped_veget_rast <- raster("02_Data/processed_data/03_Landcover_Data/landcover_grouped_veget.tif")
landcover_grouped_veget_data_dict <- read.csv("02_Data/processed_data/03_Landcover_Data/landcover_grouped_veget_data_dic.csv", stringsAsFactors = F)
## Population Density
pop <- st_read("02_Data/processed_data/08_Population_Data/pop.gpkg")
## Socio economics data
filosofi <- st_read("02_Data/processed_data/06_Filosofi_Data/filosofi.gpkg")

#### Definition of buzzer size (radius in m)
buffer_sizes <- c(20,50,100,250) 

#### Openning meteorologic and microclimate data

## Meteo 
meteo_odee <- read.csv("02_Data/processed_data/09_Climatic_Data/meteo_macro_dpt.csv", stringsAsFactors = F)
meteo_synop <- read.csv("02_Data/processed_data/09_Climatic_Data/meteo_macro_synop.csv", stringsAsFactors = F)

## Microclimate
meteo_microclim <- read.csv("02_Data/processed_data/09_Climatic_Data/microclim.csv", stringsAsFactors = F)

## Definition of time lag
lag_max <- 42 # 42 days or 6 weeks

plan(multiprocess)
options(future.globals.maxSize= 20000*1024^2)

#### Opening air quality data
polluant<-st_read("02_Data/raw_data/11_Pollution_Air_Data/Montpellier_Pollution_Air_Data.shp")|>
  dplyr::filter(nom_com=="MONTPELLIER", date_debut>="2023-01-01" & date_fin<="2024-01-01")|> ## Selection of station in Montpellier and in the study period
  mutate(nom_station=nom_statio)|>
  dplyr::select(nom_com, nom_station, influence, date_debut, date_fin, nom_poll, valeur)|>
  arrange(date_debut)

########################### Extraction of spatial data
#########'The objectives are (i) to calculate landscape metrics with the land cover raster,
#########'(ii) to extract the socio economics and population density data
###########################

#### Calcul of landscape metrics of Land cover with  two vegetations classes 

df_lsm_landcover_veget <- buffer_sizes %>%
  set_names(buffer_sizes) %>%
  furrr::future_map_dfr(~landscapemetrics::sample_lsm(landscape = landcover_grouped_veget_rast,
                                                      y =  st_transform(pieges, raster::crs(landcover_grouped_veget_rast)),
                                                      plot_id = pieges$ID_PIEGE,
                                                      what = c("lsm_c_ca","lsm_c_area_mn",  "lsm_c_ed", "lsm_c_te","lsm_c_frac_mn", "lsm_c_np","lsm_c_pland" , "lsm_l_shdi"), ## Percentage of every class and landscape diversity 
                                                      shape = "circle",
                                                      size = .,
                                                      all_classes = T),
                        .id = "buffer")

df_lsm_landcover_veget<-df_lsm_landcover_veget%>%
  dplyr::filter(metric%in%c("area_mn","ca","ed","np","pland","te" ,"shdi"))%>%
  dplyr::mutate(value=ifelse(metric%in%c("area_mn","ca","ed","np","pland","te")& is.na(value), 0, value))

#### Socio economics data
  pieges_proj <- st_transform(pieges,terra::crs(filosofi))
  df_filosofi <- sf::st_intersection(pieges_proj,filosofi) %>% 
    st_drop_geometry() %>%
    dplyr::select(ID_PIEGE, Men, Men_pauv, Ind_snv, Log_av45, Log_45_70, Log_70_90, Log_ap90, Log_soc)

#### Population density data
  pieges_proj <- st_transform(pieges,terra::crs(pop))
  
  POP <- buffer_sizes %>%
    set_names(buffer_sizes) %>%
    purrr::map_dfr(~sf::st_intersection(st_buffer(pieges_proj,.), pop),
                   .id = "buffer") %>%
    st_drop_geometry() %>%
    group_by(buffer,ID_PIEGE) %>%
    summarise(sum = sum(pop_2016), sd = sd(pop_2016)) %>%
    complete(fill = list(sum = 0, sd = 0)) %>%
    as_tibble()
  
############################ Meteorological and temporal data 
#########'The objectives are (i) to select the interesting variables and to calculate the GDD
#########'(ii) to calculate thel for the different time lags
#########'(iii) to prepare them for the cross correlation maps analysis. 
############################ 
  
  ##### Meteo ODEE
  
  meteo <- meteo_odee %>%
    rename(RFDode = precipitations, TMINode = tmin, TMAXode = tmax, TMNode = tmean, TAMPode = tamp) %>% ### Selecting interesting variables
    filter(!is.na(RFDode), !is.na(TMINode), !is.na(TMAXode), !is.na(TMNode), !is.na(TAMPode))|>
    mutate(TMINode_GDD = ifelse(TMINode>11, TMINode, 11),TMAXode_GDD = case_when(TMAXode<11~11, TMAXode==11~11, TMAXode==30~30, 11<TMAXode & TMAXode<30~TMAXode, TMAXode>30~30), GDDjour=(TMAXode_GDD+TMINode_GDD)/2-11 ) ## Adding the daily Growing Degree Day


  df_releves_pieges2 <- df_releves_pieges %>%
    dplyr::select(ID_COLLECTE, ID_PIEGE, DATE_COLLECTE)
  
  
  df_meteo_pieges <- data.frame(ID_COLLECTE = numeric(),ID_PIEGE = character(), date = character(), stringsAsFactors = F) 
  for(i in 1:nrow(df_releves_pieges2)){ ## Selecting of every sampling date the different time lags date associated
    for(j in 0:lag_max){
      df_meteo_pieges <- rbind(df_meteo_pieges,
                               data.frame(ID_COLLECTE = df_releves_pieges2$ID_COLLECTE[i],
                                          ID_PIEGE = df_releves_pieges2$ID_PIEGE[i],
                                          date = as.character(as.Date(df_releves_pieges2$DATE_COLLECTE[i]-j)),
                                          lag_n = j,
                                          stringsAsFactors = F))
    }
  }
  
  ## To associate for every time lag the different value of rainfall, tmin, tmax, etc
  df_meteo_pieges2 <- df_meteo_pieges %>%  
    left_join(meteo) %>% 
    pivot_longer( !(ID_COLLECTE:lag_n), names_to = "var", values_to = 'val') %>%
    mutate(idpointdecapture = paste0(ID_PIEGE,"_",ID_COLLECTE))
  
  ## Function which aggregates weekly the different variables
   fun_summarize_week <- function(df_meteo_pieges2,var_to_summarize){
    
    if(grepl("RFD",var_to_summarize)|grepl("GDDsemaine",var_to_summarize)){ ## to sum rainfall and GDD weekly 
      df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
        filter(var==var_to_summarize) %>%
        group_by(idpointdecapture,lag_n = lubridate::week(date)) %>%
        summarise(val=sum(val, na.rm = T),date = min(date)) %>%
        group_by(idpointdecapture) %>%
        mutate(lag_n=seq(n()-1,0,-1)) %>%
        mutate(var = var_to_summarize) %>%
        as_tibble()
    }
    
     else {
      df_meteo_pieges2_summarize <- df_meteo_pieges2 %>% ## to average weekly the others : wind speed, relative humidity, temperature minmal, maximal and mean
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

  df_meteo_pieges_summ <- fun_summarize_week(df_meteo_pieges2,"RFDode") %>% ## To link every data frame between them
    bind_rows(fun_summarize_week(df_meteo_pieges2,"TMINode")) %>%
    bind_rows(fun_summarize_week(df_meteo_pieges2,"TMAXode")) %>%
    bind_rows(fun_summarize_week(df_meteo_pieges2,"TMNode"))%>%
    bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDjour"))%>%
    bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDsemaine"))
  
  ## Function to prepare the data frame for the modelling and for the cross correlation maps, putting the value for all variables selected for every time lag for every collection 
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
        } 
        else if (function_to_apply=="sum"){
          df_timeseries_wide[column_name] <- rowSums(df_timeseries_wide[,i:j], na.rm = T)
        }
      }
    }
    
    for(i in 2:max_col){
      colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
    }
    
    return(df_timeseries_wide)
  }
  
  ## Realization of the function for the different values
  df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ,"RFDode","sum")
  df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ,"TMINode","mean")
  df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ,"TMAXode","mean")
  df_meteo_pieges_summ_wide4 <- fun_ccm_df(df_meteo_pieges_summ,"TMNode","mean")
  df_meteo_pieges_summ_wide5 <- fun_ccm_df(df_meteo_pieges_summ,"GDDjour","mean")
  df_meteo_pieges_summ_wide6 <- fun_ccm_df(df_meteo_pieges_summ,"GDDsemaine","sum")
  
  ## To put together all data frame
   df_meteo_pieges_summ_wide <- df_meteo_pieges_summ_wide1 %>%
    left_join(df_meteo_pieges_summ_wide2) %>%
    left_join(df_meteo_pieges_summ_wide3) %>%
    left_join(df_meteo_pieges_summ_wide4)%>%
    left_join(df_meteo_pieges_summ_wide5)%>%
    left_join(df_meteo_pieges_summ_wide6)
  

##### Meteo Synop from Meteo france
  
  
  meteo <- meteo_synop %>%
    rename(RFDmf = precipitations, TMINmf = tmin, TMAXmf = tmax, TMNmf = tmean, TAMPmf = tamp, RHmf = rh, WINDmf = wind) %>%
    filter(!is.na(RFDmf), !is.na(TMINmf), !is.na(TMAXmf), !is.na(TMNmf), !is.na(TAMPmf), !is.na(RHmf), !is.na(WINDmf)) ## Selection of Tmin, tmax, tmean, wind speed, rainfall and relative humidity
  
  df_releves_pieges2 <- df_releves_pieges %>%
    dplyr::select(ID_COLLECTE, ID_PIEGE, DATE_COLLECTE)
  
  
  df_meteo_pieges <- data.frame(ID_COLLECTE = numeric(),ID_PIEGE = character(), date = character(), stringsAsFactors = F)  ## Selecting of every sampling date the different time lags date associated
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
  
  ## To associate for every time lag the different value of rainfall, tmin, tmax, etc
  df_meteo_pieges2 <- df_meteo_pieges %>% 
    left_join(meteo) %>%
    pivot_longer( !(ID_COLLECTE:lag_n), names_to = "var", values_to = 'val') %>%
    mutate(idpointdecapture = paste0(ID_PIEGE,"_",ID_COLLECTE))
  ## Function which aggregates weekly the different variables and putting all together
  df_meteo_pieges_summ <- fun_summarize_week(df_meteo_pieges2,"RFDmf") %>%
    bind_rows(fun_summarize_week(df_meteo_pieges2,"TMINmf")) %>%
    bind_rows(fun_summarize_week(df_meteo_pieges2,"TMAXmf")) %>%
    bind_rows(fun_summarize_week(df_meteo_pieges2,"TMNmf")) %>%
    bind_rows(fun_summarize_week(df_meteo_pieges2,"RHmf")) %>%
    bind_rows(fun_summarize_week(df_meteo_pieges2,"WINDmf"))
  ## Preparing the data for the modelling ans the cross correlation maps, putting the value for all variables selected for every time lag for every collection 
  df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ,"RFDmf","sum")
  df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ,"TMINmf","mean")
  df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ,"TMAXmf","mean")
  df_meteo_pieges_summ_wide4 <- fun_ccm_df(df_meteo_pieges_summ,"TMNmf","mean")
  df_meteo_pieges_summ_wide5 <- fun_ccm_df(df_meteo_pieges_summ,"RHmf","mean")
  df_meteo_pieges_summ_wide6 <- fun_ccm_df(df_meteo_pieges_summ,"WINDmf","mean")
  
  ## Putting data frame all togetger
  df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
    left_join(df_meteo_pieges_summ_wide2) %>%
    left_join(df_meteo_pieges_summ_wide3) %>%
    left_join(df_meteo_pieges_summ_wide4) %>%
    left_join(df_meteo_pieges_summ_wide5) %>%
    left_join(df_meteo_pieges_summ_wide6)
  
  
##### In order to indicate Rainfall during collection

  
  
df_rainfall <- read.csv('02_Data/raw_data/09_Climatic_Data/09_Montpellier_ODEE/09_Montpellier_ODEE_Data/Station_202_20210526_H.csv', sep = ";",stringsAsFactors = F, na.strings = "", dec = ",", col.names = c('date',"heure","precipitations","temperatures")) %>%
  mutate(date = parse_date_time(date,"%d/%m/%Y")) %>%
  mutate(date_time = ymd_hms(paste(date,heure))) 

df_releves_pieges3 <- df_releves_pieges %>% 
  mutate(DATE_HEURE_POSE = ymd_hm(paste(DATE_POSE,HEURE_COLLECTE)),
         DATE_HEURE_COLLECTE = ymd_hm(paste(DATE_COLLECTE,HEURE_COLLECTE))
  ) ## selection the date of sampling on the ODEE data

## Calculation of precipitation falling between the break and collection on the same day, one day before and two days before
for(i in 1:nrow(df_releves_pieges2)){
  
  th_df_rainfall <- df_rainfall %>% filter(date_time >= df_releves_pieges3$DATE_HEURE_POSE[i], date_time <= df_releves_pieges3$DATE_HEURE_COLLECTE[i] )
  df_releves_pieges3$RFSUM_collection[i] = sum(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$RFMIN_collection[i] = min(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$RFMAX_collection[i] = max(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$pres_RF_collection[i] = ifelse(df_releves_pieges3$RFSUM_collection[i]>0.5, 1, 0)
  
  
  th_df_rainfall <- df_rainfall %>% filter(date_time >= df_releves_pieges3$DATE_HEURE_POSE[i]-24*3600, date_time <= df_releves_pieges3$DATE_HEURE_POSE[i] )
  df_releves_pieges3$RFSUM_24hprec[i] = sum(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$RFMIN_24hprec[i] = min(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$RFMAX_24hprec[i] = max(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$pres_RF_24hprec[i] = ifelse(df_releves_pieges3$RFSUM_24hprec[i]>0.5, 1, 0)
  
  th_df_rainfall <- df_rainfall %>% filter(date_time >= df_releves_pieges3$DATE_HEURE_POSE[i]-48*3600, date_time <= df_releves_pieges3$DATE_HEURE_POSE[i] )
  df_releves_pieges3$RFSUM_48hprec[i] = sum(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$RFMIN_48hprec[i] = min(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$RFMAX_48hprec[i] = max(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$pres_RF_48hprec[i] = ifelse(df_releves_pieges3$RFSUM_48hprec[i]>0.5, 1, 0)
  
  th_df_rainfall <- df_rainfall %>% filter(date_time >= df_releves_pieges3$DATE_HEURE_POSE[i]-48*3600, date_time <=  df_releves_pieges3$DATE_HEURE_POSE[i]-24*3600 )
  df_releves_pieges3$RFSUM_24h_48hprec[i] = sum(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$RFMIN_24h_48hprec[i] = min(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$RFMAX_24h_48hprec[i] = max(th_df_rainfall$precipitations, na.rm = T)
  df_releves_pieges3$pres_RF_24h_48hprec[i] = ifelse(df_releves_pieges3$RFSUM_24h_48hprec[i]>0.5, 1, 0)
  
  
}

df_rf_during_coll <- df_releves_pieges3 %>% dplyr::select(-c("NB_ALBO_TOT","DATE_POSE",  "DATE_COLLECTE", "num_session", "HEURE_COLLECTE", "DATE_HEURE_POSE", "DATE_HEURE_COLLECTE"))|>
  mutate(pres_RF_collection = ifelse(RFSUM_collection>0.5, 1, 0), 
         pres_RF_24hprec = ifelse(RFSUM_24hprec>0.5, 1, 0),
         pres_RF_48hprec = ifelse(RFSUM_48hprec>0.5, 1, 0),
         pres_RF_24h_48hprec = ifelse(RFSUM_24h_48hprec>0.5, 1, 0))


###### In order to calculate Atmospheric pressure

########################### Donnees temporelles : pression

th_meteo_pression<-meteo_pression|>
  mutate(date=as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))|>
  filter(date>"2023-04-10")

extract_pression_lag <- function(th_meteo_pression, lag_sup_nday_bef_collection, lag_inf_nday_bef_collection, var_suffix) {
  
  # To filter the data on the period of interest
  df_meteo_pression <- th_meteo_pression %>% 
    filter(date >= th_df_releves_pieges$DATE_HEURE_POSE - lag_inf_nday_bef_collection * 24 * 3600 & 
             date <= th_df_releves_pieges$DATE_HEURE_POSE - lag_sup_nday_bef_collection * 24 * 3600)
  
  if (nrow(df_meteo_pression) > 0) {
    Patmean <- mean(df_meteo_pression$patm, na.rm = TRUE)
    Patmin <- min(df_meteo_pression$patm, na.rm = TRUE)
    Patmax <- max(df_meteo_pression$patm, na.rm = TRUE)
    Pmermean <- mean(df_meteo_pression$pmer, na.rm = TRUE)
    Pmermin <- min(df_meteo_pression$pmer, na.rm = TRUE)
    Pmermax <- max(df_meteo_pression$pmer, na.rm = TRUE)
    
    # To calculate the difference between pressure
    df_meteo_pression <- df_meteo_pression %>%
      mutate(date_only = as.Date(date)) # Extraire uniquement la date (sans heure)
    
    daily_means <- df_meteo_pression %>%
      group_by(date_only) %>%
      summarize(Patm_daily_mean = mean(patm, na.rm = TRUE),
                Pmer_daily_mean = mean(pmer, na.rm = TRUE)) %>%
      arrange(date_only) %>%
      mutate(Patm_diff_prev_day = Patm_daily_mean - lag(Patm_daily_mean),
             Pmer_diff_prev_day = Pmer_daily_mean - lag(Pmer_daily_mean))
    
    daily_extremes <- df_meteo_pression %>%
      group_by(date_only) %>%
      summarize(Patm_daily_range = max(patm, na.rm = TRUE) - min(patm, na.rm = TRUE),
                Pmer_daily_range = max(pmer, na.rm = TRUE) - min(pmer, na.rm = TRUE))
    
    pression <- data.frame(
      Patmean = Patmean,
      Patmin = Patmin,
      Patmax = Patmax,
      Pmermean = Pmermean, 
      Pmermin = Pmermin, 
      Pmermax = Pmermax,
      Patm_diff_prev_day = tail(daily_means$Patm_diff_prev_day, 1),
      Pmer_diff_prev_day = tail(daily_means$Pmer_diff_prev_day, 1),
      Patm_daily_range = max(daily_extremes$Patm_daily_range, na.rm = TRUE),
      Pmer_daily_range = max(daily_extremes$Pmer_daily_range, na.rm = TRUE)
    )
    
  } else {

    pression <- data.frame(
      Patmean = NA,
      Patmin = NA,
      Patmax = NA,
      Pmermean = NA, 
      Pmermin = NA, 
      Pmermax = NA,
      Patm_diff_prev_day = NA,
      Pmer_diff_prev_day = NA,
      Patm_daily_range = NA,
      Pmer_daily_range = NA
    )
  }
  

  colnames(pression) <- paste0(colnames(pression), "_", var_suffix)
  
  return(pression)
}

#### To calculate the data 

df_pression <- NULL
for (i in 1:nrow(df_releves_pieges)) {
  
  th_df_releves_pieges <- df_releves_pieges[i,]
  th_df_releves_pieges$DATE_HEURE_POSE <- ymd_hm(paste(th_df_releves_pieges$DATE_POSE,th_df_releves_pieges$HEURE_COLLECTE))
  th_df_releves_pieges$DATE_HEURE_COLLECTE <- ymd_hm(paste(th_df_releves_pieges$DATE_COLLECTE,th_df_releves_pieges$HEURE_COLLECTE))
  
  
  # During sampling
  pression_during_collection <- th_meteo_pression %>% 
    filter(date >= th_df_releves_pieges$DATE_HEURE_POSE & date <= th_df_releves_pieges$DATE_HEURE_COLLECTE)
  
  if (nrow(pression_during_collection) > 0) {
    
    Patmean <- mean(pression_during_collection$patm, na.rm = TRUE)
    Patmin <- min(pression_during_collection$patm, na.rm = TRUE)
    Patmax <- max(pression_during_collection$patm, na.rm = TRUE)
    Pmermean <- mean(pression_during_collection$pmer, na.rm = TRUE)
    Pmermin <- min(pression_during_collection$pmer, na.rm = TRUE)
    Pmermax <- max(pression_during_collection$pmer, na.rm = TRUE)
    
    pression_during_collection <- pression_during_collection %>%
      mutate(date_only = as.Date(date))
    
    daily_means_collection <- pression_during_collection %>%
      group_by(date_only) %>%
      summarize(Patm_daily_mean = mean(patm, na.rm = TRUE),
                Pmer_daily_mean = mean(pmer, na.rm = TRUE)) %>%
      mutate(Patm_diff_prev_day = Patm_daily_mean - lag(Patm_daily_mean),
             Pmer_diff_prev_day = Pmer_daily_mean - lag(Pmer_daily_mean))
    
    daily_extremes_collection <- pression_during_collection %>%
      group_by(date_only) %>%
      summarize(Patm_daily_range = max(patm, na.rm = TRUE) - min(patm, na.rm = TRUE),
                Pmer_daily_range = max(pmer, na.rm = TRUE) - min(pmer, na.rm = TRUE))
    
    Patm_diff_prev_day <- tail(daily_means_collection$Patm_diff_prev_day, 1)
    Pmer_diff_prev_day <- tail(daily_means_collection$Pmer_diff_prev_day, 1)
    Patm_daily_range <- max(daily_extremes_collection$Patm_daily_range, na.rm = TRUE)
    Pmer_daily_range <- max(daily_extremes_collection$Pmer_daily_range, na.rm = TRUE)
    
    pression <- data.frame(
      Patmean = Patmean,
      Patmin = Patmin,
      Patmax = Patmax,
      Pmermean = Pmermean, 
      Pmermin = Pmermin, 
      Pmermax = Pmermax,
      Patm_diff_prev_day = Patm_diff_prev_day,
      Pmer_diff_prev_day = Pmer_diff_prev_day,
      Patm_daily_range = Patm_daily_range,
      Pmer_daily_range = Pmer_daily_range
    )
    
    colnames(pression) <- paste0(colnames(pression), "_collection")
    
    th_df_releves_pieges_pression <- cbind(
      ID_PIEGE = th_df_releves_pieges$ID_PIEGE,
      DATE_POSE = th_df_releves_pieges$DATE_POSE,
      DATE_COLLECTE = th_df_releves_pieges$DATE_COLLECTE,
      pression
    )
    
    # Before sampling
    pression_24hprec_collection <- extract_pression_lag(th_meteo_pression, 0, 1, "24h_prec")
    th_df_releves_pieges_pression <- cbind(th_df_releves_pieges_pression, pression_24hprec_collection)
    
    pression_48hprec_collection <- extract_pression_lag(th_meteo_pression, 0, 2, "48h_prec")
    th_df_releves_pieges_pression <- cbind(th_df_releves_pieges_pression, pression_48hprec_collection)
    
    pression_24h_48hprec_collection <- extract_pression_lag(th_meteo_pression, 1, 2, "24h_48h_prec")
    th_df_releves_pieges_pression <- cbind(th_df_releves_pieges_pression, pression_24h_48hprec_collection)
    
    df_pression <- rbind(df_pression, th_df_releves_pieges_pression)
  }
}



#### For missing values

df_pression$Patmean_24h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-25" & df_pression$DATE_COLLECTE=="2023-07-26"]<-df_pression$Patmean_24h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-25"&df_pression$DATE_COLLECTE=="2023-07-26"]
df_pression$Patmin_24h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-25" & df_pression$DATE_COLLECTE=="2023-07-26"]<-df_pression$Patmin_24h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-25"&df_pression$DATE_COLLECTE=="2023-07-26"]
df_pression$Patmax_24h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-25" & df_pression$DATE_COLLECTE=="2023-07-26"]<-df_pression$Patmax_24h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-25"&df_pression$DATE_COLLECTE=="2023-07-26"]
df_pression$Pmermean_24h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-25" & df_pression$DATE_COLLECTE=="2023-07-26"]<-df_pression$Pmermean_24h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-25"&df_pression$DATE_COLLECTE=="2023-07-26"]
df_pression$Pmermin_24h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-25" & df_pression$DATE_COLLECTE=="2023-07-26"]<-df_pression$Pmermin_24h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-25"&df_pression$DATE_COLLECTE=="2023-07-26"]
df_pression$Pmermax_24h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-25" & df_pression$DATE_COLLECTE=="2023-07-26"]<-df_pression$Pmermax_24h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-25"&df_pression$DATE_COLLECTE=="2023-07-26"]


df_pression$Patmean_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-26" & df_pression$DATE_COLLECTE=="2023-07-27"]<-df_pression$Patmean_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-26"&df_pression$DATE_COLLECTE=="2023-07-27"]
df_pression$Patmin_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-26" & df_pression$DATE_COLLECTE=="2023-07-27"]<-df_pression$Patmin_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-26"&df_pression$DATE_COLLECTE=="2023-07-27"]
df_pression$Patmax_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-26" & df_pression$DATE_COLLECTE=="2023-07-27"]<-df_pression$Patmax_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-26"&df_pression$DATE_COLLECTE=="2023-07-27"]
df_pression$Pmermean_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-26" & df_pression$DATE_COLLECTE=="2023-07-27"]<-df_pression$Pmermean_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-26"&df_pression$DATE_COLLECTE=="2023-07-27"]
df_pression$Pmermin_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-26" & df_pression$DATE_COLLECTE=="2023-07-27"]<-df_pression$Pmermin_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-26"&df_pression$DATE_COLLECTE=="2023-07-27"]
df_pression$Pmermax_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_01", "BG_02", "BG_21", "BG_22", "BG_23", "BG_24") & df_pression$DATE_POSE=="2023-07-26" & df_pression$DATE_COLLECTE=="2023-07-27"]<-df_pression$Pmermax_24h_48h_prec[df_pression$ID_PIEGE%in%c("BG_03")&df_pression$DATE_POSE=="2023-07-26"&df_pression$DATE_COLLECTE=="2023-07-27"]


pression_during_collection <- th_meteo_pression %>% 
  filter(date >= '2023-07-24 00:00:00' & date <= "2023-07-25 12:00:00") %>%
  mutate(date_only = as.Date(date))
daily_means_collection <- pression_during_collection %>%
  group_by(date_only) %>%
  summarize(Patm_daily_mean = mean(patm, na.rm = TRUE),
            Pmer_daily_mean = mean(pmer, na.rm = TRUE)) %>%
  mutate(Patm_diff_prev_day = Patm_daily_mean - lag(Patm_daily_mean),
         Pmer_diff_prev_day = Pmer_daily_mean - lag(Pmer_daily_mean))

daily_extremes_collection <- pression_during_collection %>%
  group_by(date_only) %>%
  summarize(Patm_daily_range = max(patm, na.rm = TRUE) - min(patm, na.rm = TRUE),
            Pmer_daily_range = max(pmer, na.rm = TRUE) - min(pmer, na.rm = TRUE))

Patm_diff_prev_day <- tail(daily_means_collection$Patm_diff_prev_day, 1)
Pmer_diff_prev_day <- tail(daily_means_collection$Pmer_diff_prev_day, 1)
Patm_daily_range <- max(daily_extremes_collection$Patm_daily_range, na.rm = TRUE)
Pmer_daily_range <- max(daily_extremes_collection$Pmer_daily_range, na.rm = TRUE)

df_pression$Patm_diff_prev_day_24h_prec[df_pression$DATE_COLLECTE=="2023-07-26"]<-Patm_diff_prev_day
df_pression$Pmer_diff_prev_day_24h_prec[df_pression$DATE_COLLECTE=="2023-07-26"]<-Pmer_diff_prev_day
df_pression$Patm_daily_range_24h_prec[df_pression$DATE_COLLECTE=="2023-07-26"]<-Patm_daily_range
df_pression$Pmer_daily_range_24h_prec[df_pression$DATE_COLLECTE=="2023-07-26"]<-Pmer_daily_range

pression_during_collection <- th_meteo_pression %>% 
  filter(date >= '2023-07-24 00:00:00' & date <= "2023-07-26 12:00:00") %>%
  mutate(date_only = as.Date(date))
daily_means_collection <- pression_during_collection %>%
  group_by(date_only) %>%
  summarize(Patm_daily_mean = mean(patm, na.rm = TRUE),
            Pmer_daily_mean = mean(pmer, na.rm = TRUE)) %>%
  mutate(Patm_diff_prev_day = Patm_daily_mean - lag(Patm_daily_mean),
         Pmer_diff_prev_day = Pmer_daily_mean - lag(Pmer_daily_mean))

daily_extremes_collection <- pression_during_collection %>%
  group_by(date_only) %>%
  summarize(Patm_daily_range = max(patm, na.rm = TRUE) - min(patm, na.rm = TRUE),
            Pmer_daily_range = max(pmer, na.rm = TRUE) - min(pmer, na.rm = TRUE))

Patm_diff_prev_day <- tail(daily_means_collection$Patm_diff_prev_day, 1)
Pmer_diff_prev_day <- tail(daily_means_collection$Pmer_diff_prev_day, 1)
Patm_daily_range <- max(daily_extremes_collection$Patm_daily_range, na.rm = TRUE)
Pmer_daily_range <- max(daily_extremes_collection$Pmer_daily_range, na.rm = TRUE)

df_pression$Patm_diff_prev_day_24h_48h_prec[df_pression$DATE_COLLECTE=="2023-07-27"]<-Patm_diff_prev_day
df_pression$Pmer_diff_prev_day_24h_48h_prec[df_pression$DATE_COLLECTE=="2023-07-27"]<-Pmer_diff_prev_day
df_pression$Patm_daily_range_24h_48h_prec[df_pression$DATE_COLLECTE=="2023-07-27"]<-Patm_daily_range
df_pression$Pmer_daily_range_24h_48h_prec[df_pression$DATE_COLLECTE=="2023-07-27"]<-Pmer_daily_range


df_pression <- df_releves_pieges %>%
  dplyr::select(ID_PIEGE, DATE_POSE, DATE_COLLECTE,  ID_COLLECTE) %>%
  left_join(df_pression) %>%
  dplyr::select(-c("DATE_POSE", "DATE_COLLECTE"))

############################ Micro climatic data : during collection, 24h and 48h before 
#########'The objectives are (i) to select the interesting variables 
#########'(ii) to calculate thel for the different time lags
############################ 

extract_microclim_lag <- function(df_meteo_microclim, lag_sup_nday_bef_collection, lag_inf_nday_bef_collection, var_suffix){ ## Function which calculates for every date and hour of sampling the minimum, max mean temperature and relative humidity during differentr time lags
  
  df_meteo_microclim <- th_meteo_microclim %>% 
    filter(date_heure >= th_df_releves_pieges$DATE_HEURE_POSE-lag_inf_nday_bef_collection*24*3600 & date_heure <= th_df_releves_pieges$DATE_HEURE_POSE-lag_sup_nday_bef_collection*24*3600) ## 3600 because in minutes
  
  TMEAN <- mean(df_meteo_microclim$temperature, na.rm=T)
  TMIN <- min(df_meteo_microclim$temperature, na.rm=T)
  TMAX <- max(df_meteo_microclim$temperature, na.rm=T)
  TAMP<-max(meteo_during_collection$temperature, na.rm=T)-min(meteo_during_collection$temperature, na.rm=T)
  
  RHMEAN <- mean(df_meteo_microclim$humidite, na.rm=T)
  RHMIN <- min(df_meteo_microclim$humidite, na.rm=T)
  RHMAX <- max(df_meteo_microclim$humidite, na.rm=T)
  
  microclim = data.frame(TMEAN = TMEAN,
                         TMIN = TMIN,
                         TMAX = TMAX,
                         TAMP = TAMP,
                         RHMEAN = RHMEAN, 
                         RHMIN = RHMIN, 
                         RHMAX = RHMAX,
                         min_date = as.numeric(difftime(th_df_releves_pieges$DATE_HEURE_POSE, min(df_meteo_microclim$date_heure), units="days"))
                         )
  
  colnames(microclim) <- paste0(colnames(microclim),"_",var_suffix)
  
  return(microclim)
   
}

meteo_microclim$date_heure <- ymd_hms(meteo_microclim$date_heure)

meteo_microclim <- meteo_microclim %>% ## Aggregating teh value of the data logger of traps too closed
  mutate(date_heure =  ymd_hms(date_heure)) %>%
  mutate(ID_PIEGE = case_when(ID_PIEGE %in% c("BG_12","BG_13") ~ "BG_12_13",
                            ID_PIEGE %in% c("BG_15","BG_16") ~ "BG_15_16",
                            TRUE ~ ID_PIEGE))
                            
df_microclim <- NULL

for(i in 1:nrow(df_releves_pieges)){
  
  th_df_releves_pieges <- df_releves_pieges[i,]
  th_df_releves_pieges$DATE_HEURE_POSE <- ymd_hm(paste(th_df_releves_pieges$DATE_POSE,th_df_releves_pieges$HEURE_COLLECTE))
  th_df_releves_pieges$DATE_HEURE_COLLECTE <- ymd_hm(paste(th_df_releves_pieges$DATE_COLLECTE,th_df_releves_pieges$HEURE_COLLECTE))
    
  th_meteo_microclim <- meteo_microclim %>% filter(ID_PIEGE == th_df_releves_pieges$ID_PIEGE)
  
  #### Extraction of micro climate data during sampling
  meteo_during_collection <- th_meteo_microclim %>% 
    filter(date_heure >= th_df_releves_pieges$DATE_HEURE_POSE & date_heure <= th_df_releves_pieges$DATE_HEURE_COLLECTE)
 
  if(nrow(meteo_during_collection)>0){
    
  TMEAN <- mean(meteo_during_collection$temperature, na.rm=T)
  TMIN <- min(meteo_during_collection$temperature, na.rm=T)
  TMAX <- max(meteo_during_collection$temperature, na.rm=T)
  TAMP<-max(meteo_during_collection$temperature, na.rm=T)-min(meteo_during_collection$temperature, na.rm=T)
  
  RHMEAN <- mean(meteo_during_collection$humidite, na.rm=T)
  RHMIN <- min(meteo_during_collection$humidite, na.rm=T)
  RHMAX <- max(meteo_during_collection$humidite, na.rm=T)
  
  d_microclim = data.frame(TMEAN = TMEAN,
                         TMIN = TMIN,
                         TMAX = TMAX,
                         TAMP = TAMP,
                         RHMEAN = RHMEAN, 
                         RHMIN = RHMIN, 
                         RHMAX = RHMAX)
  
  colnames(d_microclim) <- paste0(colnames(d_microclim),"_collection")
  
  th_df_releves_pieges_microclim <- cbind(ID_PIEGE=th_df_releves_pieges$ID_PIEGE,
                                          DATE_POSE=th_df_releves_pieges$DATE_POSE,
                                          DATE_COLLECTE=th_df_releves_pieges$DATE_COLLECTE,
                                          d_microclim)
  
  
  #### Extraction of micro climate data 24h before sampling
  
  meteo_24hprec_collection <- extract_microclim_lag(th_meteo_microclim, 0,1,"24h_prec")
  th_df_releves_pieges_microclim <- cbind(th_df_releves_pieges_microclim,meteo_24hprec_collection)
  
  #### Extraction of micro climate data 48h before sampling
  meteo_48hprec_collection <- extract_microclim_lag(th_meteo_microclim, 0,2,"48h_prec")
  th_df_releves_pieges_microclim <- cbind(th_df_releves_pieges_microclim,meteo_48hprec_collection)
  
  #### Extraction of micro climate data 24h and  48h before sampling
  meteo_24h_48hprec_collection <- extract_microclim_lag(th_meteo_microclim, 1,2,"24h_48h_prec")
  th_df_releves_pieges_microclim <- cbind(th_df_releves_pieges_microclim,meteo_24h_48hprec_collection)
  
  th_df_releves_pieges_microclim <- cbind(th_df_releves_pieges_microclim,meteo_6sprec_collection)
  
  
  
  df_microclim <- rbind(df_microclim,th_df_releves_pieges_microclim) ### putting all together
  
  }
  
}

#### To add to the data frame of sampling data
df_microclim <- df_releves_pieges %>%
  dplyr::select(ID_PIEGE, DATE_POSE, DATE_COLLECTE,  ID_COLLECTE) %>%
  left_join(df_microclim) %>%
  dplyr::select(-contains("min_date")) %>%
  dplyr::select(-c("DATE_POSE", "DATE_COLLECTE"))


############################ Air quality data: extraction of spatial data and calcul of temporal data
#########'The objectives are (i) to select the interesting variables 
#########'(ii) to calculate thel for the different time lags
#########'(iii) to prepare them for the cross correlation maps analysis. 
############################ 

#### Selection of stations the nearest of every traps
pieges_proj <- st_transform(pieges,terra::crs(polluant))
polluants_piege<-st_join(pieges_proj,
                         polluant,
                         join = st_nearest_feature
)
station_piege<-polluants_piege|>
  dplyr::select(ID_PIEGE,nom_station)

#### Preparation of the different collection date time lags

df_releves_pieges_4<-df_releves_pieges|>
  dplyr::select(ID_PIEGE , ID_COLLECTE, DATE_COLLECTE)|>
  left_join(station_piege)

lag_max<-42
df_polluant_pieges <- data.frame(ID_COLLECTE  = character(),ID_PIEGE = character(), date_debut = Date(), nom_station=as.character(), stringsAsFactors = F) 
for(i in 1:nrow(df_releves_pieges_4)){
  for(j in 0:lag_max){
    df_polluant_pieges <- rbind(df_polluant_pieges,
                                data.frame(ID_COLLECTE   = df_releves_pieges_4$ID_COLLECTE[i],
                                           ID_PIEGE = df_releves_pieges_4$ID_PIEGE[i],
                                           date_debut = as.Date(df_releves_pieges_4$DATE_COLLECTE[i])-j,
                                           nom_station=df_releves_pieges_4$nom_station[i],
                                           lag_n = j,
                                           stringsAsFactors = F))
  }
}


df_polluant_pieges_2 <- df_polluant_pieges %>%  ## Selection of polluants: NO, NO2 and NOX
  left_join(polluant) %>%
  filter(nom_poll%in%c("NO", "NO2", "NOX"))|>
  dplyr::mutate(DATE=date_debut, var=nom_poll)|>
  dplyr::select(ID_COLLECTE, ID_PIEGE, DATE, nom_station, nom_com, influence,var, valeur)

## Function which aggregates weekly the different variables and putting all together
fun_summarize_week_polluant <- function(df,var_to_summarize){
  df_polluant_pieges2_summarize <- df %>%
    dplyr::filter(var==var_to_summarize) %>%
    group_by(ID_COLLECTE,lag_n = lubridate::week(DATE)) %>%
    summarise(val=mean(valeur, na.rm = T),date = min(DATE)) %>%
    group_by(ID_COLLECTE) %>%
    mutate(lag_n=seq(n()-1,0,-1)) %>%
    mutate(var = var_to_summarize) %>%
    as_tibble()
  return(df_polluant_pieges2_summarize)
}

## Putting together
df_polluants_pieges_summ <- fun_summarize_week_polluant(df_polluant_pieges_2,"NOX") %>%
  bind_rows(fun_summarize_week_polluant(df_polluant_pieges_2,"NO")) %>%
  bind_rows(fun_summarize_week_polluant(df_polluant_pieges_2,"NO2"))


#### Function to prepare the data frame for the modelling and for the cross correlation maps, putting the value for all variables selected for every time lag for every collection 
fun_ccm_df <- function(df, varr, function_to_apply){
  
  df_wide <- df %>%
    filter(var==varr) %>%
    dplyr::select(-c("date","var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr,"_"))
  
  max_col <- ncol(df_wide)
  
  for(i in 2:(max_col-1)){
    for(j in (i+1):max_col){
      column_name <- paste0(colnames(df_wide[i]),"_",(j-2))
      if(function_to_apply=="mean"){
        df_wide[column_name] <- rowMeans(df_wide[,i:j], na.rm = T)
      } 
      else if (function_to_apply=="sum"){
        df_wide[column_name] <- rowSums(df_wide[,i:j], na.rm = T)
      }
    }
  }
  
  for(i in 2:max_col){
    colnames(df_wide)[i] <- paste0(colnames(df_wide)[i],"_",sub('.*\\_', '', colnames(df_wide)[i]))
  }
  
  return(df_wide)
}
#### Applyong the function for different polluants
df_polluants_pieges_summ_wide1 <- fun_ccm_df(df_polluants_pieges_summ,"NOX","mean")
df_polluants_pieges_summ_wide2 <- fun_ccm_df(df_polluants_pieges_summ,"NO","mean")
df_polluants_pieges_summ_wide3 <- fun_ccm_df(df_polluants_pieges_summ,"NO2","mean")


#### Same treatments for O3, PM2.5 and PM10 which belong to another station

df_releves_pieges_6 <- df_releves_pieges|>
  dplyr::select(ID_PIEGE, ID_COLLECTE, DATE_COLLECTE)

df_releves_pieges_6$nom_station<-"Montpellier - Prés d Arènes Urbain"


df_polluant_3 <- data.frame(ID_COLLECTE = character(),ID_PIEGE = character(), date_debut = character(), nom_station=as.character(), stringsAsFactors = F) ## Pour chaque collecte, on met la valeur des precipitations, temperature de 1 à 42 jours avant

for(i in 1:nrow(df_releves_pieges_6)){
  for(j in 0:lag_max){
    df_polluant_3 <- rbind(df_polluant_3,
                           data.frame(ID_COLLECTE = df_releves_pieges_6$ID_COLLECTE[i],
                                      ID_PIEGE = df_releves_pieges_6$ID_PIEGE[i],
                                      date_debut = as.Date(df_releves_pieges_6$DATE_COLLECTE[i])-j,
                                      nom_station=df_releves_pieges_6$nom_station[i],
                                      lag_n = j,
                                      stringsAsFactors = F))
  }
}


df_polluant_pieges_4 <- df_polluant_3 %>% 
  left_join(polluant) %>%
  filter(nom_poll%in%c("PM2.5", "PM10", "O3"))|>
  dplyr::mutate(DATE=date_debut, var=nom_poll)|>
  dplyr::select(ID_COLLECTE, ID_PIEGE, DATE, nom_station, nom_com, influence,var, valeur)

#### To summarise the week
fun_summarize_week_polluant <- function(df,var_to_summarize){
  df_polluant_pieges2_summarize <- df %>%
    filter(var==var_to_summarize) %>%
    group_by(ID_COLLECTE,lag_n = lubridate::week(DATE)) %>%
    summarise(val=mean(valeur, na.rm = T),date = min(DATE)) %>%
    group_by(ID_COLLECTE) %>%
    mutate(lag_n=seq(n()-1,0,-1)) %>%
    mutate(var = var_to_summarize) %>%
    as_tibble()
  return(df_polluant_pieges2_summarize)
}

df_polluants_pieges_summ_2 <- fun_summarize_week_polluant(df_polluant_pieges_4,"PM2.5") %>%
  bind_rows(fun_summarize_week_polluant(df_polluant_pieges_4,"PM10")) %>%
  bind_rows(fun_summarize_week_polluant(df_polluant_pieges_4,"O3"))


#### Function to prepare the data frame for the modelling and for the cross correlation maps, putting the value for all variables selected for every time lag for every collection 
fun_ccm_df <- function(df, varr, function_to_apply){
  
  df_wide <- df %>%
    filter(var==varr) %>%
    dplyr::select(-c("date","var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr,"_"))
  
  max_col <- ncol(df_wide)
  
  for(i in 2:(max_col-1)){
    for(j in (i+1):max_col){
      column_name <- paste0(colnames(df_wide[i]),"_",(j-2))
      if(function_to_apply=="mean"){
        df_wide[column_name] <- rowMeans(df_wide[,i:j], na.rm = T)
      } 
      else if (function_to_apply=="sum"){
        df_wide[column_name] <- rowSums(df_wide[,i:j], na.rm = T)
      }
    }
  }
  
  for(i in 2:max_col){
    colnames(df_wide)[i] <- paste0(colnames(df_wide)[i],"_",sub('.*\\_', '', colnames(df_wide)[i]))
  }
  
  return(df_wide)
}

df_polluants_pieges_summ_wide4 <- fun_ccm_df(df_polluants_pieges_summ_2,"PM2.5","mean")
df_polluants_pieges_summ_wide5 <- fun_ccm_df(df_polluants_pieges_summ_2,"PM10","mean")
df_polluants_pieges_summ_wide6 <- fun_ccm_df(df_polluants_pieges_summ_2,"O3","mean")

#### To aput together all data frame with pollutant information 
df_polluants_piege_fin<-df_polluants_pieges_summ_wide1|>
  left_join(df_polluants_pieges_summ_wide2)|>
  left_join(df_polluants_pieges_summ_wide3)|>
  left_join(df_polluants_pieges_summ_wide4)|>
  left_join(df_polluants_pieges_summ_wide5)|>
  left_join(df_polluants_pieges_summ_wide6)


########################### Restructuring tables

#### Cleaning the land cover data
metrics_defs <- landscapemetrics::list_lsm() # list of landscape metrics

df_lsm_landcover_veget <- df_lsm_landcover_veget %>% ### vegetation grouped
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


#### Cleaning the socio economics data
 
df_filosofi = df_filosofi %>%
   mutate(ID_PIEGE = as.character(ID_PIEGE)) #%>%
  #dplyr::select(-c( "TYPE_PIEGE", "LATITUDE", "LONGITUDE"))
colnames(df_filosofi) <- paste0('FIL_',colnames(df_filosofi))
colnames(df_filosofi)[1] <- 'ID_PIEGE'


#### Cleaning the population density data
POP <- POP %>%
  mutate(buffer = as.numeric(buffer)) %>%
  pivot_wider(names_from = buffer, values_from = -c(buffer,ID_PIEGE), names_glue = "POP_{buffer}_{.value}") %>%#, values_fill = list(val = 0))  
  mutate(ID_PIEGE = as.character(ID_PIEGE))


#### Cleaning the table of meteo from ODEE
df_meteo_pieges_odee_summ_wide <- df_meteo_pieges_summ_wide %>%
  mutate(ID_PIEGE = sub('_[^_]*$', '', idpointdecapture)) %>%
  mutate(ID_COLLECTE = as.numeric(sub('.*\\_', '', idpointdecapture))) %>%
  left_join(df_releves_pieges2) %>%
  relocate(ID_PIEGE,ID_COLLECTE,DATE_COLLECTE, .after = idpointdecapture)

#### Cleaning the table of meteo from METEO FRANCE
df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide_meteofrance %>%
  mutate(ID_PIEGE = sub('_[^_]*$', '', idpointdecapture)) %>%
  mutate(ID_COLLECTE = as.numeric(sub('.*\\_', '', idpointdecapture))) %>%
  left_join(df_releves_pieges2) %>%
  relocate(ID_PIEGE,ID_COLLECTE,DATE_COLLECTE, .after = idpointdecapture)
  

#### Join all data to create 1 big dataset 

df_model <- df_meteo_pieges_odee_summ_wide %>%
  left_join(df_meteo_pieges_summ_wide_meteofrance) %>%
  left_join(df_releves_pieges) %>%
  left_join(pieges %>% st_drop_geometry(), by = "ID_PIEGE") %>%
  left_join(df_lsm_landcover_veget) %>%
  left_join(df_filosofi) %>%
  left_join(POP) %>%
  left_join(df_rf_during_coll) %>%
  left_join(df_microclim) %>%
  left_join(df_polluants_piege_fin) %>%
  left_join(df_pression)%>%
  relocate(NB_ALBO_F, .after = DATE_COLLECTE) %>%
  relocate(num_session, LATITUDE, LONGITUDE, .after = DATE_COLLECTE)

##### Selection of data of sampling in May, adding the zone of sampling and the site

df_model<-df_model|>
  utate(num_session=as.factor(num_session))|>
  mutate(AREA=case_when(ID_PIEGE%in%c("BG_01", "BG_02")~"Aiguelongue",ID_PIEGE%in%c("BG_03", "BG_04", "BG_05")~"Botanical Garden",  ID_PIEGE%in%c("BG_11", "BG_14")~"Lemasson", ID_PIEGE%in%c("BG_12_13")~"Soulas", ID_PIEGE%in%c("BG_15_16")~"Aiguerelles",
                        ID_PIEGE%in%c("BG_21")~"St-Charles", ID_PIEGE%in%c("BG_22")~"Bouisson Bertrand",  ID_PIEGE%in%c("BG_23") ~"Diderot", ID_PIEGE%in%c("BG_24")~"Acapulco"),
         ZONE=case_when(ID_PIEGE%in%c("BG_01", "BG_02","BG_03", "BG_04", "BG_05")~"Park", ID_PIEGE%in%c("BG_11", "BG_14", "BG_12_13", "BG_15_16")~"Residential", ID_PIEGE%in%c("BG_21","BG_22",  "BG_23", "BG_24")~"City Center"))%>%
  filter(DATE_COLLECTE>"2023-05-01")%>%
  relocate(NB_ALBO_TOT,NB_ALBO_F, NB_ALBO_M, .after = DATE_COLLECTE) %>%
  relocate(num_session, SESSION_DAY, JOUR, LATITUDE, LONGITUDE, .after = DATE_COLLECTE)

write.csv(df_model, "02_Data/processed_data/01_Adults_Abundance/df_model.csv")



