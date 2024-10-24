## Load packages

library(readxl)
library(tidyverse)
library(lubridate)
library(ggmap)
library(patchwork)
library(ggdensity)
library(tidyr)
#library(ggsn)
library(terra)
library(sp)
library(patchwork)
library(sf)
library(stars)
library(gstat)
library(sfhotspot)
library(purrr)

pieges <- st_read("localisation_piege_provisoire.gpkg") %>% 
  filter(TYPE_PIEGE == 'bg-sentinel') %>%
  st_drop_geometry() %>%
  mutate(ID_PIEGE = case_when(ID_PIEGE %in% c("BG_12","BG_13") ~ "BG_12_13",
                              ID_PIEGE %in% c("BG_15","BG_16") ~ "BG_15_16",
                              TRUE ~ ID_PIEGE))

df_releves_pieges_raw <- read.csv("BG_labo2.csv", stringsAsFactors = F, sep = ";") %>%
  left_join(pieges) %>%
  mutate(HEURE_COLLECTE = ifelse(is.na(HEURE_COLLECTE),"10:00",HEURE_COLLECTE)) %>%
  rename(ID_COLLECTE_STRING = ID_COLLECTE) %>%
  mutate(DATE_POSE = parse_date(DATE_POSE,"%d/%m/%Y")) %>%
  mutate(DATE_COLLECTE = parse_date(DATE_COLLECTE,"%d/%m/%Y")) %>%
  mutate(ID_PIEGE = case_when(ID_PIEGE %in% c("BG_12","BG_13") ~ "BG_12_13",
                              ID_PIEGE %in% c("BG_15","BG_16") ~ "BG_15_16",
                              TRUE ~ ID_PIEGE))


df_releves_pieges <- df_releves_pieges_raw %>%
  group_by(ID_PIEGE, DATE_POSE, DATE_COLLECTE) %>%
  summarise(NB_ALBO_TOT = round(mean(NB_ALBO_TOT))) %>%
  as_tibble() %>%
  mutate(ID_COLLECTE = seq(1,nrow(.),1)) %>%
  arrange(DATE_COLLECTE) %>%
  filter(!is.na(DATE_COLLECTE))

df_releves_pieges$consecutive <- c(NA,diff(as.Date(df_releves_pieges$DATE_COLLECTE)) %in% c(0,1))
df_releves_pieges$consecutive[1] = FALSE
df_releves_pieges$num_session = cumsum(df_releves_pieges$consecutive==FALSE)
df_releves_pieges$consecutive=NULL

df <- df_releves_pieges %>%
  left_join(df_releves_pieges_raw %>% dplyr::select(ID_PIEGE, DATE_POSE, DATE_COLLECTE, HEURE_COLLECTE)) %>%
  group_by_at(vars(-HEURE_COLLECTE)) %>%
  filter(row_number()==1) %>%
  arrange(ID_COLLECTE) %>%
  as_tibble() %>%
  group_by(ID_PIEGE,DATE_COLLECTE,num_session) %>%
  summarise(NB_ALBO_TOT = sum(NB_ALBO_TOT)) %>%
  left_join(pieges) %>%
  ungroup() %>%
  rename(Site = lieu, Longitude = LONGITUDE, Latitude = LATITUDE) %>%
  mutate(Mois = as.character(lubridate::month(DATE_COLLECTE, label = TRUE))) %>%
  mutate(Mois =  fct_relevel(Mois, c("janv","févr","mars","avril","mai","juin","juil","août","sept","oct","nov","déc"))) %>%
  mutate(Year = year(DATE_COLLECTE)) %>%
  mutate(Year = factor(Year, levels = c("2023", "2024"))) %>%
  filter(!is.na(Latitude))



df_sf <- df %>%
  st_as_sf( crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))


bbox_aiguelongue <- c(left = min(df$Longitude[which(df$Site == "Aiguelongue")])-0.001, bottom = min(df$Latitude[which(df$Site == "Aiguelongue")])-0.001, right = max(df$Longitude[which(df$Site == "Aiguelongue")])+0.001, top = max(df$Latitude[which(df$Site == "Aiguelongue")])+0.001)
bbox_perols <- c(left = min(df$Longitude[which(df$Site == "PEROLS")])-0.002, bottom = min(df$Latitude[which(df$Site == "PEROLS")])-0.002, right = max(df$Longitude[which(df$Site == "PEROLS")])+0.002, top = max(df$Latitude[which(df$Site == "PEROLS")])+0.002)
bbox_stmedard <- c(left = min(df$Longitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])-0.002, bottom = min(df$Latitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])-0.002, right = max(df$Longitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])+0.002, top = max(df$Latitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])+0.002)
bbox_murviel <- c(left = min(df$Longitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])-0.002, bottom = min(df$Latitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])-0.002, right = max(df$Longitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])+0.002, top = max(df$Latitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])+0.002)
bbox_montpellier <- c(left = min(df$Longitude[which(df$Site == "MONTPELLIER")])-0.002, bottom = min(df$Latitude[which(df$Site == "MONTPELLIER")])-0.002, right = max(df$Longitude[which(df$Site == "MONTPELLIER")])+0.002, top = max(df$Latitude[which(df$Site == "MONTPELLIER")])+0.002)
bbox_allsites <- c(left = min(df$Longitude)-1, bottom = min(df$Latitude)-1, right = max(df$Longitude)+1, top = max(df$Latitude)+1)

ggmap::register_stadiamaps("46e02592-4347-4401-95bb-66873ec3a35b", write = FALSE)

map_bayonne <- get_stadiamap(bbox_bayonne, maptype = "stamen_terrain", zoom = 16)
map_perols <- get_stadiamap(bbox_perols, maptype = "stamen_terrain", zoom = 16)
map_stmedard <- get_stadiamap(bbox_stmedard, maptype = "stamen_terrain", zoom = 16)
map_murviel <- get_stadiamap(bbox_murviel, maptype = "stamen_terrain", zoom = 16)
map_montpellier <- get_stadiamap(bbox_montpellier, maptype = "stamen_terrain", zoom = 16)
map_allsites <- get_stadiamap(bbox_allsites, maptype = "stamen_terrain", zoom = 8)



fun_get_map <- function(df, site, temporal_grouping_column, map_type, spat_res = 1/2220){
  
  # Define a function to fix the bbox to be in EPSG:3857
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")),
                         c("ymin", "xmin", "ymax", "xmax"))
    
    # Coonvert the bbox to an sf polygon, transform it to 3857,
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
    
    # Overwrite the bbox of the ggmap object with the transformed coordinates
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }
  
  # define theme
  my_theme <- function() {
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_rect(fill = "transparent"), # necessary to avoid drawing panel outline
      panel.border = element_rect(colour = "grey", fill=NA, linewidth=1),
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank()
    )
  }
  
  
  if(site == "BAYONNE"){
    
    map = map_bayonne
    df <- df %>% filter(Site == "BAYONNE")
    
  } else if (site == "PEROLS"){
    
    map = map_perols
    df <- df %>% filter(Site == "PEROLS")
    
  } else if (site == "SAINT-MEDARD-EN-JALLES"){
    
    map = map_stmedard
    df <- df %>% filter(Site == "SAINT-MEDARD-EN-JALLES")
    
  } else if (site == "MURVIEL-LES-MONTPELLIER"){
    
    map = map_murviel
    df <- df %>% filter(Site == "MURVIEL-LES-MONTPELLIER")
    
  } else if (site == "MONTPELLIER"){
    
    map = map_montpellier
    df <- df %>% filter(Site == "MONTPELLIER")
    
  } else if (site == "all"){
    map = map_allsites
  }
  
  
  if(map_type=="heatmap"){
    
    # df2 <- tidyr::uncount(df, NB_ALBO_TOT)
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), NB_ALBO_TOT_mn = trunc(mean(NB_ALBO_TOT, na.rm = T)), NB_ALBO_TOT_sd = sd(NB_ALBO_TOT, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
      tidyr::uncount(NB_ALBO_TOT_mn)
    
    
    final_map <- ggmap(map) +
      geom_hdr(aes(Longitude, Latitude, fill = after_stat(probs)), data = df2, alpha = .5, method = "kde") +
      geom_hdr_lines(aes(Longitude, Latitude), data = df2, method = "kde", linewidth = 0.2, show.legend=F) +
      scale_fill_brewer(name="Egg density", palette = "YlOrRd", labels=c("Lowest","","","Highest")) +
      {if(temporal_grouping_column=="Year")geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02)}+
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",1,1)) +
      my_theme() +
      theme(legend.position="none") +
      ggtitle(paste0(site," - by ", temporal_grouping_column))
    
  }
  
  if(map_type=="idw"){
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), NB_ALBO_TOT_mn = mean(NB_ALBO_TOT, na.rm = T), NB_ALBO_TOT_sd = sd(NB_ALBO_TOT, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))
    
    colnames(df2)[1] <- temporal_grouping_column
    
    unique_times <- unique(df[,temporal_grouping_column])[[1]]
    
    df_sf <- st_as_sf(df2, crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))
    
    grd <- st_bbox(df_sf) %>%
      st_as_stars(dx = spat_res)
    
    grd2 <- st_simplify(st_buffer(st_convex_hull(st_union(st_geometry(df_sf))), dist = 15000), dTolerance = 5000)
    
    maps <- list()
    
    for(i in 1:length(unique_times)){
      
      df_sf2 <- df_sf %>% filter(!!sym(temporal_grouping_column) == unique_times[i])
      j <- idw(NB_ALBO_TOT_mn~1, df_sf2,grd)
      
      rext <- st_bbox(j)
      
      r <- raster(t(j[[1]]), xmn = rext[1], xmx = rext[3],
                  ymn = rext[2], ymx=rext[4],
                  crs = st_crs(i)$proj4string)
      
      r = mask(r, as(grd2, "Spatial"))
      
      r <-  as.data.frame(r, xy=TRUE) %>%
        filter(!is.na(layer))
      
      th_map<- ggmap(map) +
        geom_tile(data = r,  aes(x = x, y = y, fill = layer),  alpha = 0.8, size = 0.02) +
        scale_fill_gradient(low="lightyellow", high="red", limits = c(0,max(df2$NB_ALBO_TOT_mn))) +
        geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02) +
        ggtitle(unique_times[i])
      
      maps[[i]] <- th_map
    }
    
    final_map <- wrap_plots(maps) +  plot_layout(nrow = 1, guides = "collect")
    
  }
  
  
  if(map_type=="rasterized"){
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), NB_ALBO_TOT_mn = mean(NB_ALBO_TOT, na.rm = T), NB_ALBO_TOT_sd = sd(NB_ALBO_TOT, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))
    
    colnames(df2)[1] <- temporal_grouping_column
    
    unique_times <- unique(df[,temporal_grouping_column])[[1]]
    
    for(i in 1:length(unique_times)){
      
      df_th_year <- df2 %>% filter(!!sym(temporal_grouping_column) == unique_times[i])
      df_th_year_sp <- SpatialPointsDataFrame(df_th_year[,c("Longitude","Latitude")], df_th_year,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
      
      r <- raster(ext = extent(df_th_year_sp), crs = crs(df_th_year_sp), res = spat_res)
      r <- extend(r, c(1,1))
      r <- raster::rasterize(df_th_year_sp, r, "NB_ALBO_TOT_mn", fun = mean)
      
      r <-  as.data.frame(r, xy=TRUE) %>%
        filter(!is.na(layer))
      
      th_map<- ggmap(map) +
        geom_tile(data = r,  aes(x = x, y = y, fill = layer),  alpha = 0.8, size = 0.02) +
        scale_fill_gradient(low="lightyellow", high="red", limits = c(0,max(df2$NB_ALBO_TOT_mn))) +
        geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02) +
        ggtitle(unique_times[i])
      
      
      maps[[i]] <- th_map
      
    }
    
    final_map <- wrap_plots(maps) +  plot_layout(nrow = 1, guides = "collect")
    
  }
  
  if(map_type == "points"){
    
    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), NB_ALBO_TOT_mn = mean(NB_ALBO_TOT, na.rm = T), NB_ALBO_TOT_sd = sd(NB_ALBO_TOT, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))
    
    final_map <- ggmap(map) +
      #{if(temporal_grouping_column=="Year")geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02)}+
      geom_point(aes(x = Longitude, y = Latitude), data = df2, size = 0.5, color = "black") +
      geom_point(aes(x = Longitude, y = Latitude, size = NB_ALBO_TOT_mn), data = df2 %>% filter(NB_ALBO_TOT_mn>0), colour = "darkred", alpha = 0.7) +
      scale_size_continuous(breaks = c(10,20,50,100,200), limits = c(0,200), range = c(1,10), name="Mean egg count / trap / day", labels=c("1-10","10-20","20-50","50-100",">100")) +
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",1,1)) +
      ggtitle(paste0(site," - by ", temporal_grouping_column)) +
      my_theme()
    #+ theme(legend.position="none")
    
    
  }
  
  if(map_type=="hexagrid"){
    
    # Use the function:
    map <- ggmap_bbox(map)
    
    df_sf <- st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
      st_transform(3857)
    
    g = st_make_grid(df_sf , square=FALSE, cellsize = 150)
    nc2 = st_sf(geom=g)
    nc2$ID=1:length(g)
    
    a= nc2 %>%
      st_join(df_sf, join = st_intersects,left = TRUE) %>%
      filter(!is.na(Year)) %>%
      group_by(ID,!!sym(temporal_grouping_column)) %>%
      summarise(NB_ALBO_TOT_mn = mean(NB_ALBO_TOT, na.rm = T))
    
    
    final_map <- ggmap(map) +
      coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
      geom_sf(data = a, aes(fill = NB_ALBO_TOT_mn), inherit.aes = FALSE,alpha = .7,) +
      scale_fill_gradient(low="lightyellow", high="red", trans = "sqrt") +
      {if(temporal_grouping_column=="Year")geom_sf(data = df_sf %>% st_transform(3857), color = "red", size = 0.01, inherit.aes = FALSE)}+
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",2,1)) +
      ggtitle(paste0(site," - by ", temporal_grouping_column," - ", map_type)) +
      my_theme()
    
    
  }
  
  if (map_type=="hotspots_eachyear"){
    
    map <- ggmap_bbox(map)
    
    unique_times <- unique(df[,temporal_grouping_column])[[1]]
    
    if(temporal_grouping_column=="Mois"){
      unique_times <- c("janv","févr","mars","avril","mai","juin","juil","août","sept","oct","nov","déc")
    }
    if(temporal_grouping_column=="saison"){
      unique_times <- c("Winter","Summer")
    }
    
    df_sf <- st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))
    
    maps <- list()
    hotspots <- list()
    dfs_sf <- list()
    
    for(i in 1:length(unique_times)){
      
      df_th_year <- df %>%
        filter(!!sym(temporal_grouping_column) == unique_times[i]) %>%
        group_by(NumPP) %>%
        summarise(nb_rec = n(), NB_ALBO_TOT_mn = trunc(mean(NB_ALBO_TOT, na.rm = T)), NB_ALBO_TOT_sd = sd(NB_ALBO_TOT, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))
      
      
      th_df_sf <- st_as_sf(df_th_year, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
        st_transform(32740)
      
      th_hotspot <- hotspot_gistar(th_df_sf, weights = NB_ALBO_TOT_mn, cell_size = 150, grid_type = "hex")
      
      th_hotspot <- th_hotspot %>%
        filter(gistar > 0)
      
      #th_hotspot <- ms_simplify(th_hotspot, keep = 0.1,keep_shapes = FALSE)
      
      
      hotspots[[i]] <- th_hotspot
      dfs_sf[[i]] <- th_df_sf
      
    }
    
    max_kde <- do.call("rbind", hotspots)
    max_kde <- max(max_kde$kde)
    
    for(i in 1:length(unique_times)){
      
      th_map <- ggmap(map) +
        coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
        geom_sf(data = hotspots[[i]] %>% st_transform(3857), mapping = aes(fill = kde), inherit.aes = FALSE, lwd = 0,alpha = .8) +
        scale_fill_gradient(low="lightyellow", high="red",limits = c(0,max_kde), name = "Egg density",  breaks=seq(0, max_kde, length.out = 4), labels = c("Lowest","","","Higest")) +
        {if(temporal_grouping_column=="Year")geom_sf(data = dfs_sf[[i]] %>% st_transform(3857), color = "red", size = 0.05, inherit.aes = FALSE)}+
        my_theme() +
        labs(subtitle = unique_times[i])
      
      
      maps[[i]] <- th_map
    }
    
    final_map <- wrap_plots(maps) +
      plot_layout(nrow = ifelse(temporal_grouping_column=="Mois",2,1), guides = "collect") +
      plot_annotation(title = paste0(site," - hotspots - ",temporal_grouping_column))
    
    
  }
  
  if(map_type == "hotspots_change"){
    
    map <- ggmap_bbox(map)
    
    # df_sf <- df %>%
    #   tidyr::uncount(NB_ALBO_TOT) %>%
    #   st_as_sf(crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
    #   st_transform(32740)
    
    df_sf <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), NB_ALBO_TOT_mn = trunc(mean(NB_ALBO_TOT, na.rm = T)), NB_ALBO_TOT_sd = sd(NB_ALBO_TOT, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude), daterec = mean(daterec)) %>%
      mutate(daterec = round(daterec,units="year")) %>%
      tidyr::uncount(NB_ALBO_TOT_mn) %>%
      st_as_sf(crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
      st_transform(32740)
    
    
    cl = hotspot_classify(
      df_sf,
      cell_size = 150,
      time = "daterec",
      period = "1 year",
      grid_type = "rect"
    )
    
    cl <- cl %>%
      filter(hotspot_category!="no pattern") %>%
      st_transform(3857)
    
    final_map <- ggmap(map) +
      coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
      geom_sf(data = cl, aes(fill = hotspot_category), inherit.aes = FALSE) +
      my_theme() +
      geom_sf(data = st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>% st_transform(3857), color = "red", size = 0.1, inherit.aes = FALSE) +
      ggtitle(paste0(site, " - Evolution des hotspots entre ", min(df$Year), " et ",max(df$Year)))
    
  }
  
  return(final_map)
}



Sites <- data.frame(site = unique(df$Site))

# points - par année et par mois
pl1 <- Sites %>%
  mutate(pl_year_points = purrr::map(.$site,~fun_get_map(df,.,"Year","points"))) %>%
  mutate(pl_month_points = purrr::map(.$site,~fun_get_map(df,.,"Mois","points"))) 

purrr::map2(pl1$site, pl1$pl_year_points, ~ggsave(paste0("plots/points_year_",.x,".png"),.y, width = 20, height = 5, units = 'in'))
purrr::map2(pl1$site, pl1$pl_month_points, ~ggsave(paste0("plots/points_month_",.x,".png"),.y, width = 20, height = 5, units = 'in'))

# hexagrid - par année et par mois
pl2 <- Sites %>%
  mutate(pl_year_hexagrid = purrr::map(.$site,~fun_get_map(df,.,"Year","hexagrid"))) %>%
  mutate(pl_month_hexagrid = purrr::map(.$site,~fun_get_map(df,.,"Mois","hexagrid")))


purrr::map2(pl2$site, pl2$pl_year_hexagrid, ~ggsave(paste0("plots/hexagrid_year_",.x,".png"),.y, width = 20, height = 5, units = 'in'))
purrr::map2(pl2$site, pl2$pl_month_hexagrid, ~ggsave(paste0("plots/hexagrid_month_",.x,".png"),.y, width = 20, height = 5, units = 'in'))

# heatmap - par année et par mois
pl5 <- Sites %>%
  mutate(pl_year_heatmap = purrr::map(.$site,~fun_get_map(df,.,"Year","heatmap"))) %>%
  mutate(pl_month_heatmap = purrr::map(.$site,~fun_get_map(df,.,"Mois","heatmap")))

purrr::map2(pl5$site, pl5$pl_year_heatmap, ~ggsave(paste0("plots/heatmap_year_col2",.x,".png"),.y, width = 20, height = 5, units = 'in'))
purrr::map2(pl5$site, pl5$pl_month_heatmap, ~ggsave(paste0("plots/heatmap_month_col2",.x,".png"),.y, width = 20, height = 5, units = 'in'))


