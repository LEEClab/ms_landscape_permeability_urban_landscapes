#' ---
#' title: 'Urban ecological corridors in Ouro Preto, sensibility analysis: removing STs from corridors'
#' author: Tulaci Bhakti, Bernardo Niebuhr, Joao Carlos Pena
#' date: November 2020
#' 
#' output:
#' pdf_document:
#'   toc: true
#'   toc_depth: 2
#'   number_sections: true
#' ---

# --------------- label=load_packages, warning=FALSE, message=FALSE, echo=FALSE

# Load packages
if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load('raster', 'sp', "sf", 'rgdal', 'rgeos')
install.load::install_load('ezknitr', 'knitr', 'tidyverse')
install.load::install_load('fasterize')

# Print options
options(width = 165)
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
opts_chunk$set(error = F, message = F, warning = F, cache = F, echo = T, results = F,
               fig.align = 'center')

# --------------- label=setup

# Clean everything before beginning

rm(list = ls())

# Data folder
corrdir <- 'analysis_sensibility/'
stdir <- 'input/sts'
mapdir <- 'input/shapefiles'

# Output folder
outdir <- 'analysis_sensibility/output/'

# --------------- label=load_data

#' ## Loading data
#' 
#' Load corridor rasters, vetors, and information already imported into R.
load(paste0(corrdir, 'corridors_loaded.RData'))

# species
sp.short <- c('aleuco')
sp <- 'A. leucophthalmus'

#' ## Remove ST from maps
#' 
#' ### Remove ST from RSFI maps

#' First, we check that all maps have the same extent
#' We'll standardize the extent according to the first species of no zone scenario

# no zoning
corridors_nozone_nost <- list()

for(i in 1:length(corridors_no_zone)) {
  corridors_nozone_nost[[i]] <- raster::crop(raster::extend(corridors_no_zone[[i]], 
                                                            corridors_no_zone[[1]]), 
                                             corridors_no_zone[[1]])
}
corridors_nozone_nost
names(corridors_nozone_nost) <- met_scal

# just to check that nothing changed in the map, only the extent
#raster::writeRaster(corridors_zone[[4]], 'raster_antigo.tif', overwrite = TRUE)
#raster::writeRaster(corridors_zone_nost[[4]], 'raster_novo.tif')

# zoning
corridors_zone_nost <- list()

for(i in 1:length(corridors_zone)) {
  corridors_zone_nost[[i]] <- raster::crop(raster::extend(corridors_zone[[i]], 
                                                          corridors_no_zone[[1]]), 
                                           corridors_no_zone[[1]])
}
corridors_zone_nost
names(corridors_zone_nost) <- met_scal

#' Now we'll transform do the same for the ST map.
ST.map <- raster::crop(raster::extend(ST.map, corridors_nozone_nost[[1]], value = NA), corridors_nozone_nost[[1]])
ST.map[1:10]

#' Finally we'll remove corridors information from ST patches

# no zoning
for(i in 1:length(corridors_nozone_nost)) {
  # print(i)
  corridors_nozone_nost[[i]][] <- ifelse(!is.na(ST.map[]), NA, corridors_nozone_nost[[i]][])
  # corridors_nozone_nost[[i]][] <- ifelse(ST.map[] != 0, NA, corridors_nozone_nost[[i]][])
}
corridors_nozone_nost

plot(corridors_no_zone[[1]])
plot(corridors_nozone_nost[[1]])

#' Check if values have inceased. On the left (right) we have the range (minimum, maximum) of values
#' before (after) removing STs from corridor RSFI maps.

# ----- label=print_nozone, results=TRUE
for(i in 1:length(corridors_no_zone)) {
  rb <- raster::brick(corridors_no_zone[[i]], corridors_nozone_nost[[i]])
  names(rb) <- c('with.ST', 'without.ST')
  rb %>%   
    cellStats(stat = 'range') %>% 
    print
}

# -----
# zoning
for(i in 1:length(corridors_zone_nost)) {
  # print(i)
  corridors_zone_nost[[i]][] <- ifelse(!is.na(ST.map[]), NA, corridors_zone_nost[[i]][])
  # corridors_zone_nost[[i]][] <- ifelse(ST.map[] != 0, NA, corridors_zone_nost[[i]][])
}
corridors_zone_nost

plot(corridors_zone[[1]])
plot(corridors_zone_nost[[1]])

#' Check if values have inceased. On the left (right) we have the range (minimum, maximum) of values
#' before (after) removing STs from corridor RSFI maps.

# ----- label=print_zone, results=TRUE
for(i in 1:length(corridors_zone)) {
  rb <- raster::crop(raster::extend(corridors_zone[[i]], corridors_no_zone[[1]]), 
                     corridors_no_zone[[1]]) %>%
    raster::brick(corridors_zone_nost[[i]])
  names(rb) <- c('with.ST', 'without.ST')
  rb %>% 
    cellStats(stat = 'range') %>% 
    print
}

#' ## Write folder

# Create new folder, if it does not exist
newdir <- paste0(corrdir, 'corredores_sem_st/')
dir.create(newdir, showWarnings = FALSE)

for(i in 1:length(corridors_no_zone)) {
  # no zoning
  corridors_nozone_nost[[i]] %>% 
    raster::writeRaster(paste0(newdir, 'corridors_RSFI_nozoning_', met_scal[i], '.tif'), 
                        overwrite = TRUE)
  
  # zoning
  corridors_zone_nost[[i]] %>% 
    raster::writeRaster(paste0(newdir, 'corridors_RSFI_zoning_', met_scal[i], '.tif'), 
                        overwrite = TRUE)
}

#' ## Remove STs from vector corridors

# load ST vector
ST.pol <- sf::st_read(paste0(mapdir, 'sources_targets.shp'))

ST.pol <- ST.pol %>% 
  dplyr::group_by(gridcode) %>% 
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

plot(ST.pol)

# one example
# select ID polygons
(ids <- corr.shp.sts.sem.zona[[1]][[5]])
(st.ids <- ST.pol[ST.pol$gridcode == ids[1] | ST.pol$gridcode == ids[2],])

# remove area within STs
corr.cut <- sf::st_difference(corr.shp.sem.zona[[1]][[5]], 
                              st_union(st.ids))

# initial and final positions
pos.init <- st_line_sample(corr.cut, sample = 0)
pos.fin <- st_line_sample(corr.cut, sample = 1)

# Euclidean distance
(ED <- sf::st_distance(pos.init, pos.fin) %>%
    units::set_units(value = "m") %>% 
    as.numeric())

# calculate total corridor length
(Tot.len <- sf::st_length(corr.cut) %>% 
    units::set_units(value = "m") %>% 
    as.numeric())

# Plot to check
ggplot() +
  geom_sf(data = st.ids, aes(fill = gridcode)) +
  geom_sf(data = corr.shp.sem.zona[[1]][[5]]) +
  geom_sf(data = corr.cut, col = 2) +
  geom_sf(data = pos.init, col = 2) +
  geom_sf(data = pos.fin, col = 2)

# Loop for all species

# No zoning
method
scale
met_scal
(tab.sem.zona <- data.frame(sp = NA, scenario = NA, method_scale = NA,
                            source = NA, target = NA, sim = NA,
                            tot.dist = NA, euc.dist = NA, cost = NA)[-1,])
scen <- 'LandC'

# for each method
corr.shp.sem.zona.nost <- list()
names(corr.shp.sem.zona)
for(i in 1:length(corr.shp.sem.zona)) {
  
  corr.shp.tmp.nost <- list()
  
  # for each simulation
  for(j in 1:length(corr.shp.sem.zona[[i]])) {
    
    # select ID polygons
    (ids <- corr.shp.sts.sem.zona[[i]][[j]])
    (st.ids <- ST.pol[ST.pol$gridcode == ids[1] | ST.pol$gridcode == ids[2],])
    
    # remove area within STs
    corr.cut <- sf::st_difference(st_union(corr.shp.sem.zona[[i]][[j]]), 
                                  st_union(st.ids))
    
    if(inherits(corr.cut, "sfc_MULTILINESTRING")) {
      corr.cut <- corr.cut %>% 
        sf::st_line_merge() %>% 
        sf::st_cast(to = "LINESTRING")
    }
    
    # get initial and end coordinates
    pos.init <- st_line_sample(corr.cut, sample = 0) %>% 
      sf::st_cast(to = "POINT")
    if(length(pos.init) > 1) {
      fst <- 1
      lst <- 2
      # check the points which are around the source patch
      close.source <- which(st_distance(pos.init, st.ids[st.ids$gridcode == ids[fst],]) < units::set_units(100, "m"))
      if(length(close.source) == 0) {
        fst <- 2
        lst <- 1
        close.source <- which(st_distance(pos.init, st.ids[st.ids$gridcode == ids[fst],]) < units::set_units(100, "m"))
      }
      pos.init <- pos.init[close.source,]
      # if there are still two points, get the one closes to the target patch
      if(length(pos.init) > 1) {
        pos.init <- pos.init[which.min(st_distance(pos.init, st.ids[st.ids$gridcode == ids[lst],])),]
      }
    }
      
    pos.fin <- st_line_sample(corr.cut, sample = 1) %>% 
      sf::st_cast(to = "POINT")
    if(length(pos.fin) > 1) {
      # check the points which are around the target patch
      close.target <- which(st_distance(pos.fin, st.ids[st.ids$gridcode == ids[lst],]) < units::set_units(100, "m"))
      if(length(close.source) == 0) {
        temp <- fst
        fst <- lst
        lst <- temp
        close.target <- which(st_distance(pos.fin, st.ids[st.ids$gridcode == ids[lst],]) < units::set_units(100, "m"))
      }
      pos.fin <- pos.fin[close.target,]
      # if there are still two points, get the one closes to the source patch
      if(length(pos.fin) > 1) {
        pos.fin <- pos.fin[which.min(st_distance(pos.fin, st.ids[st.ids$gridcode == ids[fst],])),]
      }
    }
    
    # Euclidean distance
    (ED <- sf::st_distance(pos.init, pos.fin) %>%
        units::set_units(value = "m") %>% 
        as.numeric())
    if(length(ED) > 1) ED <- max(ED, na.rm = T)
    if(length(ED) == 0) ED <- NA
    
    # calculate total corridor length
    (Tot.len <- sf::st_length(corr.cut) %>% 
        units::set_units(value = "m") %>% 
        as.numeric())
    if(length(Tot.len) > 1) Tot.len <- max(Tot.len, na.rm = T)
    
    # Plot to check
    ggplot() +
      geom_sf(data = st.ids, aes(fill = gridcode)) +
      # geom_sf(data = ST.pol) +
      geom_sf(data = corr.shp.sem.zona[[i]][[j]]) +
      geom_sf(data = corr.cut, col = 2) +
      geom_sf(data = pos.init, col = 2) +
      geom_sf(data = pos.fin, col = 2)
    
    # attach info
    tab.sem.zona <- rbind(tab.sem.zona,
                          data.frame(sp = sp, scenario = scen, 
                                     method_scale = names(corr.shp.sem.zona)[i], 
                                     source = ids[1], target = ids[2], 
                                     sim = j, tot.dist = Tot.len, 
                                     euc.dist = ED, cost = NA))
    
    # save cut corridors
    corr.shp.tmp.nost[[j]] <- corr.cut
    
    # print
    print(paste(i, j, ids[1], ids[2]))
  }
  
  corr.shp.sem.zona.nost[[i]] <- corr.shp.tmp.nost
  
}
corr.shp.sem.zona.nost

# transform table into tibble
tab.sem.zona <- tab.sem.zona %>% 
  dplyr::as_tibble()

# if Euclidean distance > Total distance, there was some error, so put NA
tab.sem.zona$euc.dist[which(tab.sem.zona$euc.dist > tab.sem.zona$tot.dist)] <- NA

#---------

# Zoning
method
scale
met_scal
(tab.com.zona <- data.frame(sp = NA, scenario = NA, method_scale = NA,
                            source = NA, target = NA, sim = NA,
                            tot.dist = NA, euc.dist = NA, cost = NA)[-1,])
scen <- 'LandC+UrbZ'

# for each method
corr.shp.com.zona.nost <- list()
for(i in 1:length(corr.shp.com.zona)) {
  
  corr.shp.tmp.nost <- list()
  
  # for each simulation
  for(j in 1:length(corr.shp.com.zona[[i]])) {
    
    # select ID polygons
    (ids <- corr.shp.sts.com.zona[[i]][[j]])
    (st.ids <- ST.pol[ST.pol$gridcode == ids[1] | ST.pol$gridcode == ids[2],])
    
    # remove area within STs
    corr.cut <- sf::st_difference(st_union(corr.shp.com.zona[[i]][[j]]), 
                                  st_union(st.ids))
    
    if(inherits(corr.cut, "sfc_MULTILINESTRING")) {
      corr.cut <- corr.cut %>% 
        sf::st_line_merge() %>% 
        sf::st_cast(to = "LINESTRING")
    }
    
    # get initial and end coordinates
    pos.init <- st_line_sample(corr.cut, sample = 0) %>% 
      sf::st_cast(to = "POINT")
    if(length(pos.init) > 1) {
      fst <- 1
      lst <- 2
      # check the points which are around the source patch
      close.source <- which(st_distance(pos.init, st.ids[st.ids$gridcode == ids[fst],]) < units::set_units(100, "m"))
      if(length(close.source) == 0) {
        fst <- 2
        lst <- 1
        close.source <- which(st_distance(pos.init, st.ids[st.ids$gridcode == ids[fst],]) < units::set_units(100, "m"))
      }
      pos.init <- pos.init[close.source,]
      # if there are still two points, get the one closes to the target patch
      if(length(pos.init) > 1) {
        pos.init <- pos.init[which.min(st_distance(pos.init, st.ids[st.ids$gridcode == ids[lst],])),]
      }
    }
    
    pos.fin <- st_line_sample(corr.cut, sample = 1) %>% 
      sf::st_cast(to = "POINT")
    if(length(pos.fin) > 1) {
      # check the points which are around the target patch
      close.target <- which(st_distance(pos.fin, st.ids[st.ids$gridcode == ids[lst],]) < units::set_units(100, "m"))
      if(length(close.source) == 0) {
        temp <- fst
        fst <- lst
        lst <- temp
        close.target <- which(st_distance(pos.fin, st.ids[st.ids$gridcode == ids[lst],]) < units::set_units(100, "m"))
      }
      pos.fin <- pos.fin[close.target,]
      # if there are still two points, get the one closes to the source patch
      if(length(pos.fin) > 1) {
        pos.fin <- pos.fin[which.min(st_distance(pos.fin, st.ids[st.ids$gridcode == ids[fst],])),]
      }
    }
    
    # Euclidean distance
    (ED <- sf::st_distance(pos.init, pos.fin) %>%
        units::set_units(value = "m") %>% 
        as.numeric())
    if(length(ED) > 1) ED <- max(ED, na.rm = T)
    if(length(ED) == 0) ED <- NA
    
    # calculate total corridor length
    (Tot.len <- sf::st_length(corr.cut) %>% 
        units::set_units(value = "m") %>% 
        as.numeric())
    if(length(Tot.len) > 1) Tot.len <- max(Tot.len, na.rm = T)
    
    # Plot to check
    ggplot() +
      geom_sf(data = st.ids, aes(fill = gridcode)) +
      # geom_sf(data = ST.pol) +
      geom_sf(data = corr.shp.com.zona[[i]][[j]]) +
      geom_sf(data = corr.cut, col = 2) +
      geom_sf(data = pos.init, col = 2) +
      geom_sf(data = pos.fin, col = 2)
    
    # attach info
    tab.com.zona <- rbind(tab.com.zona,
                          data.frame(sp = sp, scenario = scen, 
                                     method_scale = names(corr.shp.com.zona)[i], 
                                     source = ids[1], target = ids[2], 
                                     sim = j, tot.dist = Tot.len, 
                                     euc.dist = ED, cost = NA))
    
    # save cut corridors
    corr.shp.tmp.nost[[j]] <- corr.cut
    
    # print
    print(paste(i, j, ids[1], ids[2]))
  }
  
  corr.shp.com.zona.nost[[i]] <- corr.shp.tmp.nost
  
}
corr.shp.com.zona.nost

# transform table into tibble
tab.com.zona <- tab.com.zona %>% 
  dplyr::as_tibble()

# if Euclidean distance > Total distance, there was some error, so put NA
tab.com.zona$euc.dist[which(tab.com.zona$euc.dist > tab.com.zona$tot.dist)] <- NA


#----

#' ## Save corridors loaded without STs
save.image(paste0(corrdir, 'corridors_noST.RData'))
# load(paste0(corrdir, 'corridors_noST.RData'))

# rasterize to extract resistance values
res_map <- ceiling(res(ST.map)[1]/2)

# too slow
corr.rast <- corr.shp.sem.zona.nost[[1]][[1]] %>% 
  sf::st_buffer(dist = res_map) %>% 
  as("Spatial") %>% 
  raster::rasterize(ST.map)
corr.rast

# 1m buffer to transform into a polygon, then rasterize
# rast <- ST.map
# rast[] <- 1
# 
# corr.rast <- corr.shp.sem.zona.nost[[1]][[1]] %>% 
#   sf::st_buffer(dist = 5) %>%
#   sf::st_union() %>% 
#   fasterize::fasterize(raster = rast)

# raster::overlay(raster::brick(corr.rast, resistance_nozone[[1]]), function(a,b) a*b)
# corr.rast*resistance_nozone[[1]]
corr.vals <- ifelse(!is.na(corr.rast[]), resistance_nozone[[1]][], NA)
sum(corr.vals, na.rm = T)
cellStats(corr.rast*resistance_nozone[[1]], sum, na.rm = T)

#' ### Extract cost values from corridors outside STs, without zoning

# for each method
res_map <- ceiling(res(ST.map)[1]/2)
row.counter <- 1
for(i in 1:length(corr.shp.sem.zona.nost)) {
  
  # for each simulation
  for(j in 1:length(corr.shp.sem.zona.nost[[i]])) {
    
    print(row.counter, i, j)
    corr.rast <- corr.shp.sem.zona.nost[[i]][[j]] %>% 
      sf::st_buffer(dist = res_map) %>% 
      as("Spatial") %>% 
      raster::rasterize(ST.map)
    # corr.rast <- sf::st_as_sf(corr.shp.sem.zona.nost[[i]][[j]]) %>% 
    #   sf::st_buffer(dist = 1) %>%
    #   fasterize::fasterize(raster = ST.map)
    
    cost <- cellStats(corr.rast*resistance_nozone, sum, na.rm = T)
    
    tab.sem.zona$cost[row.counter] <- cost
    row.counter <- row.counter+1
  }
}
tab.sem.zona

#' ### Extract cost values from corridors outside STs, with zoning

# for each species
row.counter <- 1
for(i in 1:length(corr.shp.com.zona.nost)) {
  
  # for each simulation
  for(j in 1:length(corr.shp.com.zona.nost[[i]])) {
    
    print(row.counter, i, j)
    corr.rast <- corr.shp.com.zona.nost[[i]][[j]] %>% 
      sf::st_buffer(dist = res_map) %>% 
      as("Spatial") %>% 
      raster::rasterize(ST.map)
    # corr.rast <- sf::st_as_sf(corr.shp.com.zona.nost[[i]][[j]]) %>% 
    #   sf::st_buffer(dist = 1) %>%
    #   fasterize::fasterize(raster = ST.map)
    
    cost <- cellStats(corr.rast*resistance_zone, sum, na.rm = T)
    
    tab.com.zona$cost[row.counter] <- cost
    row.counter <- row.counter+1
  }
}
tab.com.zona

#' ## Save corridors loaded and cut by ST

save.image(paste0(corrdir, 'corridors_loaded_noST.RData'))

