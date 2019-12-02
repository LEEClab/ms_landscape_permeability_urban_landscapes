#' ---
#' title: 'Urban ecological corridors in Ouro Preto: removing STs from corridors'
#' author: 
#' - Tulaci Bhakti, Bernardo Niebuhr, Joao Carlos Pena
#' date: August 2019
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
install.load::install_load('raster', 'sp', 'rgdal', 'rgeos')
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
corrdir <- 'simulated_corridors/results_corridors_rescaled/'
stdir <- 'input/sts'
mapdir <- 'input/shapefiles'

# Output folder
outdir <- 'output/'

# --------------- label=load_data

#' ## Loading data
#' 
#' Load corridor rasters, vetors, and information already imported into R.
load(paste0(corrdir, 'corridors_loaded.RData'))

# species
sp.short <- c('aleuco', 'ccaudata', 'pleuco', 'sscans', 'xfuscus')
sp <- c('A. leucophthalmus', 'C. caudata', 'P. leucoptera', 'S. scansor', 'X. fuscus')

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
names(corridors_nozone_nost) <- sp

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
names(corridors_zone_nost) <- sp

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
newdir <- paste0('simulated_corridors/corredores_sem_st/')
dir.create(newdir, showWarnings = FALSE)

for(i in 1:length(corridors_no_zone)) {
  # no zoning
  corridors_nozone_nost[[i]] %>% 
    raster::writeRaster(paste0(newdir, 'corridors_RSFI_nozoning_', sp.short[i], '.tif'), 
                      overwrite = TRUE)
  
  # zoning
  corridors_zone_nost[[i]] %>% 
    raster::writeRaster(paste0(newdir, 'corridors_RSFI_zoning_', sp.short[i], '.tif'), 
                        overwrite = TRUE)
}

#' ## Remove STs from vector corridors

# load ST vector
ST.pol <- rgdal::readOGR(mapdir, 'sources_targets')

# one example
# select ID polygons
(ids <- corr.shp.sts.sem.zona[[1]][[5]])
(st.ids <- ST.pol[ST.pol$gridcode == ids[1] | ST.pol$gridcode == ids[2],])

# remove area within STs
corr.cut <- rgeos::gDifference(corr.shp.sem.zona[[1]][[5]], st.ids)

# get initial and end coordinates
coords <- do.call('rbind', lapply(coordinates(corr.cut)[[1]], function(x) x[c(1, nrow(x)),])) %>% 
  SpatialPoints(proj4string = crs(st.ids))

# initial coord
pos.init <- rgeos::gDistance(coords, st.ids[st.ids$gridcode == ids[1],], byid = TRUE) %>% 
  apply(MARGIN = 2, min) %>% 
  which.min()
init.pt <- coords[pos.init]

# final coord
pos.fin <- rgeos::gDistance(coords, st.ids[st.ids$gridcode == ids[2],], byid = TRUE) %>% 
  apply(MARGIN = 2, min) %>% 
  which.min()
fin.pt <- coords[pos.fin]

# Euclidean distance
(ED <- raster::pointDistance(init.pt, fin.pt) %>% as.numeric)

# calculate total corridor length
(Tot.len <- rgeos::gLength(corr.cut))

# Plot to check
plot(st.ids)
plot(corr.cut, add = T, col = 2)
plot(init.pt, add = 2, cex = 2, pch = 19)
plot(fin.pt, add = 2, cex = 2, pch = 19)

# Loop for all species

# No zoning
sp
(tab.sem.zona <- data.frame(sp = NA, scenario = NA, source = NA, target = NA, sim = NA, 
                  tot.dist = NA, euc.dist = NA, cost = NA)[-1,])
scen <- 'Land cover'

# for each species
corr.shp.sem.zona.nost <- list()
for(i in 1:length(corr.shp.sem.zona)) {
  
  corr.shp.tmp.nost <- list()
  
  # for each simulation
  for(j in 1:length(corr.shp.sem.zona[[i]])) {
    
    # select ID polygons
    (ids <- corr.shp.sts.sem.zona[[i]][[j]])
    (st.ids <- ST.pol[ST.pol$gridcode == ids[1] | ST.pol$gridcode == ids[2],])
    
    # remove area within STs
    corr.cut <- rgeos::gDifference(corr.shp.sem.zona[[i]][[j]], st.ids)
    
    # get initial and end coordinates
    coords <- do.call('rbind', lapply(coordinates(corr.cut)[[1]], function(x) x[c(1, nrow(x)),])) %>% 
      SpatialPoints(proj4string = crs(st.ids))
    
    # initial coord
    pos.init <- rgeos::gDistance(coords, st.ids[st.ids$gridcode == ids[1],], byid = TRUE) %>% 
      apply(MARGIN = 2, min) %>% 
      which.min()
    init.pt <- coords[pos.init]
    
    # final coord
    pos.fin <- rgeos::gDistance(coords, st.ids[st.ids$gridcode == ids[2],], byid = TRUE) %>% 
      apply(MARGIN = 2, min) %>% 
      which.min()
    fin.pt <- coords[pos.fin]
    
    # Euclidean distance
    (ED <- raster::pointDistance(init.pt, fin.pt) %>% as.numeric)
    
    # calculate total corridor length
    (Tot.len <- rgeos::gLength(corr.cut))
    
    # Plot to check
    # plot(st.ids)
    # plot(corr.cut, add = T, col = 2)
    # plot(init.pt, add = 2, cex = 2, pch = 19)
    # plot(fin.pt, add = 2, cex = 2, pch = 19)
    
    # attach info
    tab.sem.zona <- rbind(tab.sem.zona,
                          data.frame(sp = sp[i], scenario = scen, source = ids[1], target = ids[2], 
                                     sim = j, tot.dist = Tot.len, euc.dist = ED, cost = NA))
    
    # save cut corridors
    corr.shp.tmp.nost[[j]] <- corr.cut
    
    # print
    print(paste(i, j, ids[1], ids[2]))
  }
  
  corr.shp.sem.zona.nost[[i]] <- corr.shp.tmp.nost
  
}
corr.shp.sem.zona.nost

# No zoning
sp
(tab.com.zona <- data.frame(sp = NA, scenario = NA, source = NA, target = NA, sim = NA, 
                            tot.dist = NA, euc.dist = NA, cost = NA)[-1,])
scen <- 'Land cover + Urban zoning'

# for each species
corr.shp.com.zona.nost <- list()
for(i in 1:length(corr.shp.com.zona)) {
  
  corr.shp.tmp.nost <- list()
  
  # for each simulation
  for(j in 1:length(corr.shp.com.zona[[i]])) {
    
    # select ID polygons
    (ids <- corr.shp.sts.com.zona[[i]][[j]])
    (st.ids <- ST.pol[ST.pol$gridcode == ids[1] | ST.pol$gridcode == ids[2],])
    
    # remove area within STs
    corr.cut <- rgeos::gDifference(corr.shp.com.zona[[i]][[j]], st.ids)
    
    # get initial and end coordinates
    coords <- do.call('rbind', lapply(coordinates(corr.cut)[[1]], function(x) x[c(1, nrow(x)),])) %>% 
      SpatialPoints(proj4string = crs(st.ids))
    
    # initial coord
    pos.init <- rgeos::gDistance(coords, st.ids[st.ids$gridcode == ids[1],], byid = TRUE) %>% 
      apply(MARGIN = 2, min) %>% 
      which.min()
    init.pt <- coords[pos.init]
    
    # final coord
    pos.fin <- rgeos::gDistance(coords, st.ids[st.ids$gridcode == ids[2],], byid = TRUE) %>% 
      apply(MARGIN = 2, min) %>% 
      which.min()
    fin.pt <- coords[pos.fin]
    
    # Euclidean distance
    (ED <- raster::pointDistance(init.pt, fin.pt) %>% as.numeric)
    
    # calculate total corridor length
    (Tot.len <- rgeos::gLength(corr.cut))
    
    # Plot to check
    # plot(st.ids)
    # plot(corr.cut, add = T, col = 2)
    # plot(init.pt, add = 2, cex = 2, pch = 19)
    # plot(fin.pt, add = 2, cex = 2, pch = 19)
    
    # attach info
    tab.com.zona <- rbind(tab.com.zona,
                          data.frame(sp = sp[i], scenario = scen, source = ids[1], target = ids[2], 
                                     sim = j, tot.dist = Tot.len, euc.dist = ED, cost = NA))
    
    # save cut corridors
    corr.shp.tmp.nost[[j]] <- corr.cut
    
    # print
    print(paste(i, j, ids[1], ids[2]))
  }
  
  corr.shp.com.zona.nost[[i]] <- corr.shp.tmp.nost
  
}
corr.shp.com.zona.nost

#' ## Save corridors loaded without STs
#save.image(paste0(corrdir, 'corridors_RSFI_noSTv2.RData'))
# load(paste0(, 'corridors_RSFI_noST.RData'))

# falta: - contar o numero de corredores etc
# - extrair novamente os valores dos pixels pros corredores sem ST para calcular o custo
# (usar fasterize?)


# rasterize to extract resistance values

# too slow
corr.rast <- raster::rasterize(corr.shp.sem.zona.nost[[1]][[1]], ST.map)
corr.rast

# do not work, need to be polygon  
fasterize::fasterize(sf::st_as_sf(corr.shp.sem.zona.nost[[1]][[1]]), ST.map)

# 1m buffer to transform into a polygon, then rasterize
corr.rast <- sf::st_as_sf(corr.shp.sem.zona.nost[[1]][[1]]) %>% 
  sf::st_buffer(dist = 1) %>%
  fasterize::fasterize(raster = ST.map)

# raster::overlay(raster::brick(corr.rast, resistance_nozone[[1]]), function(a,b) a*b)
# corr.rast*resistance_nozone[[1]]
corr.vals <- ifelse(!is.na(corr.rast[]), resistance_nozone[[1]][], NA)
sum(corr.vals, na.rm = T)
cellStats(corr.rast*resistance_nozone[[1]], sum, na.rm = T)

#' ### Extract cost values from corridors outside STs, without zoning

# for each species
row.counter <- 1
for(i in 1:length(corr.shp.sem.zona)) {
  
  # for each simulation
  for(j in 1:length(corr.shp.sem.zona[[i]])) {
  
    print(row.counter, i, j)
    corr.rast <- sf::st_as_sf(corr.shp.sem.zona.nost[[i]][[j]]) %>% 
      sf::st_buffer(dist = 1) %>%
      fasterize::fasterize(raster = ST.map)
    
    cost <- cellStats(corr.rast*resistance_nozone[[i]], sum, na.rm = T)
    
    tab.sem.zona$cost[row.counter] <- cost
    row.counter <- row.counter+1
  }
}
tab.sem.zona

#' ### Extract cost values from corridors outside STs, with zoning

# for each species
row.counter <- 1
for(i in 1:length(corr.shp.com.zona)) {
  
  # for each simulation
  for(j in 1:length(corr.shp.com.zona[[i]])) {
    
    print(row.counter, i, j)
    corr.rast <- sf::st_as_sf(corr.shp.com.zona.nost[[i]][[j]]) %>% 
      sf::st_buffer(dist = 1) %>%
      fasterize::fasterize(raster = ST.map)
    
    cost <- cellStats(corr.rast*resistance_zone[[i]], sum, na.rm = T)
    
    tab.com.zona$cost[row.counter] <- cost
    row.counter <- row.counter+1
  }
}
tab.com.zona

#' ## Save corridors loaded and cut by ST

save.image(paste0('simulated_corridors/', 'corridors_loaded_noST.RData'))

