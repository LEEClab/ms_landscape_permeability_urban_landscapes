#' ---
#' title: 'Urban ecological corridors in Ouro Preto: removing STs from corridors'
#' author: 
#' - Tulaci Bhakti, Bernardo Niebuhr, Joao Carlos Pena
#' date: May 2019
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
install.load::install_load('raster', 'sp', 'rgdal')#, 'GISTools', 'rgeos', 'trajectories', 'spacetime', 'spatstat')
install.load::install_load('ezknitr', 'knitr', 'tidyverse')

# Print options
options(width = 165)
opts_chunk$set(error = F, message = F, warning = F, cache = F, echo = T, results = F,
               fig.align = 'center')

# --------------- label=setup
# Set up 

# Clean everything before beginning

rm(list = ls())

# Data folder
corrdir <- 'simulated_corridors/'
stdir <- 'input/Raster_finais/'
mapdir <- 'input/Shapefiles/'

outdir <- 'output/'

# --------------- label=load_data
# Load data

#' ## Loading data
#' 
#' Reading and plotting the data, for visualization purposes.
#' 
#' ### RSFI maps

load('corridors_loaded.RData')

#' ## Remove ST from maps
#' 
#' ### Remove ST from RSFI maps

#' First, we check that all maps have the same extent
#' We'll standardize the extent according to the first species of no zone scenario

# no zoning
corridors_nozone_nost <- list()

for(i in 1:length(corridors_no_zone)) {
  corridors_nozone_nost[[i]] <- raster::crop(raster::extend(corridors_no_zone[[i]], corridors_no_zone[[1]]), corridors_no_zone[[1]])
}
corridors_nozone_nost

# just to check that nothing changed in the map, only the extent
#raster::writeRaster(corridors_zone[[4]], 'raster_antigo.tif', overwrite = TRUE)
#raster::writeRaster(corridors_zone_nost[[4]], 'raster_novo.tif')

# zoning
corridors_zone_nost <- list()

for(i in 1:length(corridors_zone)) {
  corridors_zone_nost[[i]] <- raster::crop(raster::extend(corridors_zone[[i]], corridors_no_zone[[1]]), corridors_no_zone[[1]])
}
corridors_nozone_nost

#' Now we'll transform do the same for the ST map.

ST.map <- raster::crop(raster::extend(ST.map, corridors_no_zone[[1]]), corridors_no_zone[[1]])
ST.map[1:10]

#' Finally we'll remove corridors information from ST patches

# no zoning
for(i in 1:length(corridors_nozone_nost)) {
  print(i)
  corridors_nozone_nost[[i]][] <- ifelse(ST.map[] == 0, NA, corridors_nozone_nost[[i]][])
}
corridors_nozone_nost

# check
cellStats(corridors_no_zone[[3]], stat = 'range')
cellStats(corridors_nozone_nost[[3]], stat = 'range')

# zoning
for(i in 1:length(corridors_nozone_nost)) {
  print(i)
  corridors_nozone_nost[[i]][] <- ifelse(ST.map[] == 0, NA, corridors_nozone_nost[[i]][])
}
corridors_nozone_nost