#' ---
#' title: 'Urban ecological corridors in Ouro Preto: loading corridors'
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
install.load::install_load('raster', 'sp', 'rgdal')
install.load::install_load('ezknitr', 'knitr', 'tidyverse')

# Print options
options(width = 165)
opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
opts_chunk$set(error = F, message = F, warning = F, cache = F, echo = T, results = F,
               fig.align = 'center')

# --------------- label=setup
# Set up 

# Clean everything before beginning
rm(list = ls())

# Data folder
corrdir <- 'simulated_corridors/results_corridors_rescaled/'
stdir <- 'input/sts/'
mapdir <- 'input/shapefiles'

# Output folder
outdir <- 'output/'

# --------------- label=load_data
# Load data

#' ## Loading data
#' 
#' Reading and plotting the data, for visualization purposes.
#'
#' Species input resistance maps

# species
sp <- c('A. leucophthalmus', 'C. caudata', 'P. leucoptera', 'S. scansor', 'X. fuscus')

#' ### Resistance layers
files <- list.files('input/', pattern = '1_100.tif',
                    include.dirs = T, full.names = T, recursive = T)

#' Resistance surfaces without urban zoning
files_landuse <- grep(pattern = 'landcover', x = files, value = T)

resistance_nozone <- list()
for(i in 1:length(files_landuse)) {
  resistance_nozone[[i]] <- raster(files_landuse[i])
}
names(resistance_nozone) <- sp

#' Resistance surfaces without urban zoning
files_zoning <- grep(pattern = '/zoning/', x = files, value = T)

resistance_zone <- list()
for(i in 1:length(files_zoning)) {
  resistance_zone[[i]] <- raster(files_zoning[i])
}
names(resistance_nozone) <- sp

#' ### RSFI maps

#' Corridors without urban zoning

# files
files <- list.files(paste0(corrdir, 'simulations_landcover_rescaled/'), pattern = 'RSFI.tif', 
                    include.dirs = T, full.names = T)
files_tif <- files[endsWith(files, '.tif')]

corridors_no_zone <- list()
for(i in 1:length(files_tif)) {
  corridors_no_zone[[i]] <- raster(files_tif[i])
}
names(corridors_no_zone) <- sp
# plot(corridors_no_zone[[1]])
# plot(corridors_no_zone[[3]])

#' Corridors with urban zone
files <- list.files(paste0(corrdir, 'simulations_zoning_rescaled/'), pattern = 'RSFI.tif', 
                    include.dirs = T, full.names = T)
files_tif <- files[endsWith(files, '.tif')]

corridors_zone <- list()
for(i in 1:length(files_tif)) {
  corridors_zone[[i]] <- raster(files_tif[i])
}
names(corridors_zone) <- sp
# plot(corridors_zone[[1]])
# plot(corridors_zone[[3]])

# plot
par(mfrow = c(1, 2))
plot(corridors_no_zone[[3]])
plot(corridors_zone[[3]])
par(mfrow = c(1, 1))

#' ### Source fragments
ST.map <- raster(paste0(stdir, 'Fragmentos_fonte2.tif'))
plot(ST.map, legend = F, col = grey(0.75), colNA = 1)
plot(corridors_zone[[1]], add = T, col = rev(heat.colors(20)))

#' ### Land use
#'
#' Land use classes:  
#' - 1: water  
#' - 2: matrix  
#' - 3: forest  
#' - 4: minning  
#' - 5: urban

#land.use.map <- raster::shapefile(paste0(mapdir, 'Classe_uso_solo.shp'))
land.use.map <- rgdal::readOGR(dsn = mapdir, layer = 'Classe_uso_solo')
cols1 = c('LightSkyBlue', 'beige', 'forestgreen', 'red', 'SlateGray')[land.use.map$Classe]

#' ### Urban zones
#urban.zone.map <- raster::shapefile(paste0(mapdir, 'Zoneamento_completo'))
urban.zone.map <- rgdal::readOGR(mapdir, 'Zoneamento_completo')
cols2 = grey.colors(length(levels(urban.zone.map$Layer)))[urban.zone.map$Layer]

# Plot
par(mfrow = c(1, 2))
plot(land.use.map, col = cols1, border = cols1)
plot(urban.zone.map, col = cols2, border = cols2)
par(mfrow = c(1, 1))

#' ### Shapefiles
#' 
#' Each shapefile corresponds to a different corridor simulation.
#'
#' #### Corridors' shapefiles without urban zone

ff <- list.files(paste0(corrdir, 'simulations_landcover_rescaled/'), pattern = '.shp', 
           recursive = TRUE, full.names = T)

corr.shp.sem.zona <- list()
corr.shp.sts.sem.zona <- list()
sp <- c('aleuco', 'ccaud', 'pleuco', 'sscans', 'xfusc')

for(i in 1:length(sp)) {
  
  corr.sp <- list()
  corr.sts <- list()
  
  inds <- grep(sp[i], ff, value = T)
  for(cont in 1:length(inds)) {
    #print(paste(i, ',', cont, ',', inds[cont]))
    tryCatch({
      print(cont)
      sep <- gregexpr(inds[cont], pattern = '/')[[1]] %>% last()
      direct <- substr(inds[cont], start = 1, stop = sep-1)
      file <- substr(inds[cont], sep+1, nchar(inds[cont])-4)
      corr.sp[[cont]] <- rgdal::readOGR(dsn = direct, layer = file)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
    tt <- stringr::str_split(inds[cont], 'S_')[[1]][3]
    ini <- stringr::str_split(tt, '_T_')[[1]][1] %>% as.numeric
    fin <- stringr::str_split(tt, '_T_')[[1]][2] %>% 
      stringr::str_split('_M') %>% 
      first() %>% first() %>% 
      as.numeric
    
    corr.sts[[cont]] <- c(ini, fin)
  }
  
  corr.shp.sem.zona[[i]] <- corr.sp
  corr.shp.sts.sem.zona[[i]] <- corr.sts
}

#' #### Corridors' shapefiles with urban zone

ff <- list.files(paste0(corrdir, 'simulations_zoning_rescaled/'), pattern = '.shp', 
                 recursive = TRUE, full.names = T)

corr.shp.com.zona <- list()
corr.shp.sts.com.zona <- list()
sp <- c('aleuco', 'ccaudat', 'pleuco', 'scans', 'xfuscu')

for(i in 1:length(sp)) {
  
  corr.sp <- list()
  corr.sts <- list()
  
  inds <- grep(sp[i], ff, value = T)
  for(cont in 1:length(inds)) {
    #print(paste(i, ',', cont, ',', inds[cont]))
    print(cont)
    sep <- gregexpr(inds[cont], pattern = '/')[[1]] %>% last()
    direct <- substr(inds[cont], start = 1, stop = sep-1)
    file <- substr(inds[cont], sep+1, nchar(inds[cont])-4)
    corr.sp[[cont]] <- rgdal::readOGR(dsn = direct, layer = file)
    
    tt <- stringr::str_split(inds[cont], 'S_')[[1]][3]
    ini <- stringr::str_split(tt, '_T_')[[1]][1] %>% as.numeric
    fin <- stringr::str_split(tt, '_T_')[[1]][2] %>% 
      stringr::str_split('_M') %>% 
      first() %>% first() %>% 
      as.numeric
    
    corr.sts[[cont]] <- c(ini, fin)
  }
  
  corr.shp.com.zona[[i]] <- corr.sp
  corr.shp.sts.com.zona[[i]] <- corr.sts
}

#' ### Txts
#' 
#' Output corridor text files without zones
ff <- list.files(paste0(corrdir, 'simulations_landcover_rescaled/'), pattern = '.txt', 
                 recursive = TRUE, full.names = T) %>% 
  grep(pattern = 'Results', value = T)

corr.txt.sem.zona <- list()
for(i in 1:length(ff)) {
  corr.txt.sem.zona[[i]] <- read.table(ff[i], header = T, sep = ',')
}
str(corr.txt.sem.zona)

# # Add 900 rows with NA
# mm <- data.frame(matrix(NA, nrow = 900, ncol = ncol(corr.txt.sem.zona[[4]])))
# names(mm) <- names(corr.txt.sem.zona[[4]])
# corr.txt.sem.zona[[4]] <- rbind(corr.txt.sem.zona[[4]], mm)

#' Output corridor text files with zones
ff2 <- list.files(paste0(corrdir, 'simulations_zoning_rescaled/'), pattern = '.txt', 
                 recursive = TRUE, full.names = T) %>% 
  grep(pattern = 'Results', value = T)

corr.txt.com.zona <- list()
for(i in 1:length(ff2)) {
  corr.txt.com.zona[[i]] <- read.table(ff2[i], header = T, sep = ',')
}
str(corr.txt.com.zona)

#' ## Save corridors loaded

save.image(paste0('simulated_corridors/', 'corridors_loaded.RData'))

