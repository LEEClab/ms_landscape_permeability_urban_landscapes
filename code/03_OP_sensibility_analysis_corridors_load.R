#' ---
#' title: 'Urban ecological corridors in Ouro Preto, sensibility analysis: loading corridors'
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
install.load::install_load('raster', 'sp', "sf", 'rgdal')
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
corrdir <- 'analysis_sensibility/'
stdir <- 'input/sts/'
mapdir <- 'input/shapefiles/'

# Output folder
outdir <- 'analysis_sensibility/output/'

# --------------- label=load_data
# Load data

#' ## Loading data
#' 
#' Reading and plotting the data, for visualization purposes.
#'
#' Species input resistance maps

# species
sp <- 'A. leucophthalmus'
sp.brief <- "aleuco"

# methods
method <- c("MP", "MLavg", "MLmax", "MLavg", "MLmax")
scale <- c(100, 150, 150, 400, 400)
scale_text <- paste0("scale_", scale)
met_scal <- paste(method, scale, sep = "_")

#' ### Resistance layers
files <- list.files('input/', pattern = '1_100.tif',
                    include.dirs = T, full.names = T, recursive = T) %>% 
  grep(pattern = sp.brief, value = T)

#' Resistance surfaces without urban zoning
files_landuse <- grep(pattern = 'landcover', x = files, value = T)

resistance_nozone <- raster(files_landuse)
names(resistance_nozone) <- sp

#' Resistance surfaces without urban zoning
files_zoning <- grep(pattern = '/zoning/', x = files, value = T)

resistance_zone <- raster(files_zoning)
names(resistance_zone) <- sp

#' ### RSFI maps

#' Corridors without urban zoning

# files
files <- list.files(corrdir, pattern = 'RSFI.tif', recursive = T,
                    include.dirs = T, full.names = T)
           
files_landcover <- files %>% 
  grep(pattern = "landcover", value = T)
files_landcover_tif <- files_landcover[endsWith(files_landcover, '.tif')]

corridors_no_zone <- list()
for(i in 1:length(files_landcover_tif)) {
  corridors_no_zone[[i]] <- raster(files_landcover_tif[i])
}
names(corridors_no_zone) <- met_scal
# plot(corridors_no_zone[[1]])
# plot(corridors_no_zone[[3]])

#' Corridors with urban zone
files_zoning <- files %>% 
  grep(pattern = "_zoning", value = T)
files_zoning_tif <- files_zoning[endsWith(files_zoning, '.tif')]

corridors_zone <- list()
for(i in 1:length(files_zoning_tif)) {
  corridors_zone[[i]] <- raster(files_zoning_tif[i])
}
names(corridors_zone) <- met_scal
# plot(corridors_zone[[1]])
# plot(corridors_zone[[3]])

# plot
par(mfrow = c(1, 2))
ind <- 3
plot(corridors_no_zone[[ind]])
plot(corridors_zone[[ind]])
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

# land.use.map <- raster::shapefile(paste0(mapdir, 'Classe_uso_solo.shp'))
# land.use.map <- rgdal::readOGR(dsn = mapdir, layer = 'Classe_uso_solo')
land.use.map <- sf::st_read(dsn = paste0(mapdir, 'Classe_uso_solo.shp'))
cols1 = c('LightSkyBlue', 'beige', 'forestgreen', 'red', 'SlateGray')[land.use.map$Classe]

#' ### Urban zones
# urban.zone.map <- raster::shapefile(paste0(mapdir, 'Zoneamento_completo'))
# urban.zone.map <- rgdal::readOGR(mapdir, 'Zoneamento_completo')
urban.zone.map <- sf::read_sf(paste0(mapdir, 'Zoneamento_completo.shp')) %>% 
  dplyr::mutate(Layer = as.factor(Layer))
cols2 = grey.colors(length(levels(urban.zone.map$Layer)))[urban.zone.map$Layer]

# Plot
par(mfrow = c(1, 2))
plot(land.use.map[5], col = cols1, border = cols1)
plot(urban.zone.map[2], col = cols2, border = cols2)
par(mfrow = c(1, 1))

#' ### Shapefiles
#' 
#' Each shapefile corresponds to a different corridor simulation.
#'
#' #### Corridors' shapefiles without urban zone

ff <- list.files(corrdir, pattern = '.shp$', include.dirs = T,
           recursive = TRUE, full.names = T)

corr.shp.sem.zona <- list()
corr.shp.sts.sem.zona <- list()
method
scale_text

for(i in 1:length(method)) {
  
  corr.sp <- list()
  corr.sts <- list()
  
  inds <- grep(method[i], ff, value = T) %>% 
    grep(pattern = scale_text[i], value = T) %>% 
    grep(pattern = "landcover", value = T) %>% 
    stringr::str_replace(pattern = "//", replacement = "/")
  for(cont in 1:length(inds)) {
    #print(paste(i, ',', cont, ',', inds[cont]))
    tryCatch({
      print(cont)
      corr.sp[[cont]] <- sf::st_read(inds[cont])
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
names(corr.shp.sem.zona) <- met_scal
names(corr.shp.sts.sem.zona) <- met_scal

#' #### Corridors' shapefiles with urban zone
corr.shp.com.zona <- list()
corr.shp.sts.com.zona <- list()

for(i in 1:length(method)) {
  
  corr.sp <- list()
  corr.sts <- list()
  
  inds <- grep(method[i], ff, value = T) %>% 
    grep(pattern = scale_text[i], value = T) %>% 
    grep(pattern = "zoning", value = T) %>% 
    stringr::str_replace(pattern = "//", replacement = "/")
  for(cont in 1:length(inds)) {
    #print(paste(i, ',', cont, ',', inds[cont]))
    print(cont)
    corr.sp[[cont]] <- sf::st_read(inds[cont])
    
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
names(corr.shp.com.zona) <- met_scal
names(corr.shp.sts.com.zona) <- met_scal

#' ### Txts
#' 
#' Output corridor text files without zones
ff <- list.files(corrdir, pattern = '.txt', 
                 recursive = TRUE, full.names = T) %>% 
  grep(pattern = 'Results', value = T) %>% 
  grep(pattern = 'landcover', value = T)

corr.txt.sem.zona <- list()
for(i in 1:length(ff)) {
  corr.txt.sem.zona[[i]] <- read.table(ff[i], header = T, sep = ',')
}
str(corr.txt.sem.zona)

corr.txt.sem.zona.1tab <- dplyr::bind_rows(corr.txt.sem.zona) %>% 
  dplyr::as_tibble()

corr.txt.sem.zona <- corr.txt.sem.zona.1tab %>% 
  split(f = factor(paste(.$SIMULATION_METHOD, .$SCALE, sep = "_")))
str(corr.txt.sem.zona)

#' Output corridor text files with zones
ff2 <- list.files(corrdir, pattern = '.txt', 
                 recursive = TRUE, full.names = T) %>% 
  grep(pattern = 'Results', value = T) %>% 
  grep(pattern = 'zoning', value = T)

corr.txt.com.zona <- list()
for(i in 1:length(ff2)) {
  corr.txt.com.zona[[i]] <- read.table(ff2[i], header = T, sep = ',')
}
str(corr.txt.com.zona)

corr.txt.com.zona.1tab <- dplyr::bind_rows(corr.txt.com.zona) %>% 
  dplyr::as_tibble()

corr.txt.com.zona <- corr.txt.com.zona.1tab %>% 
  split(f = factor(paste(.$SIMULATION_METHOD, .$SCALE, sep = "_")))
str(corr.txt.com.zona)


#' ## Save corridors loaded
save.image(paste0(corrdir, 'corridors_loaded.RData'))

