#' ---
#' title: 'Urban ecological corridors in Ouro Preto: removing STs from corridors'
#' author: Bernardo Niebuhr
#' ---

# --------------- label=load_packages, warning=FALSE, message=FALSE, echo=FALSE
if(!require(install.load)) install.packages('install.load'); library(install.load)

# library for plots
install.load::install_load('ezknitr', 'knitr')

# Print options
options(width = 165)
opts_chunk$set(error = F, message = F, warning = F, cache = F, echo = T, results = F,
               fig.align = 'center')


# -----
install.load::install_load('plotrix')

# sps
sp.short <- c('aleuco', 'ccaudata', 'pleuco', 'sscans', 'xfuscus')
sp <- c('A. leucophthalmus', 'C. caudata', 'P. leucoptera', 'S. scansor', 'X. fuscus')

# land use classes and values
lu.classes <- c('forest', 'grasslands', 'urban', 'water', 'mined areas')

lu.resist <- list(
  aleuco = c(1.34, 3.83, 4.83, 5, 10),
  ccaudata = c(2.01, 3.27, 4.72, 5, 10),
  pleuco = c(1.79, 3.5, 4.71, 5, 10),
  sscansor = c(1.22, 3.93, 4.85, 5, 10),
  xfuscus = c(1.57, 3.6, 4.83, 5, 10)
)

# zoning classes and weights
zoning.classes <- c('UC', 'ZPAM', 'ZR', 'ZAR1', 'ZEUS1', 'ZPE', 'ZDE', 'ZEUS2', 'ZAR2', 'ZAR3', 'ZIE', 'ZA', 'ZA2')
zoning.weights <- c(0.1, 0.52, 1.01, 1.41, 2.07, 2.33, 2.67, 3.05, 3.33, 5, 5, 7.5, 10)

#-----
# create land use values, standardizes in the interval [1,100]
mat.lu <- lapply(lu.resist, function(x)
  matrix(rep(x, length(zoning.classes)), ncol = length(lu.classes), byrow = T,
              dimnames = list(zoning.classes, lu.classes))
)

std <- function(x) 99 * (x - min(x))/(max(x) - min(x)) + 1
std(mat.lu[[1]])

mat.lu.std <- lapply(mat.lu, std)

# plot
img <- function(x, names) {

  xmin <- min(x)
  xmax <- max(x)
  
  #Generate the palette for the matrix and the legend.  Generate labels for the legend
  palmat <- color.scale(x, c(1, 0.4), c(1, 0.4), c(0.96, 1))
  palleg <- color.gradient(c(1, 0.4), c(1, 0.4), c(0.96, 1), nslices=100)
  # palmat <- color.scale(x, c(1,0.5,0),c(0,0.5,0),c(0,0,1),)
  # palleg <- color.gradient(c(1,0.5,0),c(0,0.5,0),c(0,0,1), nslices=100)
  lableg <- c(formatC(xmin, format="f", digits=2), formatC(1*(xmax-xmin)/4, format="f", digits=2), formatC(2*(xmax-xmin)/4, format="f", digits=2), formatC(3*(xmax-xmin)/4, format="f", digits=2), formatC(xmax, format="f", digits=2))
  
  #Set up the plot area and plot the matrix
  par(mar=c(5, 5, 5, 8))
  color2D.matplot(x, cellcolors=palmat, main=names, show.values=2, vcol=rgb(0,0,0), axes=FALSE, vcex=0.7, 
                  xlab = 'Land cover', ylab = 'Zoning')
  axis(1, at=seq(1, length(lu.classes), 1)-0.5, labels=lu.classes, tck=-0.01, padj=-1)
  
  #In the axis() statement below, note that the labels are decreasing.  This is because
  #the above color2D.matplot() statement has "axes=FALSE" and a normal axis()
  #statement was used.
  axis(2, at=seq(1, length(zoning.classes), 1)-0.5, labels=zoning.classes, tck=-0.01, padj=0.7, las=1)
  
  #Plot the legend
  pardat <- par()
  color.legend(pardat$usr[2]+0.5, 0, pardat$usr[2]+1, pardat$usr[2], paste(" ", lableg, sep=""), palleg, align="rb", gradient="y", cex=0.7)
}

mapply(img, mat.lu.std, sp)

pdf('../output/land_use_resistance.pdf', width = 15, height = 18, )
mapply(img, mat.lu.std, sp)
dev.off()

for(i in 1:length(sp)) {
  png(paste0('../output/land_cover_resist_vals_', sp.short[i], '.png'), width = 18, height = 18,
      units = 'cm', res = 300)  
  img(mat.lu.std[[i]], sp[i])
  dev.off()
}

#-----
# create land cover + zoning maps, in the range [1,100]

mat.zon <- matrix(rep(zoning.weights, length(lu.classes)), nrow = length(zoning.classes),
         dimnames = list(zoning.classes, lu.classes))

# muliply land cover * zoning weights
mat.lu.zon <- lapply(mat.lu.std, function(x) x * mat.zon)

# standardize
mat.lu.zon.std <- lapply(mat.lu.zon, std)

# plot                     
mapply(img, mat.lu.zon.std, sp)

for(i in 1:length(sp)) {
  png(paste0('../output/land_cover_zoning_resist_vals_', sp.short[i], '.png'), width = 18, height = 18,
      units = 'cm', res = 300)  
  img(mat.lu.zon.std[[i]], sp[i])
  dev.off()
}

#-----
# proportional change
prop.change <- function(before, after) 100 * (after/before - 1)

mat.prop.change <- mapply(prop.change, mat.lu.std, mat.lu.zon.std, SIMPLIFY = F)

# plot
mapply(img, mat.prop.change, sp)

for(i in 1:length(sp)) {
  png(paste0('../output/relative_change_resist_vals_', sp.short[i], '.png'), width = 18, height = 18,
      units = 'cm', res = 300)  
  img(mat.prop.change[[i]], sp[i])
  dev.off()
}
