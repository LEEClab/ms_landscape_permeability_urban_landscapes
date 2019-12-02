#' ---
#' title: Supplementary Information: Several shades of gray: Combining land use, animal behavior, and urban zoning to assess landscape permeability in (growing) urban areas
#' author: Tulaci Bhakti Duarte, João Carlos Pena, Bernardo Brandão Niebuhr et al.
#' date: 2019   
#' ---

# --------------- label=load_packages, warning=FALSE, message=FALSE, echo=FALSE
if(!require(install.load)) install.packages('install.load'); library(install.load)
install.load::install_load('ezknitr', 'knitr', 'tidyverse', 'raster', 'rgdal')

knitr::opts_chunk$set(echo = FALSE)
knitr::opts_knit$set(root.dir= normalizePath('..'))
knitr::opts_chunk$set(error = FALSE)

#' # Rationale
#' 
#' Here we introduce the method used to calculate the overlap between ecological corridors in different 
#' simulation scenarios and for distinct species. The general idea is that one has two maps of corridors - 
#' e.g. maps for the same species in different scenarios or for the different species in the same scenario -
#' and we want to measure how much the corridors overlap between each other. This includes not only whether 
#' the corridors overlap, but also how many corridors from each map cross pixels where there is overlap.
#' 
#' # A simple example
#' 
#' First, we give a 1 dimensional example. Imagine that, instead of corridor simulations in a
#' two dimensional space, we have the number of animals that cross a given section of a road 
#' (so, a 1-dimensional problem).
#' It is still an issue of landscape permeability, but simplified for illustration purposes.
#' Let's say the number of animals in a given scenario (e.g., with no road passages) is given by a Gaussian 
#' distribuion with $\mu$ = 10 and $\sigma$ = 1. We'll sample 100 points where animals crossed this road section. 
#' This mean most animals
#' tend to cross the road at the kilometer 10, but there is some variation and animals also crossing at other places.
#' One possibility of explanation of this pattern may be that, at the kilometer 10, there are habitat patches
#' contiguous to both sides of the road, so animals are prone to approach and cross the road at this place. 
#' Below we show this distribution:

# seed to keep the random numbers fixed
set.seed(123)

no.passages <- rnorm(100, 0, 1) + 10
breaks <- seq(6, 18, 0.5)
hist(no.passages, breaks = breaks, col = rgb(0, 0, 1, alpha = 0.5), 
     xlab = 'Road section (km)', main = 'No road passages')

#' Now We'll do the same after a road passage is built at the kilometer 11 (so we'll assume most animals will start 
#' to cross the road at this new point, but again with the some variation).

passages <- rnorm(100, 0, 2) + 11
hist(passages, breaks = breaks, col = rgb(1, 0, 0, alpha = 0.5), 
     xlab = 'Road section (km)', main = 'With road passages')

#' Let's compare both now. But, instead of showing the absolute frequency of records at each location, we'll
#' present the relative frequency of records at each location. This means that, if we multiply the width and the
#' height of each bar in this histogram, we'll get the probability of observing an animal crossing the road at each
#' section. The probability density in the y axis is then the probability of crossing 
#' per unit of length of the road section.
#' 
#' What we want, at the end, is to calculate the area under both curves, where there is an overlap between both 
#' probability curves. This will tell us how much consistency there is between the two scenarios (without and 
#' with road passagens), a value between 0 (no overlap at all) and 1 (complete overlap, the number of crossing 
#' animals in each road section is exactly the same in both scenarios).

hist(no.passages, breaks = breaks, col = rgb(0, 0, 1, alpha = 0.5), prob = T,
     xlab = 'Road section (km)', main = '', 
     ylim = c(0, 0.5), xlim = c(7,16))
hist(passages, breaks = breaks, col = rgb(1, 0, 0, alpha = 0.5), prob = T, add = T)
legend('topright', legend = c('No road passages', 'With road passages', 'We want to calculate this area'), 
       fill = c(rgb(0, 0, 1, alpha = 0.5), rgb(1, 0, 0, alpha = 0.5), rgb(0.5, 0, 0.18, alpha = 0.6)))

#' If we would calculate the overlap between continuous probability distributions, we would calculate the area 
#' that falls below both curves, using the formula:
#' 
#' \begin{equation}
#' \tag{1}
#' \int_x min(p_1(x), p_2(x)) dx
#' \end{equation}
#' 
#' where $p_1(x)$ and $p_2(x)$ and the two probability functions. As we're using samples of a distribution, we
#' can approximate that for:
#' 
#' \begin{equation}
#' \tag{2}
#' \sum_x min(p_1(x), p_2(x)) \Delta x
#' \end{equation}
#' 
#' and $\Delta$ can be as small as one wants. We'll do that for our case. 
#' 
#' First, we'll count the number of crossing records at each road section (here, road sections of 
#' 0.5 km) and each scenario.

breaks <- seq(6, 18, 0.5)
(np.tab <- cut(no.passages, breaks) %>% table)
(p.tab <- cut(passages, breaks) %>% table)

#' Now we'll combine both counts in a `data.frame`, calculate the probability of crossing at each road
#' section, and the calculus of $min(p_1(x), p_2(x))$ (as in eq. 2 above) for each road section.

(df <- data.frame(cbind(np.tab, p.tab)) %>%
    mutate(np.prob = np.tab/sum(np.tab),
           p.prob = p.tab/sum(p.tab),
           overlap_per_road_section = pmin(np.prob, p.prob)))

#' Finally, we calculate the total overlap (the are below both curves) summing the overlap column
#' to get the sum in eq (2):

sum(df$overlap_per_road_section)

#' # Overlap between ecological corridors simulated in different scenarios
#' 
#' Now we're going to extend this approach to a two dimensional example, using an data from this manuscript. 
#' We simulated ecological corridors 
#' in two scenarios (considering (a) land use only and (b) land use + urban zoning) to define the permeability
#' of animals moving throughtout the landscape. We use the Route Selection Frequency Index of each scenario
#' (RSFI), which measures the number of corridors crossing each pixel in space, and is therefore a proxy for
#' the frequency distribution in 1D shown above. We'll transform this RSFI map into a map of probability of movement
#' (especifically, a probability per unit of area)
#' by dividing the RSFI by the sum of all RSFI pixel values. Then, we'll proceed with the same calculus of 
#' eq. 2, but applied to the 2-dimensional space.
#' 
#' Let's first load the RSFI maps.

# folder where RSFI maps are
corrdir <- '/home/bniebuhr/Documents/00_academico/01_artigos/ms_Pena_etal_corredores_OuroPreto/corridors/simulated_corridors/'

# Load data
setwd(corrdir)

# Corridors without urban zoning
files <- list.files('simulated_corridors/RSFI/', pattern = 'nozoning', include.dirs = T, 
                    full.names = T)
files_tif <- files[endsWith(files, '.tif')]

corridors_no_zone <- list()
for(i in 1:length(files_tif)) {
  corridors_no_zone[[i]] <- raster(files_tif[i])
}

# Corridors with urban zoning
files <- list.files('simulated_corridors/RSFI/', pattern = '_zoning', 
                    include.dirs = T, full.names = T)
files_tif <- files[endsWith(files, '.tif')]

corridors_zone <- list()
for(i in 1:length(files_tif)) {
  corridors_zone[[i]] <- raster(files_tif[i])
}

#' Let's compare how the corridors look like in both scenarios for one species, **A. leucophthalmus**.

par(mfrow = c(1, 2))
plot(corridors_no_zone[[1]])
plot(corridors_zone[[1]])
par(mfrow = c(1,1))

#' Now we'll transform both maps into movement probability maps, as mentioned above:

nz.prob <- corridors_no_zone[[1]]/cellStats(corridors_no_zone[[1]], sum)
sum(nz.prob[], na.rm = T) # ok, sum = 1
# plot(nz.prob)

z.prob <- corridors_zone[[1]]/cellStats(corridors_zone[[1]], sum)
sum(z.prob[], na.rm = T) # ok, sum = 1

#' The next step is to calculate the overlap at each pixel, but before that we have to make sure 
#' both maps have exactly the same extent. Finally, we'll sum the values of the overlap layer to get a 
#' measure of overlap or consistency between the two scenarios, and we'll also plot that to visualize
#' the locations where there was more overlap.

# Force that the no zone probability map has the same extent as the zone map, by extending and croping
# its extension.
nz.prob2 <- crop(extend(nz.prob, z.prob), z.prob)
#plot(nz.prob2)

# Calculate the overlap at each pixel
overlap <- z.prob
overlap[] <- pmin(nz.prob2[], z.prob[])

# Total overlap value between scenarios
cellStats(overlap, sum)

# Plot overlap layer in log scale, since the probability values per unit area are too small.
plot(log(overlap))