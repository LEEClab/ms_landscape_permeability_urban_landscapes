#' ---
#' title: 'Urban ecological corridors in Ouro Preto, sensibility analysis: understanding simulated corridors'
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
install.load::install_load('raster', 'sf', 'rgdal')#, 'GISTools', 'rgeos', 'trajectories', 'spacetime', 'spatstat')
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
corrdir <- 'analysis_sensibility/'
stdir <- 'input/sts'
mapdir <- 'input/shapefiles'

# Output folder
outdir <- 'analysis_sensibility/output/'

# --------------- label=load_data
# Load data

#' ## Loading data
#' 
#' Load corridor rasters, vetors, and information already imported into R, with source-target
#' patches removed.
load('analysis_sensibility/corridors_loaded_noST.RData')

#' ### RSFI maps

#---- label=resistance_vs_scenario ----------------------------------------------------------------

sp <- 'A. leucophthalmus'

# methods
method <- c("MP", "MLavg", "MLmax", "MLavg", "MLmax")
scale <- c(100, 150, 150, 400, 400)
scale_text <- paste0("scale_", scale)
met_scal <- paste(method, scale, sep = "_")
met_scal_fact <- paste0(method, ": scale ", scale, "m")
met_scal_fact[1] <- "MP"

# Original LSCorridors output
cont <- 1
for(i in c(5,1,3,2,4)) {
  corr.txt.sem.zona[[i]]$zone <- 'LandC'
  corr.txt.com.zona[[i]]$zone <- 'LandC+UrbZ'
  tt <- rbind(corr.txt.sem.zona[[i]], corr.txt.com.zona[[i]])
  tt$method_scale <- met_scal[cont]
  
  if(i == 5) {
    tab.st <- tt
  } else {
    tab.st <- rbind(tab.st, tt)
  }
  cont <- cont + 1
}
head(tab.st)

# Corridors output with no sts
tab.sem.zona$scenario <- 'LandC'
tab.com.zona$scenario <- 'LandC+UrbZ'
tab <- rbind(tab.sem.zona, tab.com.zona)
tab$scenario <- as.factor(tab$scenario)
tab$method_scale <- forcats::lvls_revalue(tab$method_scale, 
                                          new_levels = met_scal_fact)

# Original output

# Plot relative corridor cost
ggplot() +
  geom_jitter(data = tab.st, aes(x = zone, y = CORRIDOR_COST/EUCLIDEAN_DISTANCE)) + 
  facet_wrap(~method_scale) + 
  labs(x = '', y = 'Relative corridor cost')

ggplot() +
  geom_boxplot(data = tab.st, aes(x = zone, y = CORRIDOR_COST/EUCLIDEAN_DISTANCE)) + 
  facet_wrap(~method_scale) +
  theme_minimal() +
  labs(x = '', y = 'Relative corridor cost')

# Output without STs
ggplot() +
  geom_jitter(data = tab, aes(x = scenario, y = cost/euc.dist)) + 
  facet_wrap(~method_scale) + 
  theme_minimal() +
  labs(x = '', y = 'Relative corridor cost')

g0 <- ggplot() +
  geom_boxplot(data = tab, aes(x = scenario, y = cost/euc.dist)) + 
  facet_wrap(~method_scale) + 
  theme_minimal() +
  labs(x = '', y = 'Relative corridor cost')
g0
ggsave('relative_corridor_cost_same_scale.png', plot = g0, path = outdir, device = 'png', width = 24, height = 15, units = 'cm', dpi = 300)

g1 <- ggplot() +
  geom_boxplot(data = tab, aes(x = scenario, y = cost/euc.dist)) + 
  facet_wrap(~method_scale, scales = 'free') + 
  theme_minimal() +
  labs(x = '', y = 'Relative corridor cost')
g1
ggsave('relative_corridor_cost.png', plot = g1, path = outdir, device = 'png', width = 24, height = 15, units = 'cm', dpi = 300)

# Summary
tab.summ <- tab %>% 
  dplyr::filter(euc.dist > 0) %>% 
  group_by(method_scale, scenario) %>% 
  summarise(
    avg.cost = mean(cost/euc.dist),
    err.cost = sd(cost/euc.dist)/sqrt(n()),
    supp = avg.cost + 1.96*err.cost,
    inff = avg.cost - 1.96*err.cost
  )

g1.2 <- ggplot(data = tab.summ) +
  geom_point(aes(x = scenario, y = avg.cost)) + 
  geom_errorbar(aes(x = scenario, ymin = inff, ymax = supp)) +
  facet_wrap(~method_scale, scales = 'free') +
  theme_minimal() +
  labs(x = '', y = 'Relative corridor cost')
g1.2
ggsave('relative_corridor_cost_avg_err.png', plot = g1.2, path = outdir, device = 'png', width = 25, height = 15, units = 'cm', dpi = 300)

g1.3 <- ggplot(tab, aes(x = method_scale, y = cost/euc.dist, color = scenario)) + 
  stat_summary(geom="errorbar", fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), 
               position = position_dodge(width = .7), size = 1, width = 0) +
  # stat_summary(geom="line", fun.y=mean, linetype="dashed")+
  stat_summary(geom="point", fun = mean, size = 3, position = position_dodge(width = 0.7)) +
  # facet_wrap(~sp, scales = 'free') +
  theme_bw() +
  scale_color_manual(values = c("black", "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = c(.02, .98),
        legend.justification = c("left", "top"),
        legend.margin = margin(0, 1, 0, 2),
        legend.background = element_rect(colour = "black")) +
  labs(x = 'Bird pecies', y = 'Relative corridor cost', color = NULL)
g1.3
ggsave('relative_corridor_cost_avg_ci.png', plot = g1.3, path = outdir, device = 'png', 
       width = 15, height = 12, units = 'cm', dpi = 400)
  
tab$st <- paste(tab$source, tab$target, sep = '->') %>% as.factor
g2 <- ggplot() +
  geom_boxplot(data = tab, aes(x = scenario, y = cost/euc.dist)) + 
  facet_wrap(st~sp, scales = 'free') +
  theme_minimal() +
  labs(x = '', y = 'Relative corridor cost')
g2
ggsave('relative_corridor_cost_each_st.png', plot = g2, path = outdir, device = 'png', width = 30, height = 30, units = 'cm', dpi = 300)

#----------
# Tortuosity

# Original output
ggplot() +
  geom_jitter(data = tab.st, aes(x = zone, y = CORRIDOR_LENGTH/EUCLIDEAN_DISTANCE)) + 
  facet_wrap(~method_scale) + 
  labs(x = '', y = 'Tortuosity')

ggplot() +
  geom_jitter(data = tab.st, aes(x = zone, y = CORRIDOR_LENGTH/EUCLIDEAN_DISTANCE)) + 
  facet_wrap(~method_scale, scales = 'free') + 
  labs(x = '', y = 'Tortuosity')

g2 <- ggplot() +
  geom_boxplot(data = tab.st, aes(x = zone, y = CORRIDOR_LENGTH/EUCLIDEAN_DISTANCE)) + 
  facet_wrap(~method_scale) +
  labs(x = '', y = 'Tortuosity')
g2

# Output without STs
ggplot() +
  geom_jitter(data = tab, aes(x = scenario, y = euc.dist/tot.dist)) + 
  facet_wrap(~method_scale) + 
  labs(x = '', y = 'Straightness index')

ggplot() +
  geom_jitter(data = tab, aes(x = scenario, y = euc.dist/tot.dist)) + 
  facet_wrap(~method_scale, scales = 'free') + 
  labs(x = '', y = 'Straightness index')

g2 <- ggplot() +
  geom_boxplot(data = tab, aes(x = scenario, y = euc.dist/tot.dist)) + 
  facet_wrap(~method_scale, scales = 'free') +
  theme_minimal() +
  labs(x = '', y = 'Straightness index')
g2
ggsave('straightness.png', plot = g2, path = outdir, device = 'png', width = 25, height = 15, units = 'cm', dpi = 300)

tab[tab$tot.dist < tab$euc.dist,]
tab.test <- tab[tab$tot.dist >= tab$euc.dist,] %>% droplevels()

g3 <- ggplot() +
  geom_boxplot(data = tab.test, aes(x = scenario, y = euc.dist/tot.dist)) + 
  facet_wrap(~method_scale, scales = 'free') +
  theme_minimal() +
  labs(x = '', y = 'Straightness index')
g3
ggsave('straightness.png', plot = g3, path = outdir, device = 'png', width = 25, height = 15, units = 'cm', dpi = 300)

# Summary
tab.test.summ <- tab.test %>% 
  group_by(method_scale, scenario) %>% 
  summarise(
    avg.str = mean(euc.dist/tot.dist),
    err.str = sd(euc.dist/tot.dist)/sqrt(n()),
    supp = avg.str + 1.96*err.str,
    inff = avg.str - 1.96*err.str
  ) %>% 
  dplyr::filter(!is.na(scenario))

tab.test.summ$method_scale <- droplevels(tab.test.summ$method_scale)

# percentual difference
tab.test.summ$avg.orig <- rep(tab.test.summ$avg.str[seq(1,9,2)], each = 2)
tab.test.summ$diff.scenario <- tab.test.summ$avg.str/tab.test.summ$avg.orig
tab.test.summ$diff.sp.aleuco <- tab.test.summ$avg.str/tab.test.summ$avg.str[1]

tab.test.summ %>% 
  write.csv(file = paste0(outdir, "straightness_compare_zone_nonoze.csv"))

g4 <- ggplot(data = tab.test.summ) +
  geom_point(aes(x = scenario, y = avg.str)) + 
  geom_errorbar(aes(x = scenario, ymin = inff, ymax = supp)) +
  facet_wrap(~method_scale, scales = 'free') +
  theme_minimal() +
  ylim(0.78, 1) +
  labs(x = '', y = 'Straightness index')
g4
ggsave('straightness.png', plot = g4, path = outdir, device = 'png', width = 25, height = 15, units = 'cm', dpi = 300)

(a <- glm(euc.dist/tot.dist ~ scenario * sp, data = tab.test)) %>% summary
cf <- coef(a)
cf[3:6] <- cf[3:6] + cf[1]
cf[7:10] <- cf[7:10] + cf[1] + cf[2]
cf[2] <- cf[2] + cf[1]
cf

glm(euc.dist/tot.dist ~ scenario -1, data = tab.test[tab.test$sp == unique(tab.test$sp)[1],]) %>% summary
glm(euc.dist/tot.dist ~ scenario -1, data = tab.test[tab.test$sp == unique(tab.test$sp)[2],]) %>% summary
glm(euc.dist/tot.dist ~ scenario -1, data = tab.test[tab.test$sp == unique(tab.test$sp)[3],]) %>% summary
glm(euc.dist/tot.dist ~ scenario -1, data = tab.test[tab.test$sp == unique(tab.test$sp)[4],]) %>% summary
glm(euc.dist/tot.dist ~ scenario -1, data = tab.test[tab.test$sp == unique(tab.test$sp)[6],]) %>% summary

g5 <- ggplot(tab.test %>% dplyr::filter(!is.na(method_scale)), aes(x = method_scale, y = euc.dist/tot.dist, color = scenario)) + 
  stat_summary(geom="errorbar", fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), 
               position = position_dodge(width = .7), size = 1, width = 0) +
  # stat_summary(geom="line", fun.y=mean, linetype="dashed")+
  stat_summary(geom="point", fun = mean, size = 3, position = position_dodge(width = 0.7)) +
  # facet_wrap(~sp, scales = 'free') +
  theme_bw() +
  scale_color_manual(values = c("black", "grey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = c(.02, .98),
        legend.justification = c("left", "top"),
        legend.margin = margin(0, 1, 0, 2),
        legend.background = element_rect(colour = "black")) +
  labs(x = 'Bird species', y = 'Straightness index', color = NULL)
g5
ggsave('FigS2_5_straightness_avg_ci_methods.png', plot = g5, path = outdir, device = "png", 
       width = 15, height = 12, units = 'cm', dpi = 400)


#---- label=overlap_maps ----------------------------------------------------------------


#---------
# example for 1-d

a <- rnorm(10000, 0, 1)
b <- rnorm(10000, 1, 1)
plot(a, b)

breaks <- seq(-7, 7, 0.5)

hist(a, breaks = breaks)
hist(b, breaks = breaks, col = 2, add = T)

a.t <- cut(a, breaks) %>% table
b.t <- cut(b, breaks) %>% table

data.frame(cbind(a.t, b.t)) %>% 
  mutate(a.t.p = a.t/sum(a.t),
         b.t.p = b.t/sum(b.t),
         over = pmin(a.t.p,b.t.p)) %>% 
  colSums()

#------
# example for one species
plot(corridors_nozone_nost[[1]])
plot(corridors_zone_nost[[1]])

nz.prob <- corridors_nozone_nost[[1]]/cellStats(corridors_nozone_nost[[1]], sum)
# sum(nz.prob[], na.rm = T) # ok, sum = 1
# plot(nz.prob)

z.prob <- corridors_zone_nost[[1]]/cellStats(corridors_zone_nost[[1]], sum)
# sum(z.prob[], na.rm = T) # ok, sum = 1
# plot(z.prob)

ext <- raster::union(extent(nz.prob), extent(z.prob))

nz.prob2 <- crop(extend(nz.prob, z.prob), z.prob)
plot(nz.prob2)

stack.prob <- raster::stack(nz.prob2, z.prob)
plot(pmin(stack.prob))

diff <- z.prob
diff[] <- pmin(nz.prob2[], z.prob[])
plot(log(diff))

cellStats(diff, sum)

# for all species, comparindg the two scenarios
overlap.zone <- list()
overlap.zone.rast <- list()
for(i in 1:length(corridors_nozone_nost)) {
  
  nz.prob <- corridors_nozone_nost[[i]]/cellStats(corridors_nozone_nost[[i]], sum)
  # sum(nz.prob[], na.rm = T) # ok, sum = 1
  # plot(nz.prob)
  
  z.prob <- corridors_zone_nost[[i]]/cellStats(corridors_zone_nost[[i]], sum)
  # sum(z.prob[], na.rm = T) # ok, sum = 1
  # plot(z.prob)
  
  ext <- raster::union(extent(nz.prob), extent(z.prob))
  nz.prob2 <- crop(extend(nz.prob, z.prob), z.prob)
  
  diff <- z.prob
  diff[] <- pmin(nz.prob2[], z.prob[])
  plot(log(diff), main = i)
  
  over <- cellStats(diff, sum)
  
  overlap.zone.rast[[i]] <- diff
  overlap.zone[[i]] <- over
}
names(overlap.zone.rast) <- sp
names(overlap.zone) <- sp

(overlap.zone <- data.frame(overlap = unlist(overlap.zone)))
overlap.zone

# between species, no zoning scenario
overlap.sps.nozone.rast <- list()
overlap.sps.nozone <- matrix(NA, length(corridors_nozone_nost), length(corridors_nozone_nost),
                                  dimnames = list(sp, sp))
for(i in 1:length(corridors_nozone_nost)) {
  
  list.corr.rasts <- list()
  
  for(j in 1:length(corridors_nozone_nost)) {
    
    if(i >= j) {
    nz.prob <- corridors_nozone_nost[[i]]/cellStats(corridors_nozone_nost[[i]], sum)
    # sum(nz.prob[], na.rm = T) # ok, sum = 1
    # plot(nz.prob)
  
    z.prob <- corridors_nozone_nost[[j]]/cellStats(corridors_nozone_nost[[j]], sum)
    # sum(z.prob[], na.rm = T) # ok, sum = 1
    # plot(z.prob)
    
    ext <- raster::union(extent(nz.prob), extent(z.prob))
    nz.prob2 <- crop(extend(nz.prob, z.prob), z.prob)
    
    diff <- z.prob
    diff[] <- pmin(nz.prob2[], z.prob[])
    plot(log(diff), main = i)
    
    over <- cellStats(diff, sum)
    
    list.corr.rasts[[j]] <- diff
    overlap.sps.nozone[i,j] <- over
    }
  }
  
  overlap.sps.nozone.rast[[i]] <- list.corr.rasts
}
names(overlap.sps.nozone.rast) <- sp
overlap.sps.nozone

mean(overlap.sps.nozone[5,1:4])

# between species, zoning scenario
overlap.sps.zone.rast <- list()
overlap.sps.zone <- matrix(NA, length(corridors_zone_nost), length(corridors_zone_nost),
                             dimnames = list(sp, sp))
for(i in 1:length(corridors_zone_nost)) {
  
  list.corr.rasts <- list()
  
  for(j in 1:length(corridors_zone_nost)) {
    
    if(i >= j) {
      nz.prob <- corridors_zone_nost[[i]]/cellStats(corridors_zone_nost[[i]], sum)
      # sum(nz.prob[], na.rm = T) # ok, sum = 1
      # plot(nz.prob)
      
      z.prob <- corridors_zone_nost[[j]]/cellStats(corridors_zone_nost[[j]], sum)
      # sum(z.prob[], na.rm = T) # ok, sum = 1
      # plot(z.prob)
      
      ext <- raster::union(extent(nz.prob), extent(z.prob))
      nz.prob2 <- crop(extend(nz.prob, z.prob), z.prob)
      
      diff <- z.prob
      diff[] <- pmin(nz.prob2[], z.prob[])
      plot(log(diff), main = i)
      
      over <- cellStats(diff, sum)
      
      list.corr.rasts[[j]] <- diff
      overlap.sps.zone[i,j] <- over
    }
  }
  
  overlap.sps.zone.rast[[i]] <- list.corr.rasts
}
names(overlap.sps.zone.rast) <- sp
overlap.sps.zone

mean(overlap.sps.zone[5,1:4])

# compare
overlap.sps.nozone
overlap.sps.zone

# export overlap
overlap.zone %>% 
  write.table(file = paste0(outdir, 'overlap_zone.csv'), sep = ';', row.names = T)

overlap.sps.nozone %>% 
  write.table(file = paste0(outdir, 'overlap_sps_nozone.csv'), sep = ';', row.names = T)

overlap.sps.zone %>% 
  write.table(file = paste0(outdir, 'overlap_sps_zone.csv'), sep = ';', row.names = T)
