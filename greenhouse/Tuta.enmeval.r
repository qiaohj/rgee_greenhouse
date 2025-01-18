library(ENMeval)
library(geodata)
library(usdm)
library(blockCV)
library(sf)
library(tibble)
library(ggplot2)
library(knitr)
library(terra)
library(dplyr)
library(data.table)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")

occs<-readRDS("../Data/Tuta/occs_lonlat.rda")

envs_files<-list.files("../Data/Tuta/bioclim_full", pattern="\\.tif")
envs<-rast(sprintf("../Data/Tuta/bioclim_full/%s", envs_files))
names(envs) <- gsub("\\.tif", "", envs_files)
plot(envs[[1]])
if (F){
  # Now we will remove those variables from consideration that have a high VIF.
  envs.vif <- usdm::vifstep(envs)
  saveRDS(envs.vif, "../Data/Tuta/envs.vif.rda")
  envs.rem <- envs.vif@excluded
  saveRDS(envs.rem, "../Data/Tuta/envs.rem.rda")
}
envs.rem<-readRDS("../Data/Tuta/envs.rem.rda")
envs <- envs[[!(names(envs) %in% envs.rem)]]


# Let's now remove occurrences that are cell duplicates -- these are
# occurrences that share a grid cell in the predictor variable rasters.
# Although Maxent does this by default, keep in mind that for other algorithms you may
# or may not want to do this based on the aims of your study.
# Another way to space occurrence records a defined distance from each other to avoid
# spatial autocorrelation is with spatial thinning (Aiello-Lammens et al. 2015).
occs.cells <- terra::extract(envs, data.frame(occs), cellnumbers = TRUE, ID = FALSE)
occs.cells$lon<-occs$lon
occs.cells$lat<-occs$lat
occs.cells<-data.table(occs.cells)
occs.cells<-occs.cells[!is.na(bio1)]

# Randomly sample 10,000 background points from one background extent raster 
# (only one per cell without replacement). 
bg <- terra::spatSample(envs, size = 10000, na.rm = TRUE, 
                        values = FALSE, xy = TRUE) |> as.data.frame()
colnames(bg) <- colnames(occs)

# Notice how we have pretty good coverage (every cell).
plot(envs[[1]])
points(bg, pch = 20, cex = 0.2)

cb1 <- get.checkerboard(occs, envs, bg, aggregation.factor = 25)
# Plotting the background points shows the checkerboard pattern clearly.
evalplot.grps(pts = occs, pts.grp = cb1$occs.grp, envs = envs)

rand <- get.randomkfold(occs, bg, k = 5)
evalplot.grps(pts = occs, pts.grp = rand$occs.grp, envs = envs)

saveRDS(bg, "../Data/Tuta/bg.rda")
env_serialize<-serialize(envs, NULL)
saveRDS(env_serialize, "../Data/Tuta/env_picked.rda")


occs<-readRDS("../Data/Tuta/occs_lonlat.rda")
occs<-as.data.frame(occs)
envs<-readRDS("../Data/Tuta/env_picked.rda")
envs<-unserialize(envs)
bg<-readRDS("../Data/Tuta/bg.rda")
algorithm = 'maxnet'
ps <- list(kfolds = 5)
partition.settings = ps

e.mx.l <- ENMevaluate(occs = occs, envs = envs, bg = bg, 
                      algorithm = 'maxent.jar', partitions = 'randomkfold', 
                      partition.settings = ps,
                      raster.preds = F,
                      tune.args = list(fc = c("L", "Q", "P", "LQ", "LP",
                                              "QP", "LQP"), 
                                       rm = c(0.01, 0.1, 0.5, c(1:10))))


