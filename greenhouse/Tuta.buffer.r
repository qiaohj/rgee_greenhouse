library(ENMeval)
library(data.table)
library(terra)
library(sf)
library(ggplot2)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")

gbif<-readRDS("../Data/Tuta/Tuta_gbif_241227.rda")
gbif$source<-"GBIF"
jps<-readRDS("../Data/Tuta/Tuta_JPS_reference.rda")
colnames(jps)<-c("lon", "lat", "date")
jps$source<-"JPS"
ra<-readRDS("../Data/Tuta/Tuta_RA_reference.rda")

ecoregion<-read_sf("../Shape/Ecoregions2017/Ecoregions2017.shp")

occs<-rbindlist(list(gbif[, c("lon", "lat", "source")],
                     jps[, c("lon", "lat", "source")]))
occs<-unique(occs)
occs_lonlat<-occs
occs_lonlat$source<-NULL
occs_lonlat<-unique(occs_lonlat)
saveRDS(occs_lonlat, "../Data/Tuta/occs_lonlat.rda")
occs_p<-st_as_sf(x = occs,                         
                 coords = c("lon", "lat"),
                 crs = st_crs(ecoregion))

index<-st_contains(ecoregion, occs_p)
i=1
occs$eco_biom<-""
length(unique(ecoregion$ECO_BIOME_))
length(unique(ecoregion$ECO_NAME))

for (i in c(1:length(index))){
  item<-ecoregion[i,]
  if (length(index[[i]])>0){
    occs[index[[i]]]$eco_biom<-item$ECO_BIOME_
  }
}

eco_biom_N<-occs[, .(N=.N), by=eco_biom]

occs_p<-st_as_sf(x = occs,                         
                 coords = c("lon", "lat"),
                 crs = st_crs(ecoregion))
write_sf(occs_p, "../Data/Tuta/occs/occs.shp")
sf_use_s2(T)
occs_buffer<-st_buffer(occs_p, 2e5)
occs_buffer<-st_union(occs_buffer)
write_sf(occs_buffer, "../Data/Tuta/occs_buffer/occs_buffer.shp")

sf_use_s2(FALSE)
ecoregion_world<-st_union(ecoregion)
occs_buffer_2<-st_intersection(occs_buffer, ecoregion_world)
plot(occs_buffer_2)
occs_buffer<-vect(occs_buffer_2)


plot(occs_buffer)


for (label in c("full", "breading", "wintering")){
  target<-sprintf("../Data/bioclim_%s", label)
  file<-list.files(target, pattern="\\.tif")
  r<-rast(sprintf("%s/%s", target, file))
  
  crop<-crop(r, occs_buffer)
  mask<-mask(crop, occs_buffer)
  
  folder<-sprintf("../Data/Tuta/bioclim_%s", label)
  if (!dir.exists(folder)){
    dir.create(folder)
  }
  
  for (i in c(1:nlyr(r))){
    print(paste(label, i))
    r_item<-mask[[i]]
    writeRaster(r_item, sprintf("%s/%s", folder, file[i]), overwrite=T)
  }
}
ggplot()+geom_point(data=occs, aes(x=lon, y=lat, color=eco_biom))+
  geom_point(data=occs[eco_biom==""], aes(x=lon, y=lat), shape=2)


