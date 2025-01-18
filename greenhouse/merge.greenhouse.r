library(terra)
library(sf)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")
if (F){
  
  #gdal_translate -ot Byte -a_nodata 9 -projwin_srs EPSG:4326 greenhouse.vrt greenhouse.asc
  china<-read_sf("../Shape/Towns/China.shp")
  china<-st_transform(china, st_crs(r))
  china_s<-st_union(china)
  plot(china_s)
  write_sf(china_s, "../Shape/Towns/China.fixed.shp")
  plot(rast(files[1]))
  
  files<-list.files("/media/huijieqiao/Butterfly/rgee_greenhouse/Data/global_greenhouse", 
                    pattern="re_det_planet_256_nonorm_ps_PSScene4Band_2019*", 
                    full.names = T, include.dirs=F)
  files<-files[1:10]
  v <- vrt(files, "/media/huijieqiao/Butterfly/rgee_greenhouse/Data/test.vrt", overwrite=T)
  writeRaster(v, "/media/huijieqiao/Butterfly/rgee_greenhouse/Data/test.tif", overwrite=T)
}
r <- rast("/media/huijieqiao/Butterfly/rgee_greenhouse/Data/greenhouse.vrt")
r_ch<-rast("/media/huijieqiao/WD20T_50/chelse_cmip6/CHELSA_tasmin_12_2017_V.2.1.tif")
r_1km<-resample(r, r_ch, method="sum")
writeRaster(r_1km, "../Data/greenhouse.1km.sum.tif", overwrite=T)
r_1km<-resample(r, r_ch, method="near")
writeRaster(r_1km, "../Data/greenhouse.1km.near.tif", overwrite=T)


r_blank<-rast(nrows=nrow(r), ncols=ncol(r), 
              xmin=xmin(r), xmax=xmax(r), 
              ymin=ymin(r), ymax=ymax(r))



r_china<-crop(r, st_bbox(china_s))
r<-rast(files[1])

china_s<-read_sf("../Shape/Towns/China.fixed.shp")


vrt

r_china<-crop(r, st_bbox(china_s))
