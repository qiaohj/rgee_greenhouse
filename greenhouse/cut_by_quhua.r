library(data.table)
library(terra)
library(sf)
setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")

files<-list.files("../Data/climate_mean", patter="\\.tif")


env<-rast(sprintf("../Data/climate_mean/%s", files))
quhua<-read_sf("../Shape/Climate_quhua/Climate_quhua.shp")
quhua<-st_transform(quhua, crs=st_crs(env))

for (i in c(unique(quhua$code1))){
  
  target<-sprintf("../Data/climate_mean_quhua/%d", i)
  if (!dir.exists(target)){
    dir.create(target)
  }
  item<-quhua[which(quhua$code1==i),]
  crop<-crop(env, item)
  mask<-mask(crop, item)
  for (j in c(1:nlyr(mask))){
    print(sprintf("writing %d, %s", i, files[j]))
    r<-mask[[j]]
    writeRaster(r, sprintf("%s/%s", target, files[j]), overwrite=T)
  }
}

greenhouse<-rast("../Data/greenhouse_sum_wrap.tif")
for (i in c(unique(quhua$code1))){
  print(i)
  item<-quhua[which(quhua$code1==i),]
  crop<-crop(greenhouse, item)
  mask<-mask(crop, item)
  writeRaster(mask, sprintf("../Data/greenhouse_quhua/%d.tif", i), overwrite=T)
}
