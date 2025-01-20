library(terra)
setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")

#files<-list.files("../Data/bioclim_full", pattern="\\.tif")
files<-list.files("../Data/bioclim_quhua_greenhouse", pattern="\\.tif")
f<-files[1]
for (f in files){
  r<-rast(sprintf("../Data/bioclim_quhua_greenhouse/%s", f))
  r2<-r
  res(r2)<-c(0.1, 0.1)
  r_new<-resample(r, r2)
  writeRaster(r_new, sprintf("../Data/bioclim_quhua_greenhouse_low/%s", f), overwrite=T)
}
