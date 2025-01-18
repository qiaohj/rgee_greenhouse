library(gdalUtilities)
library(terra)
setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")
files<-list.files("/media/huijieqiao/Butterfly/rgee_greenhouse/Data/global_greenhouse", 
                  pattern="\\.tif", 
                  full.names = T, include.dirs=F)
v <- vrt(files, "/media/huijieqiao/Butterfly/rgee_greenhouse/Data/greenhouse.vrt", overwrite=T)

r <- rast("/media/huijieqiao/Butterfly/rgee_greenhouse/Data/greenhouse.vrt")
plot(r)
r_ch<-rast("/media/huijieqiao/WD20T_50/chelse_cmip6/CHELSA_tasmin_12_2017_V.2.1.tif")

t1 <- c(xmin(r), ymin(r), xmax(r), ymax(r))  #coordenadas de la imagen destino
t2 <- c(res(r_ch)[1], res(r_ch)[2])  #resolucion de la imagen destino
#----------------------------------------------------------------

#gdalwarp greenhouse.vrt greenhouse_near_wrap.tif -tr 0.008333333 0.008333333 -r near -dstnodata -9999 -overwrite
#gdalwarp greenhouse.vrt greenhouse_sum_wrap.tif -tr 0.008333333 0.008333333 -r sum -dstnodata -9999 -overwrite

#gdalwarp(dstfile = "/media/huijieqiao/Butterfly/rgee_greenhouse/Data/greenhouse_near.tif", 
#         tr = t2, te = t1,
#         output_Raster = T, overwrite = T, verbose = T)

gh<-rast("../Data/greenhouse_near_wrap.tif")
plot(gh)
