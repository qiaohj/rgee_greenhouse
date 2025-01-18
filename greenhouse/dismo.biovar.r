library(terra)
library(stringr)
library(data.table)

setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")
if (F){
  y<-c(1979:2019)
  coms<-data.table(expand.grid(m=c(1:12), v=c("hurs", "tasmax", "tasmin")))
  base<-"../Data/climate"
  i=1
  for (i in c(1:nrow(coms))){
    target<-sprintf("../Data/climate_mean/%s_%d.tif", coms[i]$v, coms[i]$m)
    if (file.exists(target)){
      next()
    }
    print(target)
    saveRDS(NULL, target)
    files<-sprintf("%s/CHELSA_%s_%s_%d_V.2.1.tif",
                   base, coms[i]$v,
                   str_pad(coms[i]$m, 2, pad = "0"),
                   y)
    files<-files[file.exists(files)]
    r<-rast(files)
    r_mean<-mean(r, na.rm=T)
    writeRaster(r_mean, target, overwrite=T)
  }
}
if (F){
  v<-"tasmax"
  m<-1
  y<-c(1979:2019)
  target<-sprintf("../Data/climate_mean/%s_%d.tif", v, m)
  files<-sprintf("%s/CHELSA_%s_%s_%d_V.2.1.tif",
                 base, v,
                 str_pad(m, 2, pad = "0"),
                 y)
  files<-files[file.exists(files)]
  #files<-c(files, target)
  r<-rast(files)
  lonlat<-data.frame(x=115.7, y=39.4)
  rowMeans(extract(r, lonlat, ID=F), na.rm=T)
  r_mean<-rast(target)
  extract(r_mean, lonlat)
}
if (F){
  #check the files
  missing_files<-c()
  for (y in c(1980:2018)){
    for (m in c(1:12)){
      prec<-sprintf("%s/CHELSA_hurs_%s_%d_V.2.1.tif",
                    base, 
                    str_pad(m, 2, pad = "0"),
                    y)
      tasmax<-sprintf("%s/CHELSA_tasmax_%s_%d_V.2.1.tif",
                      base, 
                      str_pad(m, 2, pad = "0"),
                      y)
      tasmin<-sprintf("%s/CHELSA_tasmin_%s_%d_V.2.1.tif",
                      base, 
                      str_pad(m, 2, pad = "0"),
                      y)
      if (!file.exists(prec)){
        missing_files<-c(missing_files, prec)
      }
      if (!file.exists(tasmax)){
        missing_files<-c(missing_files, tasmax)
      }
      if (!file.exists(tasmin)){
        missing_files<-c(missing_files, tasmin)
      }
    }
  }
}
if (F){
  library(terra)
  library(stringr)
  library(data.table)
  
  setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")
  
  breading<-c(4:9)
  wintering<-c(1:3, 10:12)
  full<-c(1:12)
  
  conf<-list("breading"=breading,
             "wintering"=wintering,
             "full"=full)
  i=2
  #for (i in c(1:length(conf))){
    tmax<-rast(sprintf("../Data/climate_mean/tasmax_%d.tif", conf[[i]]))
    tmin<-rast(sprintf("../Data/climate_mean/tasmin_%d.tif", conf[[i]]))
    prec<-rast(sprintf("../Data/climate_mean/hurs_%d.tif", conf[[i]]))
    tavg <- (tmin + tmax) / 2
    # P1. Annual Mean Temperature 
    bio1<-mean(tavg, na.rm=T)
    writeRaster(bio1, sprintf("../Data/bioclim_%s/bio1.tif", names(conf)[i]), overwrite=T)
    # P2. Mean Diurnal Range(Mean(period max-min)) 
    bio2<-mean(tmax-tmin, na.rm=T)
    writeRaster(bio2, sprintf("../Data/bioclim_%s/bio2.tif", names(conf)[i]), overwrite=T)
    # P4. Temperature Seasonality (standard deviation) 
    #bio4<-sd(tavg, na.rm=T) * 100
    #writeRaster(bio4, sprintf("../Data/bioclim_%s/bio4.tif", names(conf)[i]), overwrite=T)
    # P5. Max Temperature of Warmest Period 
    bio5 <- max(tmax, na.rm=T)
    writeRaster(bio5, sprintf("../Data/bioclim_%s/bio5.tif", names(conf)[i]), overwrite=T)
    # P6. Min Temperature of Coldest Period 
    bio6 <- min(tmin, na.rm=T)
    writeRaster(bio6, sprintf("../Data/bioclim_%s/bio6.tif", names(conf)[i]), overwrite=T)
    # P7. Temperature Annual Range (P5-P6) 
    bio7<-bio5-bio6
    writeRaster(bio7, sprintf("../Data/bioclim_%s/bio7.tif", names(conf)[i]), overwrite=T)
    # P3. Isothermality (P2 / P7) 
    bio3<-100*bio2/bio7
    writeRaster(bio3, sprintf("../Data/bioclim_%s/bio3.tif", names(conf)[i]), overwrite=T)
    # P12. Annual Precipitation 
    
    # P13. Precipitation of Wettest Period 
    bio13<-max(prec, na.rm=T)
    writeRaster(bio13, sprintf("../Data/bioclim_%s/bio13.tif", names(conf)[i]), overwrite=T)
    # P14. Precipitation of Driest Period 
    bio14<-min(prec, na.rm=T)
    writeRaster(bio14, sprintf("../Data/bioclim_%s/bio14.tif", names(conf)[i]), overwrite=T)
    # P15. Precipitation Seasonality( Coefficient of Variation) 
    bio12<-sum(prec, na.rm=T)
    writeRaster(bio12, sprintf("../Data/bioclim_%s/bio12.tif", names(conf)[i]), overwrite=T)
    #bio15<-cv(prec+1, na.rm=T)
    #writeRaster(bio15, sprintf("../Data/bioclim_%s/bio15.tif", names(conf)[i]), overwrite=T)
    
    tavg_df<-data.table(as.data.frame(tavg))
    tavg_stdev <- apply(tavg_df, 1, sd, na.rm=TRUE)
    bio4<-tmax[[1]]
    values(bio4)<-tavg_stdev * 100
    writeRaster(bio4, sprintf("../Data/bioclim_%s/bio4.tif", names(conf)[i]), overwrite=T)
    
    prec_df<-data.table(as.data.frame(prec))
    pavg_stdev <- apply(prec_df, 1, sd, na.rm=TRUE)
    
    
    cv_prec <- (pavg_stdev / (values(bio12)+1)) * 100
    bio15<-prec[[1]]
    values(bio15)<-cv_prec
    writeRaster(bio15, sprintf("../Data/bioclim_%s/bio15.tif", names(conf)[i]), overwrite=T)
  #}

}

if (F){
  #pca
  library(terra)
  library(stringr)
  library(data.table)
  
  setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")
  
  breading<-c(4:9)
  wintering<-c(1:3, 10:12)
  full<-c(1:12)
  
  conf<-list("breading"=breading,
             "wintering"=wintering,
             "full"=full)
  i=2
  print("reading rasters")
  biovars<-rast(list.files(sprintf("../Data/bioclim_%s/", names(conf)[i]), pattern="\\.tif", full.names = T))
  print("converting to data.table")
  biovars_p<-data.table(as.data.frame(biovars))
  pca<-prcomp(biovars_p, scale=T)
  saveRDS(pca, sprintf("../Data/pca_%s.rda", names(conf)[i]))
  for (j in c(1:ncol(pca$x))){
    print(paste(i, j))
    rrr<-biovars[[1]]
    values(rrr)<-pca$x[,j]
    writeRaster(rrr, sprintf("../Data/pca_%s/pc.%d.tif", names(conf)[i], j), overwrite=T)
  }
  pca<-readRDS(sprintf("../Data/pca_%s.rda", names(conf)[i]))
  pca$x<-NA
  saveRDS(pca, sprintf("../Data/pca_%s.conf.rda", names(conf)[i]))
}
