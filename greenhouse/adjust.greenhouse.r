library(terra)
setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")
greenhouse_differ<-readRDS("../Data/quhua.greenhouse.differ.rda")
quhua_data<-greenhouse_differ[, .(N=.N), by=list(quhua, code, sensor_type)]

greenhouse_differ<-greenhouse_differ[quhua %in% c(quhua_data[N==12]$quhua)]

quhua_env_raw<-"../Data/climate_mean_quhua/%d/%s_%d.tif"
quhua_env_greenhouse<-"../Data/climate_mean_quhua_greenhouse/%d/%s_%d.tif"
greenhouse_tif<-"../Data/greenhouse_quhua/%d.tif"
options(warn=2)
for (gh in unique(greenhouse_differ$code)){
  gh_r<-rast(sprintf(greenhouse_tif, gh))
  v_gh<-values(gh_r)
  v_gh[is.na(v_gh)]<-0
  for (m in c(1:12)){
    max_differ_temp<-greenhouse_differ[code==gh & month==m & sensor_type=="temp/C"]$max_differ
    min_differ_temp<-greenhouse_differ[code==gh & month==m & sensor_type=="temp/C"]$min_differ
    differ_humi<-greenhouse_differ[code==gh & month==m & sensor_type=="humi/%"]$mean_differ
    
    differ<-data.frame(hurs=differ_humi, tasmax=max_differ_temp, tasmin=min_differ_temp)
    for (var in c("hurs", "tasmax", "tasmin")){
      print(paste(gh, m, var))
      raw_raster<-rast(sprintf(quhua_env_raw, gh, var, m))
      differ_v<-differ[, var]
      if (var=="hurs"){
        values(raw_raster)<-values(raw_raster)+differ_v
      }else{
        values(raw_raster)<-values(raw_raster)+differ_v*10
      }
      writeRaster(raw_raster, sprintf(quhua_env_greenhouse, gh, var, m), overwrite=T)
    }
    
  }
}


#Generate bioclim

for (gh in unique(greenhouse_differ$code)){
  tmax<-rast(sprintf("../Data/climate_mean_quhua_greenhouse/%d/tasmax_%d.tif", gh, c(1:12)))
  tmin<-rast(sprintf("../Data/climate_mean_quhua_greenhouse/%d/tasmin_%d.tif", gh, c(1:12)))
  prec<-rast(sprintf("../Data/climate_mean_quhua_greenhouse/%d/hurs_%d.tif", gh, c(1:12)))
  tavg <- (tmin + tmax) / 2
  # P1. Annual Mean Temperature 
  bio1<-mean(tavg, na.rm=T)
  writeRaster(bio1, sprintf("../Data/bioclim_%s/bio1_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  # P2. Mean Diurnal Range(Mean(period max-min)) 
  bio2<-mean(tmax-tmin, na.rm=T)
  writeRaster(bio2, sprintf("../Data/bioclim_%s/bio2_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  # P4. Temperature Seasonality (standard deviation) 
  #bio4<-sd(tavg, na.rm=T) * 100
  #writeRaster(bio4, sprintf("../Data/bioclim_%s/bio4.tif", names(conf)[i]), overwrite=T)
  # P5. Max Temperature of Warmest Period 
  bio5 <- max(tmax, na.rm=T)
  writeRaster(bio5, sprintf("../Data/bioclim_%s/bio5_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  # P6. Min Temperature of Coldest Period 
  bio6 <- min(tmin, na.rm=T)
  writeRaster(bio6, sprintf("../Data/bioclim_%s/bio6_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  # P7. Temperature Annual Range (P5-P6) 
  bio7<-bio5-bio6
  writeRaster(bio7, sprintf("../Data/bioclim_%s/bio7_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  # P3. Isothermality (P2 / P7) 
  bio3<-100*bio2/bio7
  writeRaster(bio3, sprintf("../Data/bioclim_%s/bio3_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  # P12. Annual Precipitation 
  
  # P13. Precipitation of Wettest Period 
  bio13<-max(prec, na.rm=T)
  writeRaster(bio13, sprintf("../Data/bioclim_%s/bio13_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  # P14. Precipitation of Driest Period 
  bio14<-min(prec, na.rm=T)
  writeRaster(bio14, sprintf("../Data/bioclim_%s/bio14_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  # P15. Precipitation Seasonality( Coefficient of Variation) 
  bio12<-sum(prec, na.rm=T)
  writeRaster(bio12, sprintf("../Data/bioclim_%s/bio12_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  #bio15<-cv(prec+1, na.rm=T)
  #writeRaster(bio15, sprintf("../Data/bioclim_%s/bio15.tif", names(conf)[i]), overwrite=T)
  
  tavg_df<-data.table(as.data.frame(tavg))
  tavg_stdev <- apply(tavg_df, 1, sd, na.rm=TRUE)
  bio4<-tmax[[1]]
  values(bio4)[!is.na(values(bio4))]<-tavg_stdev * 100
  writeRaster(bio4, sprintf("../Data/bioclim_%s/bio4_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
  
  prec_df<-data.table(as.data.frame(prec))
  pavg_stdev <- apply(prec_df, 1, sd, na.rm=TRUE)
  
  
  cv_prec <- (pavg_stdev / (values(bio12)[!is.na(values(bio12))]+1)) * 100
  bio15<-prec[[1]]
  values(bio15)[!is.na(values(bio15))]<-cv_prec
  writeRaster(bio15, sprintf("../Data/bioclim_%s/bio15_qh_%d.tif", "quhua_greenhouse", gh), overwrite=T)
}
