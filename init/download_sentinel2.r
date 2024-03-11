library(rgee)
library(sf)
library(ggplot2)

setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")
#setwd("~/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")
# ee_reattach() # reattach ee as a reserved word
args = commandArgs(trailingOnly=TRUE)

user_index<-as.numeric(args[1])
if (is.na(user_index)){
  user_index<-1
}
if (user_index==1){
  user_str<-"qiaohj.others@gmail.com"
  ee_Initialize(drive = TRUE)
}
if (user_index==2){
  user_str<-"aws.20210901@gmail.com"
  ee_Initialize(user=user_str, drive = TRUE)
}
if (user_index==3){
  user_str<-"rgee20221020.1@gmail.com"
  ee_Initialize(user=user_str, drive = TRUE)
}
if (user_index==4){
  user_str<-"huijieqiao@gmail.com"
  ee_Initialize(user=user_str, drive = TRUE)
}

maskS2clouds <- function(image) {
  qa <- image$select("QA60")
  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloudBitMask <- bitwShiftL(1, 10)
  cirrusBitMask <- bitwShiftL(1, 11)
  
  # Both flags should be set to zero, indicating clear conditions.
  mask <- qa$bitwiseAnd(cloudBitMask)$eq(0)$And(
    qa$bitwiseAnd(cirrusBitMask)$eq(0)
  )
  
  # Return the masked and scaled data, without the QA bands.
  image$updateMask(mask)$
    divide(10000)$
    select("B.*")$
    copyProperties(image, list("system:time_start"))
}


grid_china<-st_read("../Shape/Towns/grid_china.shp")
st_crs(grid_china)<-st_crs("+proj=utm +zone=54 +south +datum=WGS84 +units=m +no_defs")
#https://gisgeography.com/sentinel-2-bands-combinations/
# Map the function over one year of data and take the median.
# Load Sentinel-2 TOA reflectance data.
month<-1
for (month in c(1:12)){
  date_froms<-sprintf("2021-%02d-01", c(1:12))
  date_tos<-sprintf("2021-%02d-%d", c(1:12), c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  folder<-sprintf("2021%02d", month)
  target_folder<-sprintf("../Data/Sentinel2/%s", folder)
  if (!dir.exists(target_folder)){
    dir.create(target_folder, recursive = T)
  }
  collection <- ee$ImageCollection("COPERNICUS/S2")$
    filterDate(date_froms[month], date_tos[month])$
    filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20))$
    map(maskS2clouds)
  
  #ee_print(collection$select("B3"))
  #ee_print(median)
  
  median<-collection$reduce(ee$Reducer$median())
  
  ID<-1668
  if (F){
    plot(grid_china$geometry)
    plot(grid_china[ID,]$geometry, col="red", add=T)
  }
  IDs<-unique(grid_china$ID)
  IDs<-IDs[sample(length(IDs), length(IDs))]
  for (ID in IDs){
    target_file<-sprintf("%s/%d.tif", target_folder, ID)
    if (file.exists(target_file)){
      next()
    }
    print(paste(month, ID))
    saveRDS(NULL, target_file)
    grid<-grid_china[which(grid_china$ID==ID),]
    #grid<-grid_china[which(grid_china$ID %in% seq(ID, ID+9)),]
    
    ee_grid<-sf_as_ee(grid, proj="EPSG:32754")
    
    training<-median$
      clip(ee_grid$geometry())
    
    task_vector <- ee_image_to_drive(
      training,
      folder = sprintf("sentinel2_%s_%d", folder, ID),
      fileFormat = "GeoTIFF",
      fileNamePrefix = sprintf("sentinel2_%s_%d", folder, ID),
      crs = "EPSG:32754",
      scale=10,
      region=ee_grid$geometry(),
      maxPixels=1e10
    )
    task_vector$start()
    status<-""
    while(T){
      status<-task_vector$status()$state
      print(sprintf("Current status is %s @ %s", status, user_str))
      if (status=="COMPLETED"){
        break()
      }
      if (status=="FAILED"){
        break()
      }
      if (status=="CANCEL_REQUESTED"){
        break()
      }
      if (status=="CANCELED"){
        break()
      }
      
      print("system sleeping")
      Sys.sleep(60)
    }
    if (status=="COMPLETED"){
      succeed<-F
      while(T){
        tryCatch(expr = {
          ee_drive_to_local(task = task_vector, dsn = target_file)
          succeed<-T
        },error = function(e) {
          print("Error downloading, retry...")
        }
        
        )
        if (succeed){
          print("Cleaning data from drive...")
          ee_clean_container(name = sprintf("sentinel2_%s_%d", folder, ID), type = "drive", quiet = FALSE)
          break()
        }  
      }
      
    }
    if (F){
      library(raster)
      r<-raster(target_file)
      r
    }
  }
}
