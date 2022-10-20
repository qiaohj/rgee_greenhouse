library(rgee)
library(sf)
library(ggplot2)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize(drive = TRUE)
# Input imagery is a cloud-free Landsat 8 composite.

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

# Map the function over one year of data and take the median.
# Load Sentinel-2 TOA reflectance data.
collection <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2021-01-01", "2021-01-31")$
  filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20))$
  map(maskS2clouds)

composite <- collection$median()

ee_print(composite)

ag_train_sf<-st_read("../Shape/AG_Trainingset/AG_Trainingset.shp")
ag_sf<-ag_train_sf[which(ag_train_sf$ZIP=="370000"),]
ag_sf<-st_cast(ag_sf, "POLYGON")
ag_sf$class<-ifelse(ag_sf$AG=="YES", 1, 0)
ag<-sf_as_ee(ag_sf)
centroid<-st_centroid(ag_sf)
centroid<-st_coordinates(centroid)
# Make a FeatureCollection from the hand-made geometries.
polygons<-ag
if (F){
  ee_print(ag)
  polygons = ee$FeatureCollection(c(
    ee$Feature(ag$flatten(), list(class = 0)),
    ee$Feature(none_ag$flatten(), list(class = 1))
  ))
}

# Get the values for all pixels in each polygon in the training.

training<-composite$clip(ee$Geometry$bounds(polygons))
ee_image_to_
task_vector <- ee_image_to_drive(
  training,
  fileFormat = "GeoTIFF",
  fileNamePrefix = "train_370000",
  scale=10,
  maxPixels=5e9
)
task_vector$start()

while(T){
  status<-task_vector$status()$state
  print(sprintf("Current status is %s", status))
  if (status=="COMPLETED"){
    break()
  }
  print("system sleeping")
  Sys.sleep(60)
}
ee_drive_to_local(task = task_vector, dsn = "../Data/Train/train_370000.tif")


ggplot(train_set)+geom_sf(aes(color=class))
