if (F){
  library(reticulate)
  Sys.which("python3")   # system default
  use_python(Sys.which("python3"))  # use it
  Sys.getenv("RETICULATE_PYTHON")
  # use the standard Python numeric library
  np <- reticulate::import("numpy", convert = FALSE)
  # do some array manipulations with NumPy
  a <- np$array(c(1:4))
  print(a)  # this should be a Python array
  print(py_to_r(a))  # this should be an R array
  
  library(rgee)
  ee_install()
  rgee::ee_install_set_pyenv(py_path = "/usr/bin/python3", py_env="rgee")
  ee_check()
  # set Python version to use
  py_config()
}
#ee_install(py_env = "rgee")
library(rgee)
library(sf)
ee_check()
ee_Initialize()


# Initialize Earth Engine

shp_sf<-st_read("../Data/2019全国农业塑料大棚遥感分类数据集 V2/大棚样本数据集/山东省/大棚样本数据/Shandong_Part1_AG.kml")
plot(shp_sf$geometry)
shp<-sf_as_ee(shp_sf)

#Load and preprocess Sentinel-2 images that overlay the shapefile for a given date range.
#Then select scenes and clip the mosaic of images (watch previous videos for details):
s2_sr <- ee$ImageCollection('COPERNICUS/S2_SR')$filterBounds(shp)$
  filterDate('2020-01-01', '2020-02-29')
s2_sr <- s2_sr$filter(ee$Filter$lt('CLOUD_COVERAGE_ASSESSMENT', 10))

listOfImages <- s2_sr$toList(s2_sr$size())
selected_images <- ee$ImageCollection$fromImages(list(
  ee$Image(listOfImages$get(5)), ee$Image(listOfImages$get(6)), 
  ee$Image(listOfImages$get(8)), ee$Image(listOfImages$get(9)),
  ee$Image(listOfImages$get(10)), ee$Image(listOfImages$get(11))))
mosaic <- selected_images$mosaic()$clip(shp)
#Select only Sentinel-2 bands B1 through B8, B8A, B9, B11 and B12:
mosaic <- mosaic$select(c(paste0('B', 1:9), 'B8A', 'B11', 'B12'))

#Divide the pixel values by 10000 to scale them to a 0-1 range:
rescaled <- mosaic$divide(ee$Image$constant(10000))
ndvi <- rescaled$select('B8')$subtract(rescaled$select('B4'))$
  divide(rescaled$select('B8')$add(rescaled$select('B4')))
ndvi <- ndvi$rename('ndvi')

nbr <- rescaled$normalizedDifference(c('B8', 'B12'))$rename('nbr')

evi <- rescaled$expression(
  expression = '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', 
  opt_map = list(
    'NIR' = rescaled$select('B8'),
    'RED' = rescaled$select('B4'),
    'BLUE' = rescaled$select('B2')
  ))$rename('evi')
#Add spectral indices as bands to the image:
image <- rescaled$addBands(ndvi)$addBands(nbr)$addBands(evi)
## Extract training pixel values 

#Sample pixels from the image to help the training:
  
training <- image$sample(
  region = shp,
  scale = 18,
  numPixels = 5000
)
training$getInfo()
## Perform unsupervised classification

#1. k-mean classification:
  
#  Train the k-mean algorithm with `ee$Clusterer$wekaKMeans()`:
clusterer <- ee$Clusterer$wekaKMeans(3)$train(training)

#Perform the k-means unsupervised classification on the image:
  
kmeans_class <- image$cluster(clusterer)

#Visualize the image and the resulting unsupervised classification:
  
imageVisParam <- list(bands = c('B4', 'B3', 'B2'), min = 0, max = 0.2)
Map$centerObject(shp)
Map$addLayer(image, imageVisParam, 'image') + 
  Map$addLayer(kmeans_class$randomVisualizer(), name = 'k-means')+
  Map$addLayer(shp, name = 'shp')+
  Map$addLayer(selected_images$mosaic(), name = 'selected_images')+
  Map$addLayer(training, name = 'mosaic')

print(training$getInfo())
