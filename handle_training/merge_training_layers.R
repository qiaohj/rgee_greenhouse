library(sf)
library(data.table)
setwd("~/git/rgee_greenhouse/rgee_greenhouse")
folders<-list.dirs("../Data/2019全国农业塑料大棚遥感分类数据集 V2/大棚样本数据集", recursive = F)
zip<-fread("../Data/province.txt")
folder<-folders[3]
all_sets<-data.frame()
for (folder in folders){
  zip_item<-zip[sapply(zip$PROVINCE, grepl, folder)]
  print(zip_item$PROVINCE)
  ag_files<-list.files(sprintf("%s/大棚样本数据", folder), pattern="_AG.*\\.kml")
  non_ag_files<-list.files(sprintf("%s/大棚样本数据", folder), pattern="_Non-AG.*\\.kml")
  if (length(ag_files)==0 | length(non_ag_files)==0){
    print(paste(zip_item$PROVINCE, "no data, skip"))
    next()
  }
  f<-ag_files[1]
  ag_sf<-data.frame()
  for (f in ag_files){
    item<-st_read(sprintf("%s/大棚样本数据/%s", folder, f))
    if (nrow(ag_sf)==0){
      ag_sf<-item
    }else{
      ag_sf<-rbind(ag_sf, item)
    }
  }
  ag_sf<-st_union(ag_sf)
  ag_sf_df<-st_sf(geometry=ag_sf, ZIP=zip_item$ZIP, PROVINCE=zip_item$PROVINCE, AG="YES")
  non_ag_sf<-data.frame()
  for (f in non_ag_files){
    item<-st_read(sprintf("%s/大棚样本数据/%s", folder, f))
    if (nrow(non_ag_sf)==0){
      non_ag_sf<-item
    }else{
      non_ag_sf<-rbind(non_ag_sf, item)
    }
  }
  non_ag_sf<-st_union(non_ag_sf)
  non_ag_sf_df<-st_sf(geometry=non_ag_sf, ZIP=zip_item$ZIP, PROVINCE=zip_item$PROVINCE, AG="NO")
  ag_sf_all<-rbind(ag_sf_df, non_ag_sf_df)
  if (nrow(all_sets)==0){
    all_sets<-ag_sf_all
  }else{
    all_sets<-rbind(all_sets, ag_sf_all)
  }
}

all_sets_sub<-all_sets[which(st_geometry_type(all_sets)=="MULTIPOLYGON"),]
st_write(all_sets_sub, "../Shape/AG_Trainingset/AG_Trainingset.shp", append = F, layer_options = "ENCODING=UTF-8")

