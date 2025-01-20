library(data.table)
library(ggplot2)
library(sf)
library(terra)
sf_use_s2(FALSE)
setwd("/media/huijieqiao/Butterfly/rgee_greenhouse/rgee_greenhouse")

env<-rast("../Data/pca_full/pc.1.tif")
df1<-readRDS("../Data/greenhouse.env/work1_1.rda")
df2<-readRDS("../Data/greenhouse.env/work1_2.rda")


range(df2$roundtime)

lonlat<-unique(df2[, c("lon", "lat")])
lonlat$code<--1
lonlat$quhua<-""
ggplot(lonlat)+geom_point(aes(x=lon, y=lat))


quhua<-read_sf("../Shape/Climate_quhua/Climate_quhua.shp")
plot(quhua$geometry)
quhua<-st_transform(quhua, crs=st_crs(env))

lonlat_p<-st_as_sf(x = lonlat,                         
                   coords = c("lon", "lat"),
                   crs = st_crs(env))
plot(lonlat_p, add=T, col="red")

contain<-st_contains(quhua, lonlat_p)

for (i in c(1:nrow(quhua))){
  item<-quhua[i,]
  p.index<-contain[[i]]
  if (length(p.index)>0){
    lonlat[p.index, "code"]<-item$code1
    lonlat[p.index, "quhua"]<-item$Qu1
  }
}

table(lonlat$quhua)

df2$date<-as.Date(df2$roundtime)
df2$year<-year(df2$date)



df2_with_quhua<-merge(df2, lonlat, by=c("lon", "lat"))
daily_minmax<-df2_with_quhua[, .(max_in=max(sensor_value_in),
                                 min_in=min(sensor_value_in),
                                 max_out=max(sensor_value_out),
                                 min_out=min(sensor_value_out),
                                 mean_in=mean(sensor_value_in),
                                 mean_out=mean(sensor_value_out)),
                             by=list(lon, lat, year, month, date, 
                                     code, quhua,sensor_type)]
daily_minmax$max_differ<-daily_minmax$max_in-daily_minmax$max_out
daily_minmax$min_differ<-daily_minmax$min_in-daily_minmax$min_out
daily_minmax$mean_differ<-daily_minmax$mean_in-daily_minmax$mean_out

df2_monthly<-daily_minmax[,.(mean_differ=mean(mean_differ), 
                             mean_differ_sd=sd(mean_differ),
                             max_differ=mean(max_differ), 
                             max_differ_sd=sd(max_differ),
                             min_differ=mean(min_differ), 
                             min_differ_sd=sd(min_differ)),
                          by=list(sensor_type, quhua, month, code)]

quhua_data<-df2_monthly[, .(N=.N), by=list(quhua, code, sensor_type)]

target_quhua<-saveRDS(unique(quhua_data[N==12]$code), "../Data/target_quhua.rda")
saveRDS(df2_monthly, "../Data/quhua.greenhouse.differ.rda")
ggplot(df2_monthly[quhua %in% c(quhua_data[N==12]$quhua)])+
  #geom_ribbon(aes(ymin = diff_mean - diff_sd, ymax = diff_mean + diff_sd,
  #                x=month, group=factor(quhua)), 
  #            fill = "lightgrey", alpha = 0.2) +
  geom_line(aes(x=month, y=max_differ, color=factor(quhua)))+
  geom_line(aes(x=month, y=min_differ, color=factor(quhua)), linetype=2)+
  geom_line(aes(x=month, y=mean_differ, color=factor(quhua)), linetype=3)+
  scale_x_continuous(breaks=c(1:12), labels=c(1:12))+
  facet_wrap(~sensor_type, nrow=2, scale="free")+
  theme_bw()


