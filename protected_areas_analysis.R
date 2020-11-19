library(raster)
library(rgdal)
require(tidyverse)

setwd("D:/Jef_Melastomes")

#get PH shapefile
phl.shp <- getData('GADM', country = 'PHL', level=0)
extent(phl.shp)

#very important! run and DO NOT change anything. this contains the raster specifications: CRS projection, extent, and resolution
x <-raster(xmn=116.9283  ,xmx=126.6053 ,ymn=4.58694 ,ymx=21.07014 ,res=0.04166667,crs="+proj=longlat +datum=WGS84")


#read in dissolved PA boundaries 
wdpa.shp <- readOGR(dsn=getwd(), layer = "PH_WDPA_dissolved")
wdpa.shp@data$Id <- as.factor(1) 

#convert shp to raster
wdpa.ras <- rasterize(wdpa.shp, x, field = "Id", filename = "PH_WDPA_dissolved_2.5min.tif", datatype = "INT2U", format = "GTiff")

#read back in
wdpa.ras <- raster("PH_WDPA_dissolved_2.5min.tif")
plot(wdpa.ras)

#convert to data frame
wdpa.df <- as.data.frame(wdpa.ras, xy=T, na.rm = T)
summary(wdpa.df)
wdpa.xy <- wdpa.df[1:2]

#read in current rasters
asca.cur <- raster("Astrocalyx_calycina_CH25m/Astrocalyx_calycina_CH25m_currentEnv_binary.tif")
ascu.cur <- raster("Astronia_cumingiana_CH25m/Astronia_cumingiana_CH25m_currentEnv_binary.tif")
beic.cur <- raster("Beccarianthus_ickisii_CH25m/Beccarianthus_ickisii_CH25m_currentEnv_binary.tif")
bepu.cur <- raster("Beccarianthus_pulcherrimus_CH25m/Beccarianthus_pulcherrimus_CH25m_currentEnv_binary.tif")

#extract areas inside
asca.cur.pa <- raster::extract(asca.cur, wdpa.xy, df = T, na.rm = T)
summary(asca.cur.pa)
asca.cur.pa <- na.omit(asca.cur.pa)
colnames(asca.cur.pa) <- c("ID", "Suitability")
asca.cur.pa2 <- count(asca.cur.pa, Suitability )

ascu.cur.pa <- raster::extract(ascu.cur, wdpa.xy, df = T, na.rm = T)
summary(ascu.cur.pa)
ascu.cur.pa <- na.omit(ascu.cur.pa)
colnames(ascu.cur.pa) <- c("ID", "Suitability")
ascu.cur.pa2 <- count(ascu.cur.pa, Suitability )

beic.cur.pa <- raster::extract(beic.cur, wdpa.xy, df = T, na.rm = T)
summary(beic.cur.pa)
beic.cur.pa <- na.omit(beic.cur.pa)
colnames(beic.cur.pa) <- c("ID", "Suitability")
beic.cur.pa2 <- count(beic.cur.pa, Suitability )

bepu.cur.pa <- raster::extract(bepu.cur, wdpa.xy, df = T, na.rm = T)
summary(bepu.cur.pa)
bepu.cur.pa <- na.omit(bepu.cur.pa)
colnames(bepu.cur.pa) <- c("ID", "Suitability")
bepu.cur.pa2 <- count(bepu.cur.pa, Suitability )

all.cur.pa2 <- cbind(asca.cur.pa2, ascu.cur.pa2[2], beic.cur.pa2[2], bepu.cur.pa2[2])
colnames(all.cur.pa2)[2:5] <- c("ASCA", "ASCU", "BEIC", "BEPU")
write.csv(all.cur.pa2, file = "Protected_area_analysis_spp.csv", row.names = F)
