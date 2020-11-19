library(raster)
library(rgdal)
library(maps)
library(mapdata)
library(dismo)  
library(rJava)  
library(maptools)
library(jsonlite)
library(rworldmap)
library(RColorBrewer)
require(tidyverse)
library(doParallel)
library(readxl)
library(tmap)
library(CoordinateCleaner)
registerDoParallel()
getDoParWorkers() 

setwd("D:/Jef_Melastomes")

### Data access ####
#get PH shapefile
phl.shp <- getData('GADM', country = 'PHL', level=0)

#read in LGM boundaries shapefile
bathymetryPoly <- readOGR(dsn = getwd(), layer = "PHlandmassLGMPolygon30s2")

#read and crop altitude data
altitude <- raster("D:/GIS_Layers/wc2.1_30s_elev/wc2.1_30s_elev.tif")
alt.PH <- crop(altitude, phl.shp)
alt.PH <- mask(alt.PH, phl.shp) 
plot(alt.PH)


#read envi data downloaded
#chelsa current env
setwd("D:/GIS_Layers/CHELSA_BIOCLIM/")
CurENVfiles <- list.files(pattern = ".tif$")
CurENVfiles
currentEnv <- stack(CurENVfiles)

plot(currentEnv$CHELSA_bio10_01)

currentEnvPH <- crop(currentEnv, phl.shp)
currentEnvPH2 <- mask(currentEnvPH, phl.shp) 
plot(currentEnvPH2$CHELSA_bio10_01)

names(currentEnvPH2) <- c("bio1", "bio2", "bio3", "bio4", "bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15","bio16","bio17","bio18","bio19")
plot(currentEnvPH2$bio12)


setwd("D:/Jef_Melastomes")
#write it out
writeRaster(currentEnvPH2, filename = "PH_bioclim_chelsa30s_all_stack.tif", overwrite = T) 

#read back in
currentEnvPH2 <- brick("PH_bioclim_chelsa30s_all_stack.tif")


## Species Points ##
sppts <- read.csv("Mancera_Astronieae_PH_spp_pts2.csv")
summary(sppts)

colnames(sppts) <- c("Species", "Lat", "Lon", "Source", "Collector", "Number", "Year")


### Data Processing ####
### climate ###

#check for multicollinearity, climate

currentEnvPH.df <- as.data.frame(currentEnvPH2, na.rm = T)
climcor <- cor(currentEnvPH.df, method = "spearman")

climcor[upper.tri(climcor)] <- 0
diag(climcor) <- 0

climdata.new <- currentEnvPH.df[,!apply(climcor,2,function(x) any(x > 0.75))] #drop variables with r > 0.75
head(climdata.new)
write.csv(climcor, file = "CurrentEnv_spearmans_0.75_CHELSA30s_alt_PH.csv")


#keep the following parameters: bio1, bio4, bio12, bio15, bio19

currentEnvPH3 <- dropLayer(currentEnvPH2, c("bio2", "bio3","bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio13", "bio14", "bio16", "bio17", "bio18"))

plot(currentEnvPH3) #to check


#resample envi to 2.5 arc mins
#get worldclim 2.5 min for resampling guide and crop-mask it
wc25min <- raster("D:/GIS_Layers/WorldClimdata/2.5-min/wc2.1_2.5m_bio/wc2.1_2.5m_bio_01.tif")
wc25min <- crop(wc25min, phl.shp)
wc25min <- mask(wc25min, phl.shp)

#current
currentEnvPH4 <- resample(currentEnvPH3, wc25min, method = "ngb")
writeRaster(currentEnvPH4, filename = "PH_bioclim_chelsa2.5min_envstack.tif", format = "GTiff" )

currentEnvPH4 <- brick("PH_bioclim_chelsa2.5min_envstack.tif")
names(currentEnvPH4) <- c("bio1", "bio4", "bio12", "bio15", "bio19")

#Read in and prep past and future environments
#LGM
#CNRM-CM5
setwd("D:/GIS_Layers/CHELSA_LGM/CNRM_CM5")
LGMENVfiles1 <- list.files(pattern = ".tif$")
LGMENVfiles1
LGMEnv1 <- stack(LGMENVfiles1)

LGMEnv1 <- crop(LGMEnv1, phl.shp)
LGMEnv1 <- mask(LGMEnv1, bathymetryPoly) 
plot(LGMEnv1$CHELSA_PMIP_CNRM.CM5_BIO_01)

names(LGMEnv1) <- names(currentEnvPH4)

LGMEnv1$bio1 <- ((LGMEnv1$bio1/10) - 273.15) * 10
LGMEnv1$bio12 <- LGMEnv1$bio12 / 10
LGMEnv1$bio19 <- LGMEnv1$bio19 / 10

LGMEnv1 <- resample(LGMEnv1, wc25min, method = "ngb")
plot(LGMEnv1)


#MIROC-ESM
setwd("D:/GIS_Layers/CHELSA_LGM/MIROC-ESM")
LGMENVfiles2 <- list.files(pattern = ".tif$")
LGMENVfiles2
LGMEnv2 <- stack(LGMENVfiles2)

LGMEnv2 <- crop(LGMEnv2, phl.shp)
LGMEnv2 <- mask(LGMEnv2, bathymetryPoly) 

names(LGMEnv2) <- names(currentEnvPH4)

LGMEnv2$bio1 <- ((LGMEnv2$bio1/10) - 273.15) * 10
LGMEnv2$bio12 <- LGMEnv2$bio12 / 10
LGMEnv2$bio19 <- LGMEnv2$bio19 / 10

LGMEnv2 <- resample(LGMEnv2, wc25min, method = "ngb")

plot(LGMEnv2)



#FUTURE
#2041-2060
#CNRM-CM5 
#RCP 2.6
setwd("D:/GIS_Layers/CHELSA_Future/CNRM-CM5/2041-2060/RCP2.6")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvCC.a <- stack(Files1)

futureEnvCC.a <- crop(futureEnvCC.a, phl.shp)
futureEnvCC.a <- mask(futureEnvCC.a, phl.shp) 

names(futureEnvCC.a) <- names(currentEnvPH4)

futureEnvCC.a <- resample(futureEnvCC.a, wc25min, method = "ngb")
plot(futureEnvCC.a)


#RCP 8.5
setwd("D:/GIS_Layers/CHELSA_Future/CNRM-CM5/2041-2060/RCP8.5")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvCC.b <- stack(Files1)

futureEnvCC.b <- crop(futureEnvCC.b, phl.shp)
futureEnvCC.b <- mask(futureEnvCC.b, phl.shp) 

names(futureEnvCC.b) <- names(currentEnvPH4)
futureEnvCC.b <- resample(futureEnvCC.b, wc25min, method = "ngb")
plot(futureEnvCC.b)
futureEnvCC.b

#GFDL-CM3
#RCP 2.6
setwd("D:/GIS_Layers/CHELSA_Future/GFDL-CM3/2041-2060/RCP2.6")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvGF.a <- stack(Files1)

futureEnvGF.a <- crop(futureEnvGF.a, phl.shp)
futureEnvGF.a <- mask(futureEnvGF.a, phl.shp) 

names(futureEnvGF.a) <- names(currentEnvPH4)
futureEnvGF.a <- resample(futureEnvGF.a, wc25min, method = "ngb")
plot(futureEnvGF.a)
futureEnvGF.a

#RCP 8.5
setwd("D:/GIS_Layers/CHELSA_Future/GFDL-CM3/2041-2060/RCP8.5")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvGF.b <- stack(Files1)

futureEnvGF.b <- crop(futureEnvGF.b, phl.shp)
futureEnvGF.b <- mask(futureEnvGF.b, phl.shp) 

names(futureEnvGF.b) <- names(currentEnvPH4)
futureEnvGF.b <- resample(futureEnvGF.b, wc25min, method = "ngb")
plot(futureEnvGF.b)
futureEnvGF.b

#MPI-ESM-LR
#RCP 2.6
setwd("D:/GIS_Layers/CHELSA_Future/MPI-ESM-LR/2041-2060/RCP2.6")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvMP.a <- stack(Files1)

futureEnvMP.a <- crop(futureEnvMP.a, phl.shp)
futureEnvMP.a <- mask(futureEnvMP.a, phl.shp) 

names(futureEnvMP.a) <- names(currentEnvPH4)
futureEnvMP.a <- resample(futureEnvMP.a, wc25min, method = "ngb")
plot(futureEnvMP.a)
futureEnvMP.a

#RCP 8.5
setwd("D:/GIS_Layers/CHELSA_Future/MPI-ESM-LR/2041-2060/RCP8.5")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvMP.b <- stack(Files1)

futureEnvMP.b <- crop(futureEnvMP.b, phl.shp)
futureEnvMP.b <- mask(futureEnvMP.b, phl.shp) 

names(futureEnvMP.b) <- names(currentEnvPH4)
futureEnvMP.b <- resample(futureEnvMP.b, wc25min, method = "ngb")
plot(futureEnvMP.b)
futureEnvMP.b


#2061-2080
#CNRM-CM5 
#RCP 2.6
setwd("D:/GIS_Layers/CHELSA_Future/CNRM-CM5/2061-2080/RCP2.6")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvCC.c <- stack(Files1)

futureEnvCC.c <- crop(futureEnvCC.c, phl.shp)
futureEnvCC.c <- mask(futureEnvCC.c, phl.shp) 

names(futureEnvCC.c) <- names(currentEnvPH4)
futureEnvCC.c <- resample(futureEnvCC.c, wc25min, method = "ngb")
plot(futureEnvCC.c)


#RCP 8.5
setwd("D:/GIS_Layers/CHELSA_Future/CNRM-CM5/2061-2080/RCP8.5")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvCC.d <- stack(Files1)

futureEnvCC.d <- crop(futureEnvCC.d, phl.shp)
futureEnvCC.d <- mask(futureEnvCC.d, phl.shp) 

names(futureEnvCC.d) <- names(currentEnvPH4)
futureEnvCC.d <- resample(futureEnvCC.d, wc25min, method = "ngb")
plot(futureEnvCC.d)
futureEnvCC.d

#GFDL-CM3
#RCP 2.6
setwd("D:/GIS_Layers/CHELSA_Future/GFDL-CM3/2061-2080/RCP2.6")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvGF.c <- stack(Files1)

futureEnvGF.c <- crop(futureEnvGF.c, phl.shp)
futureEnvGF.c <- mask(futureEnvGF.c, phl.shp) 

names(futureEnvGF.c) <- names(currentEnvPH4)
futureEnvGF.c <- resample(futureEnvGF.c, wc25min, method = "ngb")
plot(futureEnvGF.c)
futureEnvGF.c

#RCP 8.5
setwd("D:/GIS_Layers/CHELSA_Future/GFDL-CM3/2061-2080/RCP8.5")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvGF.d <- stack(Files1)

futureEnvGF.d <- crop(futureEnvGF.d, phl.shp)
futureEnvGF.d <- mask(futureEnvGF.d, phl.shp) 

names(futureEnvGF.d) <- names(currentEnvPH4)
futureEnvGF.d <- resample(futureEnvGF.d, wc25min, method = "ngb")
plot(futureEnvGF.d)
futureEnvGF.d

#MPI-ESM-LR
#RCP 2.6
setwd("D:/GIS_Layers/CHELSA_Future/MPI-ESM-LR/2061-2080/RCP2.6")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvMP.c <- stack(Files1)

futureEnvMP.c <- crop(futureEnvMP.c, phl.shp)
futureEnvMP.c <- mask(futureEnvMP.c, phl.shp) 

names(futureEnvMP.c) <- names(currentEnvPH4)
futureEnvMP.c <- resample(futureEnvMP.c, wc25min, method = "ngb")
plot(futureEnvMP.c)
futureEnvMP.c

#RCP 8.5
setwd("D:/GIS_Layers/CHELSA_Future/MPI-ESM-LR/2061-2080/RCP8.5")
Files1 <- list.files(pattern = ".tif$")
Files1
futureEnvMP.d <- stack(Files1)

futureEnvMP.d <- crop(futureEnvMP.d, phl.shp)
futureEnvMP.d <- mask(futureEnvMP.d, phl.shp) 

names(futureEnvMP.d) <- names(currentEnvPH4)
futureEnvMP.d <- resample(futureEnvMP.d, wc25min, method = "ngb")
plot(futureEnvMP.d)
futureEnvMP.d



# Species points cleaning ####
#Coordinates cleaner #
sppts2 <- clean_coordinates(sppts, lon = "Lon", lat = "Lat",
                              species = "Species", 
                              tests = c("capitals","centroids", "equal", "gbif", 
                                        "institutions", "outliers", "seas","zeros"), 
                              capitals_rad = 10000, centroids_rad = 1000, 
                              centroids_detail = "both", inst_rad = 100,
                              outliers_method = "quantile", outliers_mtp = 5, 
                              outliers_size = 7, range_rad = 0, zeros_rad = 0.5,
                              seas_scale = 50, value = "spatialvalid",
                              verbose = TRUE, report = FALSE)

sppts.clean <- filter(sppts2, .summary=="TRUE")
sppts.clean <- dplyr::select(sppts.clean, Species, Lat, Lon, Source, Collector, Number, Year)
summary(sppts.clean)

#rarefy

#new centering code

gridsize <- 0.04166667 # value for half a degree = 0.5; value for 10 minutes: 0.167; 5 mins is 0.083; 30 secs is 0.0083

z <- sppts.clean$Lon
sppts.clean$longmid <- ifelse(z >= 0.0,
                      (z%/%1)+(((z-z%/%1)/gridsize)%/%1)*gridsize,
                      (((z*(-1.0))%/%1)+((((z*(-1.0))-(z*(-1.0))%/%1)/gridsize)%/%1)*gridsize)*(-1.0))


z <- sppts.clean$Lat
sppts.clean$latmid <- ifelse(z >= 0.0, (z%/%1)+(((z-z%/%1)/gridsize)%/%1)*gridsize,
                     (((z*(-1.0))%/%1)+((((z*(-1.0))-(z*(-1.0))%/%1)/gridsize)%/%1)*gridsize)*(-1.0))


#now remove repeats
sppdist <- sppts.clean[c("Species","longmid","latmid", "Lat", "Lon", "Source", "Collector", "Number", "Year")]

sppdist2 <- distinct(sppdist, Species, longmid, latmid, .keep_all = TRUE)
summary(sppdist2)

write.csv(sppdist2, file = "Threatened_Astronieae_clean_2.5min.csv", row.names = F)

#separate species #

asca <- filter(sppdist2, Species == "Astrocalyx calycina")
ascu <- filter(sppdist2, Species == "Astronia cumingiana")
beic <- filter(sppdist2, Species == "Beccarianthus ickisii")
bepu <- filter(sppdist2, Species == "Beccarianthus pulcherrimus")

# find and eliminate duplicate locations
asca.dups <- duplicated(asca[, c("Lon", "Lat")])
asca <- asca[!asca.dups, ]
asca <- select(asca, Lon, Lat) 

ascu.dups <- duplicated(ascu[, c("Lon", "Lat")])
ascu <- asca[!ascu.dups, ]
ascu <- select(ascu, Lon, Lat) 

beic.dups <- duplicated(beic[, c("Lon", "Lat")])
beic <- beic[!beic.dups, ]
beic <- select(beic, Lon, Lat) 

bepu.dups <- duplicated(bepu[, c("Lon", "Lat")])
bepu <- bepu[!bepu.dups, ]
bepu <- select(bepu, Lon, Lat) 

# make initial plot for diagnostic purposes
wrld_simpl <- getMap(resolution = "low")
windows()
plot(wrld_simpl, xlim=c(115,125), ylim=c(5, 20), axes=TRUE, col="light yellow")
points(asca$Lon, asca$Lat, col="orange", pch=20, cex=1)
points(ascu$Lon, ascu$Lat, col="red", pch=20, cex=1)
points(beic$Lon, beic$Lat, col="blue", pch=20, cex=1)
points(bepu$Lon, bepu$Lat, col="darkgreen", pch=20, cex=1)

## extract species climate data per point ##

#get the lon-lat for each point
datagps <- cbind(sppdist2$Lon, sppdist2$Lat)

#download altitude data from CGIAR-CSI
altitude <- getData(name = 'alt', country = 'PHL', mask = TRUE)

#extract the envi data from the envi layers
envidata <- raster::extract(currentEnvPH3, datagps, df = T)
summary(envidata)

altdata <- raster::extract(altitude, datagps, df = T)
summary(altdata)



#combine data columns and rename
colnames(sppdist2)
data <- cbind(sppdist2[1:8], envidata[2:6], altdata[2])
colnames(data) <- c("Species", "longmid", "latmid", "Lat", "Lon", "Source", "Collector", "Number", "MAT","TS", "AP","PS","PCQ", "Alt")
summary(data)

#write and save
write.csv(data, file = "Spp_points_2.5am_with_envdata.csv",row.names=F)



### Species Distribution Model ######################################
set.seed(1789)
setwd("D:/Jef_Melastomes")

#current env
modelclimEnv <- currentEnvPH4
#plot(modelclimEnv)

#set here which species to analyze
sp.occ <- beic
species.name <- "Beccarianthus_ickisii_CH25m2" #Astrocalyx_calycina" #Astronia_cumingiana #Beccarianthus_ickisii", "Beccarianthus_pulcherrimus"
sp.occ <-as.data.frame(sp.occ)

#fit the maxent model 
sp.model <- maxent(
  x=modelclimEnv, 
  p=sp.occ,
  J = TRUE,
  path=paste0(species.name),
  burnin = 5000,
  replicates = 15,
  iteration = 500000,
  doclamp=TRUE,
  args=c(
    'betamultiplier=1',
    'linear=true',
    'quadratic=true',
    'product=false',
    'threshold=false',
    'hinge=true',
    'writebackgroundpredictions=true',
    'doclamp=true',
    'threads=3',
    'responsecurves=true',
    'jackknife=true',
    'askoverwrite=false',
    'removeduplicates',
    'replicatetype=subsample'
  )
)

#Model visualization and evaluation ####

#plot showing importance of each variable
par(mfrow=c(1,1))
plot(sp.model, main = paste0(species.name," variable contribution"))

# response curves
#windows()
#response(sp.model)

# predict to entire dataset
sp.pred.map <- predict(object=sp.model,
                       x=modelclimEnv,
                       filename=paste0(species.name,"/",species.name,"_currentEnv"),
                       na.rm=TRUE,
                       format='GTiff',
                       overwrite=TRUE,
                       doclamp= TRUE
)

#plot predictions
windows()
plot(sp.pred.map, main=paste0(species.name, " Predicted Suitability"))
maps::map('worldHires', fill=FALSE, add=TRUE)
points(sp.occ$Lon, sp.occ$Lat, pch="+", cex=1)



#create binary maps
#extract the MSS training threshold
d <- as.data.frame(sp.model@results)
a <- d[54,1] #for climate only


#create matrix for reclassification
n <- c(0,a,0,a,1,1)
binmat <- matrix(n, ncol=3, byrow=T)
binmat

#reclassify current map
sp.pred.map.bin <- reclassify(x=sp.pred.map, 
                              rcl=binmat, 
                              filename=paste0(species.name,"/",species.name,"_currentEnv_binary"), 
                              format="GTiff",
                              overwrite=T)

windows()
par(mfrow=c(1,2))
plot(sp.pred.map, main="Predicted Suitability")
points(sp.occ$lon, sp.occ$lat, pch="+", cex=0.5)
plot(sp.pred.map.bin, main="Binary", legend=F)
#points(sp.occ$lon, sp.occ$lat, pch="+", cex=0.5)

#predict into LGM climate
#past
past.mod1 <- predict(object=sp.model, 
                     x=LGMEnv1,
                     filename=paste0(species.name,"/",species.name,"_MaxEnt_LGM_CN"),
                     na.rm=TRUE,
                     format='GTiff',
                     overwrite=TRUE,
                     doclamp= TRUE)
past.mod2 <- predict(object=sp.model, 
                     x=LGMEnv2,
                     filename=paste0(species.name,"/",species.name,"_MaxEnt_LGM_ME"),
                     na.rm=TRUE,
                     format='GTiff',
                     overwrite=TRUE,
                     doclamp= TRUE)

#predict into future climate
#2041-2060
#rcp26
future.mod1a <- predict(object=sp.model, 
                        x=futureEnvCC.a, 
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2050_CC_2.6"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)
future.mod2a <- predict(object=sp.model, 
                        x=futureEnvGF.a,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2050_GF_2.6"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)
future.mod3a <- predict(object=sp.model, 
                        x=futureEnvMP.a,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2050_MP_2.6"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)

#rcp85
future.mod1b <- predict(object=sp.model, 
                        x=futureEnvCC.b,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2050_CC_8.5"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)
future.mod2b <- predict(object=sp.model, 
                        x=futureEnvGF.b,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2050_GF_8.5"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)
future.mod3b <- predict(object=sp.model, 
                        x=futureEnvMP.b,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2050_MP_8.5"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)

#2061-2080
#rcp26
future.mod4a <- predict(object=sp.model, 
                        x=futureEnvCC.c, 
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2070_CC_2.6"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)
future.mod5a <- predict(object=sp.model, 
                        x=futureEnvGF.c,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2070_GF_2.6"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)
future.mod6a <- predict(object=sp.model, 
                        x=futureEnvMP.c,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2070_MP_2.6"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)

#rcp85
future.mod4b <- predict(object=sp.model, 
                        x=futureEnvCC.d,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2070_CC_8.5"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)
future.mod5b <- predict(object=sp.model, 
                        x=futureEnvGF.d,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2070_GF_8.5"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)
future.mod6b <- predict(object=sp.model, 
                        x=futureEnvMP.d,
                        filename=paste0(species.name,"/",species.name,"_MaxEnt_2070_MP_8.5"),
                        na.rm=TRUE,
                        format='GTiff',
                        overwrite=TRUE,
                        doclamp= TRUE)


#average model predictions
#2050
fut.2050.26.ave <- (future.mod1a + future.mod2a + future.mod3a) / 3
fut.2050.85.ave <- (future.mod1b + future.mod2b + future.mod3b) / 3

#2070
fut.2070.26.ave <- (future.mod4a + future.mod5a + future.mod6a) / 3
fut.2070.85.ave <- (future.mod4b + future.mod5b + future.mod6b) / 3

#LGM
lgm.mod.ave <- (past.mod1 + past.mod2) / 2

#write it out
#future
writeRaster(fut.2050.26.ave, filename=paste0(species.name,"/",species.name,"_2050_rcp26_ensemble"), format='GTiff', overwrite=TRUE)
writeRaster(fut.2050.85.ave, filename=paste0(species.name,"/",species.name,"_2050_rcp85_ensemble"), format='GTiff', overwrite=TRUE)

writeRaster(fut.2070.26.ave, filename=paste0(species.name,"/",species.name,"_2070_rcp26_ensemble"), format='GTiff', overwrite=TRUE)
writeRaster(fut.2070.85.ave, filename=paste0(species.name,"/",species.name,"_2070_rcp85_ensemble"), format='GTiff', overwrite=TRUE)

#past
writeRaster(lgm.mod.ave, filename=paste0(species.name,"/",species.name,"_LGM_ensemble"), format='GTiff', overwrite=TRUE)

#convert to binary
#averages
#2050
rcp26.50.ave.bin <- reclassify(x=fut.2050.26.ave, 
                            rcl=binmat, 
                            filename=paste0(species.name,"/",species.name,"_2050_RCP26_ensem_bin"),                               format="GTiff",
                            overwrite=T)
rcp85.50.ave.bin <- reclassify(x=fut.2050.85.ave, 
                            rcl=binmat, 
                            filename=paste0(species.name,"/",species.name,"_2050_RCP85_ensem_bin"),                                 format="GTiff",
                            overwrite=T)

#2070
rcp26.70.ave.bin <- reclassify(x=fut.2070.26.ave, 
                            rcl=binmat, 
                            filename=paste0(species.name,"/",species.name,"_2070_RCP26_ensem_bin"),                               format="GTiff",
                            overwrite=T)
rcp85.70.ave.bin <- reclassify(x=fut.2070.85.ave, 
                            rcl=binmat, 
                            filename=paste0(species.name,"/",species.name,"_2070_RCP85_ensem_bin"),                                 format="GTiff",
                            overwrite=T)

#past
lgm.ave.bin <- reclassify(x=lgm.mod.ave, 
                          rcl=binmat, 
                          filename=paste0(species.name,"/",species.name,"_LGM_bin"),                                   format="GTiff",
                          overwrite=T)

#single models

#2050
#RCP2.6
future.mod1a.bin <- reclassify(x=future.mod1a, 
                            rcl=binmat, 
                            filename=paste0(species.name,"/",species.name,"_2050_CC_RCP26_bin"),                                 format="GTiff",
                            overwrite=T)

future.mod2a.bin <- reclassify(x=future.mod2a, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2050_GF_RCP26_bin"),                                 format="GTiff",
                               overwrite=T)

future.mod3a.bin <- reclassify(x=future.mod3a, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2050_MP_RCP85_bin"),                                 format="GTiff",
                               overwrite=T)

#RCP 8.5
future.mod1b.bin <- reclassify(x=future.mod1b, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2050_CC_RCP85_bin"),                                 format="GTiff",
                               overwrite=T)

future.mod2b.bin <- reclassify(x=future.mod2b, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2050_GF_RCP85_bin"),                                 format="GTiff",
                               overwrite=T)

future.mod3b.bin <- reclassify(x=future.mod3b, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2050_MP_RCP85_bin"),                                 format="GTiff",
                               overwrite=T)

#2070
#RCP2.6
future.mod4a.bin <- reclassify(x=future.mod4a, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2070_CC_RCP26_bin"),                                 format="GTiff",
                               overwrite=T)

future.mod5a.bin <- reclassify(x=future.mod5a, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2070_GF_RCP26_bin"),                                 format="GTiff",
                               overwrite=T)

future.mod6a.bin <- reclassify(x=future.mod6a, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2070_MP_RCP85_bin"),                                 format="GTiff",
                               overwrite=T)

#RCP 8.5
future.mod4b.bin <- reclassify(x=future.mod4b, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2070_CC_RCP85_bin"),                                 format="GTiff",
                               overwrite=T)

future.mod5b.bin <- reclassify(x=future.mod5b, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2070_GF_RCP85_bin"),                                 format="GTiff",
                               overwrite=T)

future.mod6b.bin <- reclassify(x=future.mod6b, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_2070_MP_RCP85_bin"),                                 format="GTiff",
                               overwrite=T)

#LGM
past.mod1.bin <- reclassify(x=past.mod1, 
                               rcl=binmat, 
                               filename=paste0(species.name,"/",species.name,"_LGM_CC_bin"),                                 format="GTiff",
                               overwrite=T)

past.mod2.bin <- reclassify(x=past.mod2, 
                            rcl=binmat, 
                            filename=paste0(species.name,"/",species.name,"_LGM_ME_bin"),                                 format="GTiff",
                            overwrite=T)

#PLOTS ####
species.name <- "Beccarianthus ickisii" #Beccarianthus ickisii" #Astrocalyx calycina #Beccarianthus pulcherrimus Astronia cumingiana

#FUTURE suitabilities
#CONTINUOUS
#individual GCMs
#RCP 2.6
par(mfrow=c(2,4))
plot(future.mod1a, main = paste0(species.name, " CNRM-CM5 2050 RCP 2.6"))
plot(future.mod2a, main = paste0(species.name, " GFDL-CM3 2050 RCP 2.6"))
plot(future.mod3a, main = paste0(species.name, " MPI-ESM-LR 2050 RCP 2.6"))
plot(fut.2050.26.ave, main = paste0(species.name, " ensemble 2050 RCP 2.6"))

plot(future.mod4a, main = paste0(species.name, " CNRM-CM5 2070 RCP 2.6"))
plot(future.mod5a, main = paste0(species.name, " GFDL-CM3 2070 RCP 2.6"))
plot(future.mod6a, main = paste0(species.name, " MPI-ESM-LR 2070 RCP 2.6"))
plot(fut.2070.26.ave, main = paste0(species.name, " ensemble 2070 RCP 2.6"))

#RCP 8.5
par(mfrow=c(2,4))
plot(future.mod1b, main = paste0(species.name, " CNRM-CM5 2050 RCP 8.5"))
plot(future.mod2b, main = paste0(species.name, " GFDL-CM3 2050 RCP 8.5"))
plot(future.mod3b, main = paste0(species.name, " MPI-ESM-LR 2050 RCP 8.5"))
plot(fut.2050.85.ave, main = paste0(species.name, " ensemble 2050 RCP 8.5"))

plot(future.mod4b, main = paste0(species.name, " CNRM-CM5 2070 RCP 8.5"))
plot(future.mod5b, main = paste0(species.name, " GFDL-CM3 2070 RCP 8.5"))
plot(future.mod6b, main = paste0(species.name, " MPI-ESM-LR 2070 RCP 8.5"))
plot(fut.2070.85.ave, main = paste0(species.name, " ensemble 2070 RCP 8.5"))

#BINARY MAPS
windows()
par(mfrow=c(1,3))
plot(sp.pred.map.bin, main = paste0(species.name, " current"))
plot(rcp26.50.ave.bin, main = paste0(species.name, " 2050 RCP 2.6"))
plot(rcp26.70.ave.bin, main = paste0(species.name, " 2070 RCP 2.6"))

par(mfrow=c(1,3))
plot(sp.pred.map.bin, main = paste0(species.name, " current"))
plot(rcp85.50.ave.bin, main = paste0(species.name, " 2050 RCP 8.5"))
plot(rcp85.70.ave.bin, main = paste0(species.name, " 2070 RCP 8.5"))

#individual GCMs

#rcp2.6
#2050
par(mfrow=c(2,3))
plot(future.mod1a.bin, main = paste0(species.name, " CNRM-CM5 RCP2.6 2050"))
plot(future.mod2a.bin, main = paste0(species.name, " GFDL-CM3 RCP2.6 2050"))
plot(future.mod3a.bin, main = paste0(species.name, " MPI-ESM-LR RCP2.6 2050"))
#2070
plot(future.mod4a.bin, main = paste0(species.name, " CNRM-CM5 RCP 2.6 2070"))
plot(future.mod5a.bin, main = paste0(species.name, " GFDL-CM3 RCP 2.6 2070"))
plot(future.mod6a.bin, main = paste0(species.name, " MPI-ESM-LR RCP 2.6 2070"))


#rcp8.5
#2050
par(mfrow=c(2,3))
plot(future.mod1b.bin, main = paste0(species.name, " CNRM-CM5 RCP 8.5 2050"))
plot(future.mod2b.bin, main = paste0(species.name, " GFDL-CM3 RCP 8.5 2050"))
plot(future.mod3b.bin, main = paste0(species.name, " MPI-ESM-LR RCP 8.5 2050"))
#2070
plot(future.mod4b.bin, main = paste0(species.name, " CNRM-CM5 RCP 8.5 2070"))
plot(future.mod5b.bin, main = paste0(species.name, " GFDL-CM3 RCP 8.5 2070"))
plot(future.mod6b.bin, main = paste0(species.name, " MPI-ESM-LR RCP 8.5 2070"))


#Plot PAST suitabilities
#Continuous
par(mfrow=c(1,3))
plot(past.mod1, main = paste0(species.name, " CNRM-CM5"))
plot(past.mod2, main = paste0(species.name, " MIROC-ESM"))
plot(lgm.mod.ave, main = paste0(species.name, " LGM Ensemble"))

#Binary
par(mfrow=c(1,3))
plot(past.mod1.bin, main = paste0(species.name, " CNRM-CM5"))
plot(past.mod2.bin, main = paste0(species.name, " MIROC-ESM"))
plot(lgm.ave.bin, main = paste0(species.name, " LGM Ensemble"))


#SUMMARY MAPS ####
windows()
par(mfrow=c(2,3))
plot(sp.pred.map, main = paste0(species.name, " current"))
plot(fut.2050.26.ave, main = paste0(species.name, " 2050 RCP 2.6"))
plot(fut.2070.26.ave, main = paste0(species.name, " 2070 RCP 2.6"))
plot(sp.pred.map, main = paste0(species.name, " current"))
plot(fut.2050.85.ave, main = paste0(species.name, " 2050 RCP 8.5"))
plot(fut.2070.85.ave, main = paste0(species.name, " 2070 RCP 8.5"))


#COUNT NO OF PIXELS CHANGE ####

current.df <- as.data.frame(sp.pred.map.bin, na.rm = T)
colnames(current.df) <- "x"
current.sum <- count(current.df, x)

rcp26.50.df <- as.data.frame(rcp26.50.ave.bin, na.rm = T)
colnames(rcp26.50.df) <- "x"
rcp26.50.sum <- count(rcp26.50.df, x)

rcp26.70.df <- as.data.frame(rcp26.70.ave.bin, na.rm = T)
colnames(rcp26.70.df) <- "x"
rcp26.70.sum <- count(rcp26.70.df, x)

rcp85.50.df <- as.data.frame(rcp85.50.ave.bin, na.rm = T)
colnames(rcp85.50.df) <- "x"
rcp85.50.sum <- count(rcp85.50.df, x)

rcp85.70.df <- as.data.frame(rcp85.70.ave.bin, na.rm = T)
colnames(rcp85.70.df) <- "x"
rcp85.70.sum <- count(rcp85.70.df, x)

change.summ <- as.data.frame(cbind(current.sum$x, current.sum$n, rcp26.50.sum$n, rcp26.70.sum$n, rcp85.50.sum$n, rcp85.70.sum$n))
colnames(change.summ) <- c("State", "Current", "RCP2.6-2050", "RCP2.6-2070","RCP8.5-2050", "RCP8.5-2070")

species.name <- "Beccarianthus_ickisii_CH25m2" #Astrocalyx_calycina" #Astronia_cumingiana #Beccarianthus_ickisii", "Beccarianthus_pulcherrimus"
write.csv(change.summ, file= paste0(species.name,"/",species.name,"_pixel_changes.csv"), row.names = F)


### TRUE SKILL STATISTIC TEST #####
#to be able to run this script you need to have told the Maxent model to produce background predictions. If you are running MaxEnt in R this means putting the argument (after "args") "writebackgroundpredictions=true" as true not false. 

#FUNCTION CODE
TSS_calculations <- function (sample_clog, prediction_clog, n, th) {
  
  xx <- sum(sample_clog > th)
  yy <- sum(prediction_clog > th)
  xxx <- sum(sample_clog < th)
  yyy <- sum(prediction_clog < th)
  
  ncount <- sum(xx,yy,xxx,yyy)
  
  overallaccuracy <- (xx + yyy)/ncount 
  sensitivity <- xx / (xx + xxx)
  specificity <- yyy / (yy + yyy)
  tss <- sensitivity + specificity - 1
  
  #kappa calculations
  a <- xx + xxx
  b <- xx + yy
  c <- yy + yyy
  d <- xxx + yyy
  e <- a * b
  f <- c * d
  g <- e + f
  h <- g / (ncount * ncount)
  hup <- overallaccuracy - h
  hdown <- 1 - h
  
  kappa <- hup/hdown
  Po <- (xx + yyy) / ncount
  Pe <- ((b/ncount) * (a/ncount)) + ((d/ncount) * (c/ncount))
  Px1 <- Po - Pe
  Px2 <- 1 - Pe
  Px3 <- Px1/Px2
  
  tx1 <- xx + yyy
  tx2 <- 2 * a * c
  tx3 <- a - c
  tx4 <- xx - yyy
  tx5 <- ncount * (( 2 * a ) - tx4)
  tx6 <- ncount * tx1
  
  kappamax <- (tx6 - tx2 - (tx3 * tx4)) / ((tx5 - tx3) - (tx3 * tx4))
  
  cat(" Maxent results for model with\n",a,"training sample predictions\n",c ,"background predictions\n\n TSS value:        ", tss,"\n Overall accuracy: ",overallaccuracy,"\n Sensitivity:      ",sensitivity,"\n Specificity:      ",specificity,"\n Kappa:            ",kappa,"\n Kappa max:        ",kappamax)
  
  
}


#put the working directory as the folder where the maxent results are 
setwd(species.name)

#read in the background predictions
backgroundpredictions <- read.csv("species_backgroundPredictions.csv")

#we need the last column so will set the number as x
x <- length(backgroundpredictions)

#extract the cloglog/logistic results
backgroundclog <- backgroundpredictions[,x]

#now read in the sample predictions for testing
samplepredictions <- read.csv("species_samplePredictions.csv")

#we need the last column again of logistic or cloglog predictions so set a second x
x2 <- length(samplepredictions)

#extract the cloglog/logistic results for sample
sampleclog <- samplepredictions[,x2]

#set n the number of pseudoabsences used for background predictions by MaxEnt
n <- 10000

#read in maxent results
maxres <- read.csv("maxentResults.csv")


### Set the threshold rule here
th <- maxres[maxres[,1]=="species","Maximum.training.sensitivity.plus.specificity.Cloglog.threshold"]

#Start a results text file
sink(file = "Maxent_TSS_results.txt", split = T, append = F )

#run the function, the input values are the sampleclog values, then the background clog values, the sample number for the pseudo absences and then threshold value
TSS_calculations(sampleclog,backgroundclog,n,th)


#close results textfile
sink(file = NULL)



#check suitable habitat loss for each time step

#rcp2.6
rcp26.c50 <- rcp26.50.ave.bin + sp.pred.map.bin
rcp26.c70 <- rcp26.70.ave.bin + sp.pred.map.bin

#rcp8.5
rcp85.c50 <- rcp85.50.ave.bin + sp.pred.map.bin
rcp85.c70 <- rcp85.70.ave.bin + sp.pred.map.bin


#### MAPPING ####

#renaming raster contents
#rcp2.6
rcp26.c50[rcp26.c50==0] <- "Unsuitable"
rcp26.c50[rcp26.c50==1] <- "Lost"
rcp26.c50[rcp26.c50==2] <- "Suitable"

rcp26.c70[rcp26.c70==0] <- "Unsuitable"
rcp26.c70[rcp26.c70==1] <- "Lost"
rcp26.c70[rcp26.c70==2] <- "Suitable"

names(rcp26.c50) <- "Suitability"
names(rcp26.c70) <- "Suitability"

#rcp8.5
rcp85.c50[rcp85.c50==0] <- "Unsuitable"
rcp85.c50[rcp85.c50==1] <- "Lost"
rcp85.c50[rcp85.c50==2] <- "Suitable"

rcp85.c70[rcp85.c70==0] <- "Unsuitable"
rcp85.c70[rcp85.c70==1] <- "Lost"
rcp85.c70[rcp85.c70==2] <- "Suitable"

names(rcp85.c50) <- "Suitability"
names(rcp85.c70) <- "Suitability"

#layout maps
#RCP 2.6 
map.rcp26.c50 <- tm_shape(rcp26.c50) + 
  tm_raster(palette = c("#d7191c","#1a9641","#ffffbf")) + 
  tm_layout("RCP 2.6 2050 change",
            title.size = 1,
            legend.title.color="black", 
            legend.position = c("left","bottom")) +
  tm_shape(phl.shp) +
  tm_borders(col="black") + 
  tm_fill(col = "#ffffbf", alpha = 0.1) 

map.rcp26.c70 <- tm_shape(rcp26.c70) + 
  tm_raster(palette = c("#d7191c","#1a9641","#ffffbf")) + 
  tm_layout("RCP 2.6 2070 change",
            title.size = 1,
            legend.title.color="black", 
            legend.position = c("left","bottom")) +
  tm_shape(phl.shp) +
  tm_borders(col="black") + 
  tm_fill(col = "#ffffbf", alpha = 0.1) 

#RCP 8.5
map.rcp85.c50 <- tm_shape(rcp85.c50) + 
  tm_raster(palette = c("#d7191c","#1a9641","#ffffbf")) + 
  tm_layout("RCP 8.5 2050 change",
            title.size = 1,
            legend.title.color="black", 
            legend.position = c("left","bottom")) +
  tm_shape(phl.shp) +
  tm_borders(col="black") + 
  tm_fill(col = "#ffffbf", alpha = 0.1) 

map.rcp85.c70 <- tm_shape(rcp85.c70) + 
  tm_raster(palette = c("#d7191c","#1a9641","#ffffbf")) + 
  tm_layout("RCP 8.5 2070 change",
            title.size = 1,
            legend.title.color="black", 
            legend.position = c("left","bottom")) +
  tm_shape(phl.shp) +
  tm_borders(col="black") + 
  tm_fill(col = "#ffffbf", alpha = 0.1) 


windows()
tmap_arrange(map.rcp26.c50, map.rcp26.c70 ,ncol=2, nrow=1, outer.margins = 0)
windows()
tmap_arrange(map.rcp85.c50, map.rcp85.c70 ,ncol=2, nrow=1, outer.margins = 0)


