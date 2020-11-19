#MAPPING RESULTS
library(raster)
library(tmap)
library(RColorBrewer)

#get PH shapefile
phl.shp <- getData('GADM', country = 'PHL', level=0)

#figure 2: continuous maps ####

#read in the continuous maps (ave) of all species

#name of species to read in 
#asca
species.name <- "Astrocalyx_calycina_CH25m" #Astrocalyx_calycina_CH25m #Astronia_cumingiana #Beccarianthus_ickisii #Beccarianthus_pulcherrimus

#current
asca.cur <- raster(paste0(species.name,"/",species.name,"_currentEnv.tif"))
#rcp26
asca.rcp26a <- raster(paste0(species.name,"/",species.name,"_2050_RCP26_ensemble.tif"))
asca.rcp26b <- raster(paste0(species.name,"/",species.name,"_2070_RCP26_ensemble.tif"))    
#rcp85
asca.rcp85a  <- raster(paste0(species.name,"/",species.name,"_2050_RCP85_ensemble.tif"))  
asca.rcp85b  <- raster(paste0(species.name,"/",species.name,"_2070_RCP85_ensemble.tif")) 

names(asca.cur) <- "Suitability"
names(asca.rcp26a) <- "Suitability"
names(asca.rcp26b) <- "Suitability"
names(asca.rcp85a) <- "Suitability"
names(asca.rcp85b) <- "Suitability"

#ascu
species.name <- "Astronia_cumingiana_CH25m" #Astrocalyx_calycina_CH25m #Astronia_cumingiana #Beccarianthus_ickisii #Beccarianthus_pulcherrimus

#current
ascu.cur <- raster(paste0(species.name,"/",species.name,"_currentEnv.tif"))
#rcp26
ascu.rcp26a <- raster(paste0(species.name,"/",species.name,"_2050_RCP26_ensemble.tif"))
ascu.rcp26b <- raster(paste0(species.name,"/",species.name,"_2070_RCP26_ensemble.tif"))    
#rcp85
ascu.rcp85a  <- raster(paste0(species.name,"/",species.name,"_2050_RCP85_ensemble.tif"))  
ascu.rcp85b  <- raster(paste0(species.name,"/",species.name,"_2070_RCP85_ensemble.tif")) 

names(ascu.cur) <- "Suitability"
names(ascu.rcp26a) <- "Suitability"
names(ascu.rcp26b) <- "Suitability"
names(ascu.rcp85a) <- "Suitability"
names(ascu.rcp85b) <- "Suitability"

#beic
species.name <- "Beccarianthus_ickisii_CH25m" #Astrocalyx_calycina_CH25m #Astronia_cumingiana #Beccarianthus_ickisii #Beccarianthus_pulcherrimus

#current
beic.cur <- raster(paste0(species.name,"/",species.name,"_currentEnv.tif"))
#rcp26
beic.rcp26a <- raster(paste0(species.name,"/",species.name,"_2050_RCP26_ensemble.tif"))
beic.rcp26b <- raster(paste0(species.name,"/",species.name,"_2070_RCP26_ensemble.tif"))    
#rcp85
beic.rcp85a  <- raster(paste0(species.name,"/",species.name,"_2050_RCP85_ensemble.tif"))  
beic.rcp85b  <- raster(paste0(species.name,"/",species.name,"_2070_RCP85_ensemble.tif")) 

names(beic.cur) <- "Suitability"
names(beic.rcp26a) <- "Suitability"
names(beic.rcp26b) <- "Suitability"
names(beic.rcp85a) <- "Suitability"
names(beic.rcp85b) <- "Suitability"

#bepu
species.name <- "Beccarianthus_pulcherrimus_CH25m" #Astrocalyx_calycina_CH25m #Astronia_cumingiana #Beccarianthus_ickisii #Beccarianthus_pulcherrimus

#current
bepu.cur <- raster(paste0(species.name,"/",species.name,"_currentEnv.tif"))
#rcp26
bepu.rcp26a <- raster(paste0(species.name,"/",species.name,"_2050_RCP26_ensemble.tif"))
bepu.rcp26b <- raster(paste0(species.name,"/",species.name,"_2070_RCP26_ensemble.tif"))    
#rcp85
bepu.rcp85a  <- raster(paste0(species.name,"/",species.name,"_2050_RCP85_ensemble.tif"))  
bepu.rcp85b  <- raster(paste0(species.name,"/",species.name,"_2070_RCP85_ensemble.tif")) 

names(bepu.cur) <- "Suitability"
names(bepu.rcp26a) <- "Suitability"
names(bepu.rcp26b) <- "Suitability"
names(bepu.rcp85a) <- "Suitability"
names(bepu.rcp85b) <- "Suitability"

#layout maps
#asca
map.cur.asca <- tm_shape(asca.cur) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("(a)",
            title.size = 1,
            legend.show = F,
            frame = F) 


#RCP 2.6 
map.rcp26a.asca <- tm_shape(asca.rcp26a) + 
  tm_raster(palette = "-YlGnBu") + 
  tm_layout("RCP2.6a",
            title.size = 1,
            legend.show = F,
            frame = F) 

map.rcp26b.asca <- tm_shape(asca.rcp26b) + 
  tm_raster(palette = "-YlGnBu") + 
  tm_layout("(b)",
            title.size = 1,
            legend.show = F,
            frame = F) 

#RCP 8.5
map.rcp85a.asca <- tm_shape(asca.rcp85a) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("RCP 8.5a",
            title.size = 1,
            legend.show = F,
            frame = F)

map.rcp85b.asca <- tm_shape(asca.rcp85b) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("(c)",
            title.size = 1,
            legend.show = F,
            frame = F)

#ascu
map.cur.ascu <- tm_shape(ascu.cur) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("(d)",
            title.size = 1,
            legend.show = F,
            frame = F) 


#RCP 2.6 
map.rcp26a.ascu <- tm_shape(ascu.rcp26a) + 
  tm_raster(palette = "-YlGnBu") + 
  tm_layout("RCP2.6a",
            title.size = 1,
            legend.show = F,
            frame = F) 

map.rcp26b.ascu <- tm_shape(ascu.rcp26b) + 
  tm_raster(palette = "-YlGnBu") + 
  tm_layout("(e)",
            title.size = 1,
            legend.show = F,
            frame = F)  

#RCP 8.5
map.rcp85a.ascu <- tm_shape(ascu.rcp85a) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("RCP 8.5a",
            title.size = 1,
            legend.show = F,
            frame = F) 

map.rcp85b.ascu <- tm_shape(ascu.rcp85b) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("(f)",
            title.size = 1,
            legend.show = F,
            frame = F) 

#beic
#beic
map.cur.beic <- tm_shape(beic.cur) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("(g)",
            title.size = 1,
            legend.show = F,
            frame = F) 


#RCP 2.6 
map.rcp26a.beic <- tm_shape(beic.rcp26a) + 
  tm_raster(palette = "-YlGnBu") + 
  tm_layout("RCP2.6a",
            title.size = 1,
            legend.show = F,
            frame = F)

map.rcp26b.beic <- tm_shape(beic.rcp26b) + 
  tm_raster(palette = "-YlGnBu") + 
  tm_layout("(h)",
            title.size = 1,
            legend.show = F,
            frame = F) 

#RCP 8.5
map.rcp85a.beic <- tm_shape(beic.rcp85a) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("RCP 8.5a",
            title.size = 1,
            legend.show = F,
            frame = F) 

map.rcp85b.beic <- tm_shape(beic.rcp85b) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("(i)",
            title.size = 1,
            legend.show = F,
            frame = F) 

#bepu
map.cur.bepu <- tm_shape(bepu.cur) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("(j)",
            title.size = 1,
            legend.show = F,
            frame = F)


#RCP 2.6 
map.rcp26a.bepu <- tm_shape(bepu.rcp26a) + 
  tm_raster(palette = "-YlGnBu") + 
  tm_layout("RCP2.6a",
            title.size = 1,
            legend.show = F,
            frame = F)  

map.rcp26b.bepu <- tm_shape(bepu.rcp26b) + 
  tm_raster(palette = "-YlGnBu") + 
  tm_layout("(k)",
            title.size = 1,
            legend.show = F,
            frame = F) 

#RCP 8.5
map.rcp85a.bepu <- tm_shape(bepu.rcp85a) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("RCP 8.5a",
            title.size = 1,
            legend.show = F,
            frame = F) 

map.rcp85b.bepu <- tm_shape(bepu.rcp85b) + 
  tm_raster(palette = c("-YlGnBu")) + 
  tm_layout("(l)",
            title.size = 1,
            legend.show = F,
            frame = F) 

### plotting fig 2####
tmap_mode("plot")
windows()
tmap_arrange(map.cur.asca, map.rcp26b.asca, map.rcp85b.asca,
             map.cur.ascu, map.rcp26b.ascu, map.rcp85b.ascu,
             map.cur.beic, map.rcp26b.beic, map.rcp85b.beic,
             map.cur.bepu, map.rcp26b.bepu, map.rcp85b.bepu,
             ncol=3, nrow=4, 
             outer.margins = 0)


#figure 3: binary maps showing gain/loss ####

#ASCA
#read in binary maps of the species
species.name <- "Astrocalyx_calycina_CH25m" #Astrocalyx_calycina_CH25m #Astronia_cumingiana #Beccarianthus_ickisii #Beccarianthus_pulcherrimus

#rcp26a
asca.ch.rcp26a <- raster(paste0(species.name,"/",species.name,"_change_RCP26a.tif"))
#rcp26b
asca.ch.rcp26b <- raster(paste0(species.name,"/",species.name,"_change_RCP26b.tif"))
#rcp85a
asca.ch.rcp85a <- raster(paste0(species.name,"/",species.name,"_change_RCP85a.tif"))
#rcp85b
asca.ch.rcp85b <- raster(paste0(species.name,"/",species.name,"_change_RCP85b.tif"))  

names(asca.ch.rcp26a) <- "Suitability"
names(asca.ch.rcp26b) <- "Suitability"
names(asca.ch.rcp85a) <- "Suitability"
names(asca.ch.rcp85b) <- "Suitability"

asca.ch.rcp26a[asca.ch.rcp26a == 5] <- "Unsuitable"
asca.ch.rcp26a[asca.ch.rcp26a == 6] <- "Gained"
asca.ch.rcp26a[asca.ch.rcp26a == 8] <- "Lost"
asca.ch.rcp26a[asca.ch.rcp26a == 9] <- "Retained"
asca.ch.rcp26a[asca.ch.rcp26a == 7] <- NA
asca.ch.rcp26a[asca.ch.rcp26a < 5] <- NA

asca.ch.rcp26b[asca.ch.rcp26b == 5] <- "Unsuitable"
asca.ch.rcp26b[asca.ch.rcp26b == 6] <- "Gained"
asca.ch.rcp26b[asca.ch.rcp26b == 8] <- "Lost"
asca.ch.rcp26b[asca.ch.rcp26b == 9] <- "Retained"
asca.ch.rcp26b[asca.ch.rcp26b == 7] <- NA
asca.ch.rcp26b[asca.ch.rcp26b < 5] <- NA

asca.ch.rcp85a[asca.ch.rcp85a == 5] <- "Unsuitable"
asca.ch.rcp85a[asca.ch.rcp85a == 6] <- "Gained"
asca.ch.rcp85a[asca.ch.rcp85a == 8] <- "Lost"
asca.ch.rcp85a[asca.ch.rcp85a == 9] <- "Retained"
asca.ch.rcp85a[asca.ch.rcp85a == 7] <- NA
asca.ch.rcp85a[asca.ch.rcp85a < 5] <- NA

asca.ch.rcp85b[asca.ch.rcp85b == 5] <- "Unsuitable"
asca.ch.rcp85b[asca.ch.rcp85b == 6] <- "Gained"
asca.ch.rcp85b[asca.ch.rcp85b == 8] <- "Lost"
asca.ch.rcp85b[asca.ch.rcp85b == 9] <- "Retained"
asca.ch.rcp85b[asca.ch.rcp85b == 7] <- NA
asca.ch.rcp85b[asca.ch.rcp85b < 5] <- NA

#layout maps
#RCP 2.6a
chmap.rcp26a.asca <- tm_shape(asca.ch.rcp26a) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("RCP2.6a",
            title.size = 1,
            legend.title.color="black", 
            legend.position = c("left","bottom"),
            frame = F) 

#RCP 2.6b
chmap.rcp26b.asca <- tm_shape(asca.ch.rcp26b) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("(a)",
            title.size = 1,
            legend.title.color="black", 
            legend.position = c("left","bottom"),
            frame = F) 

#RCP 8.5a
chmap.rcp85a.asca <- tm_shape(asca.ch.rcp85a) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("RCP8.5a",
            title.size = 1,
            legend.show = F,
            frame = F) 

#RCP 8.5b
chmap.rcp85b.asca <- tm_shape(asca.ch.rcp85b) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("(b)",
            title.size = 1,
            legend.show = F,
            frame = F) 

## ASCU ##
#read in binary maps of the species
species.name <- "Astronia_cumingiana_CH25m" #Astrocalyx_calycina_CH25m #Astronia_cumingiana #Beccarianthus_ickisii #Beccarianthus_pulcherrimus

#rcp26a
ascu.ch.rcp26a <- raster(paste0(species.name,"/",species.name,"_change_RCP26a.tif"))
#rcp26b
ascu.ch.rcp26b <- raster(paste0(species.name,"/",species.name,"_change_RCP26b.tif"))
#rcp85a
ascu.ch.rcp85a <- raster(paste0(species.name,"/",species.name,"_change_RCP85a.tif"))
#rcp85b
ascu.ch.rcp85b <- raster(paste0(species.name,"/",species.name,"_change_RCP85b.tif"))  

names(ascu.ch.rcp26a) <- "Suitability"
names(ascu.ch.rcp26b) <- "Suitability"
names(ascu.ch.rcp85a) <- "Suitability"
names(ascu.ch.rcp85b) <- "Suitability"

ascu.ch.rcp26a[ascu.ch.rcp26a == 5] <- "Unsuitable"
ascu.ch.rcp26a[ascu.ch.rcp26a == 6] <- "Gained"
ascu.ch.rcp26a[ascu.ch.rcp26a == 8] <- "Lost"
ascu.ch.rcp26a[ascu.ch.rcp26a == 9] <- "Retained"
ascu.ch.rcp26a[ascu.ch.rcp26a == 7] <- NA
ascu.ch.rcp26a[ascu.ch.rcp26a < 5] <- NA

ascu.ch.rcp26b[ascu.ch.rcp26b == 5] <- "Unsuitable"
ascu.ch.rcp26b[ascu.ch.rcp26b == 6] <- "Gained"
ascu.ch.rcp26b[ascu.ch.rcp26b == 8] <- "Lost"
ascu.ch.rcp26b[ascu.ch.rcp26b == 9] <- "Retained"
ascu.ch.rcp26b[ascu.ch.rcp26b == 7] <- NA
ascu.ch.rcp26b[ascu.ch.rcp26b < 5] <- NA

ascu.ch.rcp85a[ascu.ch.rcp85a == 5] <- "Unsuitable"
ascu.ch.rcp85a[ascu.ch.rcp85a == 6] <- "Gained"
ascu.ch.rcp85a[ascu.ch.rcp85a == 8] <- "Lost"
ascu.ch.rcp85a[ascu.ch.rcp85a == 9] <- "Retained"
ascu.ch.rcp85a[ascu.ch.rcp85a == 7] <- NA
ascu.ch.rcp85a[ascu.ch.rcp85a < 5] <- NA

ascu.ch.rcp85b[ascu.ch.rcp85b == 5] <- "Unsuitable"
ascu.ch.rcp85b[ascu.ch.rcp85b == 6] <- "Gained"
ascu.ch.rcp85b[ascu.ch.rcp85b == 8] <- "Lost"
ascu.ch.rcp85b[ascu.ch.rcp85b == 9] <- "Retained"
ascu.ch.rcp85b[ascu.ch.rcp85b == 7] <- NA
ascu.ch.rcp85b[ascu.ch.rcp85b < 5] <- NA

#layout maps
#RCP 2.6a
chmap.rcp26a.ascu <- tm_shape(ascu.ch.rcp26a) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("RCP2.6a",
            title.size = 1,
            legend.title.color="black", 
            legend.position = c("left","bottom"),
            frame = F)

#RCP 2.6b
chmap.rcp26b.ascu <- tm_shape(ascu.ch.rcp26b) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("(c)",
            title.size = 1,
            legend.show = F,
            frame = F) 

#RCP 8.5a
chmap.rcp85a.ascu <- tm_shape(ascu.ch.rcp85a) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("RCP8.5a",
            title.size = 1,
            legend.show = F,
            frame = F) 

#RCP 8.5b
chmap.rcp85b.ascu <- tm_shape(ascu.ch.rcp85b) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("(d)",
            title.size = 1,
            legend.show = F,
            frame = F) 

## BEIC ## 
#read in binary maps of the species
species.name <- "Beccarianthus_ickisii_CH25m" #Astrocalyx_calycina_CH25m #Astronia_cumingiana #Beccarianthus_ickisii #Beccarianthus_pulcherrimus

#rcp26a
beic.ch.rcp26a <- raster(paste0(species.name,"/",species.name,"_change_RCP26a.tif"))
#rcp26b
beic.ch.rcp26b <- raster(paste0(species.name,"/",species.name,"_change_RCP26b.tif"))
#rcp85a
beic.ch.rcp85a <- raster(paste0(species.name,"/",species.name,"_change_RCP85a.tif"))
#rcp85b
beic.ch.rcp85b <- raster(paste0(species.name,"/",species.name,"_change_RCP85b.tif"))  

names(beic.ch.rcp26a) <- "Suitability"
names(beic.ch.rcp26b) <- "Suitability"
names(beic.ch.rcp85a) <- "Suitability"
names(beic.ch.rcp85b) <- "Suitability"

beic.ch.rcp26a[beic.ch.rcp26a == 5] <- "Unsuitable"
beic.ch.rcp26a[beic.ch.rcp26a == 6] <- "Gained"
beic.ch.rcp26a[beic.ch.rcp26a == 8] <- "Lost"
beic.ch.rcp26a[beic.ch.rcp26a == 9] <- "Retained"
beic.ch.rcp26a[beic.ch.rcp26a == 7] <- NA
beic.ch.rcp26a[beic.ch.rcp26a < 5] <- NA

beic.ch.rcp26b[beic.ch.rcp26b == 5] <- "Unsuitable"
beic.ch.rcp26b[beic.ch.rcp26b == 6] <- "Gained"
beic.ch.rcp26b[beic.ch.rcp26b == 8] <- "Lost"
beic.ch.rcp26b[beic.ch.rcp26b == 9] <- "Retained"
beic.ch.rcp26b[beic.ch.rcp26b == 7] <- NA
beic.ch.rcp26b[beic.ch.rcp26b < 5] <- NA

beic.ch.rcp85a[beic.ch.rcp85a == 5] <- "Unsuitable"
beic.ch.rcp85a[beic.ch.rcp85a == 6] <- "Gained"
beic.ch.rcp85a[beic.ch.rcp85a == 8] <- "Lost"
beic.ch.rcp85a[beic.ch.rcp85a == 9] <- "Retained"
beic.ch.rcp85a[beic.ch.rcp85a == 7] <- NA
beic.ch.rcp85a[beic.ch.rcp85a < 5] <- NA

beic.ch.rcp85b[beic.ch.rcp85b == 5] <- "Unsuitable"
beic.ch.rcp85b[beic.ch.rcp85b == 6] <- "Gained"
beic.ch.rcp85b[beic.ch.rcp85b == 8] <- "Lost"
beic.ch.rcp85b[beic.ch.rcp85b == 9] <- "Retained"
beic.ch.rcp85b[beic.ch.rcp85b == 7] <- NA
beic.ch.rcp85b[beic.ch.rcp85b < 5] <- NA

#layout maps
#RCP 2.6a
chmap.rcp26a.beic <- tm_shape(beic.ch.rcp26a) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("RCP2.6a",
            title.size = 1,
            legend.title.color="black", 
            legend.position = c("left","bottom"),
            frame = F)

#RCP 2.6b
chmap.rcp26b.beic <- tm_shape(beic.ch.rcp26b) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("(e)",
            title.size = 1,
            legend.show = F,
            frame = F)

#RCP 8.5a
chmap.rcp85a.beic <- tm_shape(beic.ch.rcp85a) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("RCP8.5a",
            title.size = 1,
            legend.show = F,
            frame = F)

#RCP 8.5b
chmap.rcp85b.beic <- tm_shape(beic.ch.rcp85b) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("(f)",
            title.size = 1,
            legend.show = F,
            frame = F) 

## BEPU ##

#read in binary maps of the species
species.name <- "Beccarianthus_pulcherrimus_CH25m" #Astrocalyx_calycina_CH25m #Astronia_cumingiana #Beccarianthus_ickisii #Beccarianthus_pulcherrimus

#rcp26a
bepu.ch.rcp26a <- raster(paste0(species.name,"/",species.name,"_change_RCP26a.tif"))
#rcp26b
bepu.ch.rcp26b <- raster(paste0(species.name,"/",species.name,"_change_RCP26b.tif"))
#rcp85a
bepu.ch.rcp85a <- raster(paste0(species.name,"/",species.name,"_change_RCP85a.tif"))
#rcp85b
bepu.ch.rcp85b <- raster(paste0(species.name,"/",species.name,"_change_RCP85b.tif"))  

names(bepu.ch.rcp26a) <- "Suitability"
names(bepu.ch.rcp26b) <- "Suitability"
names(bepu.ch.rcp85a) <- "Suitability"
names(bepu.ch.rcp85b) <- "Suitability"

bepu.ch.rcp26a[bepu.ch.rcp26a == 5] <- "Unsuitable"
bepu.ch.rcp26a[bepu.ch.rcp26a == 6] <- "Gained"
bepu.ch.rcp26a[bepu.ch.rcp26a == 8] <- "Lost"
bepu.ch.rcp26a[bepu.ch.rcp26a == 9] <- "Retained"
bepu.ch.rcp26a[bepu.ch.rcp26a == 7] <- NA
bepu.ch.rcp26a[bepu.ch.rcp26a < 5] <- NA

bepu.ch.rcp26b[bepu.ch.rcp26b == 5] <- "Unsuitable"
bepu.ch.rcp26b[bepu.ch.rcp26b == 6] <- "Gained"
bepu.ch.rcp26b[bepu.ch.rcp26b == 8] <- "Lost"
bepu.ch.rcp26b[bepu.ch.rcp26b == 9] <- "Retained"
bepu.ch.rcp26b[bepu.ch.rcp26b == 7] <- NA
bepu.ch.rcp26b[bepu.ch.rcp26b < 5] <- NA

bepu.ch.rcp85a[bepu.ch.rcp85a == 5] <- "Unsuitable"
bepu.ch.rcp85a[bepu.ch.rcp85a == 6] <- "Gained"
bepu.ch.rcp85a[bepu.ch.rcp85a == 8] <- "Lost"
bepu.ch.rcp85a[bepu.ch.rcp85a == 9] <- "Retained"
bepu.ch.rcp85a[bepu.ch.rcp85a == 7] <- NA
bepu.ch.rcp85a[bepu.ch.rcp85a < 5] <- NA

bepu.ch.rcp85b[bepu.ch.rcp85b == 5] <- "Unsuitable"
bepu.ch.rcp85b[bepu.ch.rcp85b == 6] <- "Gained"
bepu.ch.rcp85b[bepu.ch.rcp85b == 8] <- "Lost"
bepu.ch.rcp85b[bepu.ch.rcp85b == 9] <- "Retained"
bepu.ch.rcp85b[bepu.ch.rcp85b == 7] <- NA
bepu.ch.rcp85b[bepu.ch.rcp85b < 5] <- NA

#layout maps
#RCP 2.6a
chmap.rcp26a.bepu <- tm_shape(bepu.ch.rcp26a) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("RCP2.6a",
            title.size = 1,
            legend.title.color="black", 
            legend.position = c("left","bottom"),
            frame = F) 

#RCP 2.6b
chmap.rcp26b.bepu <- tm_shape(bepu.ch.rcp26b) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("(g)",
            title.size = 1,
            legend.show = F,
            frame = F) 

#RCP 8.5a
chmap.rcp85a.bepu <- tm_shape(bepu.ch.rcp85a) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("RCP8.5a",
            title.size = 1,
            legend.show = F,
            frame = F) 

#RCP 8.5b
chmap.rcp85b.bepu <- tm_shape(bepu.ch.rcp85b) + 
  tm_raster(palette = c("#ffffbf","#d7191c", "#1a9641", "#404040")) + 
  tm_layout("(h)",
            title.size = 1,
            legend.show = F,
            frame = F) 


## plotting fig 3 ###
windows()
tmap_arrange(chmap.rcp26b.asca, chmap.rcp85b.asca, 
             chmap.rcp26b.ascu, chmap.rcp85b.ascu,
             chmap.rcp26b.beic, chmap.rcp85b.beic,
             chmap.rcp26b.bepu, chmap.rcp85b.bepu,
             ncol=2, nrow=4, outer.margins = 0)

#figure 5: zoomed in areas