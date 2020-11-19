library(raster)
library(rgdal)
library(plyr)
require(tidyverse)
library(forcats)
library(hrbrthemes)
library(viridis)
library(multcompView)
library(visreg)
library(jtools)

#download altitude data from CGIAR-CSI
altitude <- getData(name = 'alt', country = 'PHL', mask = TRUE)

#get PH shapefile
phl.shp <- getData('GADM', country = 'PHL', level=0)

#get worldclim 2.5 min for resampling guide and crop-mask it
wc25min <- raster("D:/GIS_Layers/WorldClimdata/2.5-min/wc2.1_2.5m_bio/wc2.1_2.5m_bio_01.tif")
wc25min <- crop(wc25min, phl.shp)
wc25min <- mask(wc25min, phl.shp)
plot(wc25min) #to check

#resample to 2.5 arc mins
alt25min <- resample(altitude, wc25min, method = "ngb")
alt25min
plot(alt25min)
writeRaster(alt25min, filename = "PH_alt_2.5min.tif", format = "GTiff" )

#name of species to read in  
species.name <- "Astrocalyx_calycina_CH25m" #Astrocalyx_calycina_CH25m #Astronia_cumingiana #Beccarianthus_ickisii #Beccarianthus_pulcherrimus

#read in binary maps of the species
#current bin map
sp.pred.map.bin <- raster(paste0(species.name,"/",species.name,"_currentEnv_binary.tif"))
#future average bin maps
#rcp26
rcp26.50.ave.bin <- raster(paste0(species.name,"/",species.name,"_2050_RCP26_ensem_bin.tif"))
rcp26.70.ave.bin <- raster(paste0(species.name,"/",species.name,"_2070_RCP26_ensem_bin.tif"))    
#rcp85
rcp85.50.ave.bin <- raster(paste0(species.name,"/",species.name,"_2050_RCP85_ensem_bin.tif"))  
rcp85.70.ave.bin <- raster(paste0(species.name,"/",species.name,"_2070_RCP85_ensem_bin.tif")) 

#determine altitude in the suitable areas
#current
r1 <- as.data.frame(sp.pred.map.bin, na.rm = T, xy=T)
colnames(r1) <- c("lon", "lat", "bin")
r1 <- filter(r1, bin==1)
r1 <- cbind(r1$lon, r1$lat)

r1.df <- raster::extract(alt25min, r1, method = "simple", na.rm = T)
r1.df <- as.data.frame(r1.df)
colnames(r1.df) <- "Altitude" 
r1.df$Scenario <- rep("Current", dim(r1.df)[1])

#rcp26
#2050
r26a <- as.data.frame(rcp26.50.ave.bin, na.rm = T, xy=T)
colnames(r26a) <- c("lon", "lat", "bin")
r26a <- filter(r26a, bin==1)
r26a <- cbind(r26a$lon, r26a$lat)

r26a.df <- raster::extract(alt25min, r26a, method = "simple", na.rm = T)
r26a.df <- as.data.frame(r26a.df)
colnames(r26a.df) <- "Altitude" 
r26a.df$Scenario <- rep("RCP2.6a", dim(r26a.df)[1]) 

#2070
r26b <- as.data.frame(rcp26.70.ave.bin, na.rm = T, xy=T)
colnames(r26b) <- c("lon", "lat", "bin")
r26b <- filter(r26b, bin==1)
r26b <- cbind(r26b$lon, r26b$lat)

r26b.df <- raster::extract(alt25min, r26b, method = "simple", na.rm = T)
r26b.df <- as.data.frame(r26b.df)
colnames(r26b.df) <- "Altitude" 
r26b.df$Scenario <- rep("RCP2.6b", dim(r26b.df)[1]) 

#rcp85
#2050
r85a <- as.data.frame(rcp85.50.ave.bin, na.rm = T, xy=T)
colnames(r85a) <- c("lon", "lat", "bin")
r85a <- filter(r85a, bin==1)
r85a <- cbind(r85a$lon, r85a$lat)

r85a.df <- raster::extract(alt25min, r85a, method = "simple", na.rm = T)
r85a.df <- as.data.frame(r85a.df)
colnames(r85a.df) <- "Altitude" 
r85a.df$Scenario <- rep("RCP8.5a", dim(r85a.df)[1]) 

#2070
r85b <- as.data.frame(rcp85.70.ave.bin, na.rm = T, xy=T)
colnames(r85b) <- c("lon", "lat", "bin")
r85b <- filter(r85b, bin==1)
r85b <- cbind(r85b$lon, r85b$lat)

r85b.df <- raster::extract(alt25min, r85b, method = "simple", na.rm = T)
r85b.df <- as.data.frame(r85b.df)
colnames(r85b.df) <- "Altitude" 
r85b.df$Scenario <- rep("RCP8.5b", dim(r85b.df)[1]) 

#combine all data from one species

alldf <- rbind(r1.df, r26a.df, r26b.df, r85a.df, r85b.df)
alldf$Species <- rep("ASCA", dim(alldf)[1]) #change for each species

summary(alldf)

#move to respective species df
asca.alt <- alldf 
ascu.alt <- alldf
beic.alt <- alldf
bepu.alt <- alldf

asca.alt <- na.omit(asca.alt)
ascu.alt <- na.omit(ascu.alt)
beic.alt <- na.omit(beic.alt)
bepu.alt <- na.omit(bepu.alt)

asca.alt <- filter(asca.alt, Altitude < 2500)
ascu.alt <- filter(ascu.alt, Altitude < 2500)
beic.alt <- filter(beic.alt, Altitude < 2500)
bepu.alt <- filter(bepu.alt, Altitude < 2500)

#combine all species df
all.alt <- rbind(asca.alt, ascu.alt, beic.alt, bepu.alt)
all.alt$Species <- as.factor(all.alt$Species)
all.alt$Scenario <- as.factor(all.alt$Scenario)
summary(all.alt)

write.csv(all.alt, file = "All_spp_suitable_altitude.csv", row.names = F)


#boxplot
altboxgg <- ggplot() + 
            geom_boxplot(aes(y = Altitude, x = Species, fill=Scenario), 
                         data=all.alt , outlier.shape = 1, outlier.size = 1) +
            scale_fill_viridis(discrete=T, option = "E", name="", direction = -1) +
            theme_ipsum() +
            ylab("Altitude (masl)")
ggsave(altboxgg, file="Boxplot2-alt-spp.png", width=19.89, height=15, units="cm", dpi=300)

#violin plot 
altgg <- ggplot(aes(y = Altitude, x = Species, fill=Scenario), data=all.alt) +
          geom_violin(position="dodge", alpha=0.5, outlier.colour="transparent") +
          scale_fill_viridis(discrete=T, option = "E", name="", direction = -1) +
          theme_ipsum() +
          ylab("Altitude (masl)")

ggsave(altgg, file="Vioplot-alt-spp.png", width=19.89, height=15, units="cm", dpi=300)

## 
#LINEAR MODELS ####
# I need to group the treatments that are not different each other together.
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}

#ASCA 
#check and transform distribution of altitude data
hist(sqrt(asca.alt$Altitude))
asca.alt$alt.sqrt <- sqrt(asca.alt$Altitude)
asca.alt <- na.omit(asca.alt)

a1 <- lm(alt.sqrt ~ Scenario, data = asca.alt)
summ(a1)
a2 <- aov(a1)

a3 <- TukeyHSD(a2, 'Scenario', conf.level = 0.95)
a3
plot(a3, las=1 , col="brown")


# Generate groups from Tukey test
a3lab <- generate_label_df(a3 , 'Scenario')
a3lab

#ASCu
#check and transform distribution of altitude data
hist(sqrt(ascu.alt$Altitude))
ascu.alt$alt.sqrt <- sqrt(ascu.alt$Altitude)
ascu.alt <- na.omit(ascu.alt)

a4 <- lm(alt.sqrt ~ Scenario, data = ascu.alt)
summ(a4)
a5 <- aov(a4)

a6 <- TukeyHSD(a5, 'Scenario', conf.level = 0.95)
a6
plot(a6, las=1 , col="brown")


# Generate groups from Tukey test
a6lab <- generate_label_df(a6 , 'Scenario')
a6lab

#beic
#check and transform distribution of altitude data
hist(beic.alt$Altitude)
hist((beic.alt$Altitude)^(1/3))
beic.alt$alt.cbrt <- (beic.alt$Altitude)^(1/3)
beic.alt <- na.omit(beic.alt)

a7 <- lm(alt.cbrt ~ Scenario, data = beic.alt)
summ(a7)
a8 <- aov(a7)

a9 <- TukeyHSD(a8, 'Scenario', conf.level = 0.95)
a9
plot(a9, las=1 , col="brown")


# Generate groups from Tukey test
a9lab <- generate_label_df(a9 , 'Scenario')
a9lab


#bepu
#check and transform distribution of altitude data
hist(bepu.alt$Altitude)
hist(sqrt(bepu.alt$Altitude))
bepu.alt$alt.sqrt <- sqrt(bepu.alt$Altitude)
bepu.alt <- na.omit(bepu.alt)

a10 <- lm(alt.sqrt ~ Scenario, data = bepu.alt)
summ(a10)
a11 <- aov(a10)

a12 <- TukeyHSD(a11, 'Scenario', conf.level = 0.95)
a12
plot(a12, las=1 , col="brown")


# Generate groups from Tukey test
a12lab <- generate_label_df(a12 , 'Scenario')
a12lab

