#CODE TO CREATE LGM BOUNDARIES OF THE PHILIPPINES DURING LAST GLACIAL MAXIMUM


library(raster)
library(rgdal)
library(rgeos)

setwd("D:/GIS_Layers/Bathymetry/")

#read in bathymetry raster file
bathymetryFile <- "D:/Chrome Downloads/GEBCO_2020_21_Sep_2020/GEBCO_2020_21_Sep_2020_ad3655fd6db6/gebco_2020_n22.0_s4.0_w116.0_e127.0.tif"
bathymetry <- raster(bathymetryFile)

#get PH shapefile
phl.shp <- getData('GADM', country = 'PHL', level=0)

#reclassify bathymetry file
rclmat <- matrix(c(-Inf,-121,NA,-120,0,1,NA,NA,1), ncol=3, byrow=TRUE)
bathymetry <- reclassify(bathymetry, rclmat)
plot(bathymetry)

#crop to PHL boundaries
bathymetry <- crop(bathymetry, phl.shp)
plot(bathymetry)

#convert to polygon
bathymetryPoly <- rasterToPolygons(bathymetry, fun=NULL, n=4, na.rm=TRUE, digits=6, dissolve=TRUE)
plot(bathymetryPoly)

writeOGR(bathymetryPoly, ".", "PHlandmassLGMPolygon30s", driver="ESRI Shapefile")
