###Calculating Zonal Statistics with R###

library(readxl) #for reading in xls file with geocodes
library(dplyr) #Package for data cleaning
library(raster) #Analyzes raster data
library(sp) #Basic spatial functions
library(rgdal) #reads shapefiles
library(rgeos) #calculates distances, buffers

#Read in geocode points
data.pts<-read_excel("select file path", col_names=TRUE)
attach(data.pts)

#Set Coordinate Reference System (CRS) as WGS 1984
wgs.84<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(data.pts)<-wgs.84


#read in raster files (variables from my M.S. thesis as examples)
ph<-raster("select file path/pH2012.tif")
sand<-raster("select file path/sand2012.tif")
soc07<-raster("select file path/soc2007.tif")
soc12<-raster("select file path/soc2012.tif")
elev<-raster("select file path/elevsrtm2.tif") 
pop10<-raster("select file path/KEN_popmap10_v2b.tif")
pop15<-raster("select file path/KEN_popmap15_v2b.tif")
avg_rain<-raster("select file path/AvgRainBunKak.tif")
tree05<-raster("select file path/TreeCover05.tif")

#extract average value from raster files based on a 1 km buffer around geocodes
pts.raster<-data.frame(data.pts,
                       extract(ph,data.pts,buffer=1000,fun=mean),
                       extract(sand,data.pts,buffer=1000,fun=mean),
                       extract(soc07,data.pts,buffer=1000,fun=mean),
                       extract(elev,data.pts,buffer=1000,fun=mean),
                       extract(pop10,data.pts,buffer=1000,fun=mean),
                       extract(pop15,data.pts,buffer=1000,fun=mean),
                       extract(avg_rain,data.pts,buffer=1000,fun=mean),
                       extract(tree05,data.pts,buffer=1000,fun=mean))

#Tell R the coordinates of this new data frame
#If projected CRS is desired, look up appropriate UTM zone
coordinates(pts.raster) <- ~XCoord+YCoord
proj4string(pts.raster)<-wgs.84



