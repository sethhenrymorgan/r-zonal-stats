###Propensity Score Matching with Raster Data###

library(readxl)
library(MatchIt) #Propensity Score Matching
library(dplyr) #Package for data cleaning
library(raster) #Analyzes raster data
library(sp) #Basic spatial functions
library(rgdal) #reads shapefiles
library(rgeos) #calculates distances, buffers
library(geosphere) #tools for spherical, non-projected coordinate systems

#Read in data from Sirisia/Malakisi Zone
data.sir<-read_excel("select file path/SirMal10C42.xls", col_names=TRUE)
attach(data.sir)

#Creates a variable for presence of microfinance activities
data.sir$vilmicrofin<-(grp_1micro+grp_2micro+grp_3micro+grp_4micro+grp_5micro+grp_6micro+grp_7micro)
data.sir$vilmicro_yn<-0
data.sir$vilmicro_yn[data.sir$vilmicrofin>0]<-1

#Subset the data to just the needed variables
data.sir<-dplyr::select(data.sir, vilid:households, vilmicro_yn, -division, -sublocatio)

#Tells R which variables contain the coordinates
coordinates(data.sir) <- ~XCoord+YCoord

#Sets Coordinate Reference System (CRS) as WGS 1984
wgs.84<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(data.sir)<-wgs.84

#reads in raster files
ph<-raster("select file path/pH2012.tif")
sand<-raster("select file path/sand2012.tif")
soc07<-raster("select file path/soc2007.tif")
soc12<-raster("select file path/soc2012.tif")
elev<-raster("select file path/elevsrtm2.tif") 
pop10<-raster("select file path/KEN_popmap10_v2b.tif")
pop15<-raster("select file path/KEN_popmap15_v2b.tif")
avg_rain<-raster("select file path/AvgRainBunKak.tif")
tree05<-raster("select file path/TreeCover05.tif")

#extracts average value from raster files based on a 1 km buffer around geocodes
sir.raster<-data.frame(data.sir,extract(ph,data.sir,buffer=1000,fun=mean),extract(sand,data.sir,buffer=1000,fun=mean),extract(soc07,data.sir,buffer=1000,fun=mean),extract(elev,data.sir,buffer=1000,fun=mean),extract(pop10,data.sir,buffer=1000,fun=mean),extract(pop15,data.sir,buffer=1000,fun=mean),extract(avg_rain,data.sir,buffer=1000,fun=mean),extract(tree05,data.sir,buffer=1000,fun=mean))

#Tells R the coordinates of this new data frame,
#option to set CRS to UTM 36N if projected CRS desired
coordinates(sir.raster) <- ~XCoord+YCoord
#utm36N<-CRS("+init=EPSG:32636")
#proj4string(sir.raster)<-utm36N
proj4string(sir.raster)<-wgs.84

#reads in Tarmac Road shapefile (OpenStreetMap data, shapefile generated in QGis)
major_roads<-readOGR("select folder containing shapefile","tarmac_road_dissolve")
spTransform(major_roads,utm36N)


dist_tarma_R<-gDistance(sir.raster,major_roads,byid=TRUE)
dist_tarma_R<-t(dist_tarma_R)

sir.raster<-cbind.data.frame(sir.raster,dist_tarma_R)

#Creates 0.25 km buffer around Tarmac Road
#Script took a long time to run, may be better to create in QGIS
#major_roads_buff <- gBuffer(major_roads, width = 250)

#Imports road buffer shapefile
major_roads_buff<-readOGR("select folder containing shapefile","majorroadsbuff25")

#Creates binary variable for "on road" i.e. within 0.25 km from road
on_road_R<-(sir.raster %over% major_roads_buff)
on_road_R<-(on_road_R+1)
on_road_R[is.na(on_road_R)]<-0

sir.raster<-cbind.data.frame(sir.raster,on_road_R)

names(sir.raster)<-c("vilid","village","location","XCoord","YCoord","treatment","households","vilmicro_yn","optional.1","ph","sand","soc07","elev","pop10","pop15","avg_rain","tree05","optional.2","dist_tarmac","on_road")
sir.raster$optional.1<-NULL
sir.raster$optional.2<-NULL

attach(sir.raster)


#PSM Matching with R-derived variables

#Use matchit to implement Propensity Score Matching model
#Notice the caliper setting. This can be adjusted to get a tighter match with better balance,
#or a looser match with more matched observations.
#This match has 15 treatment, 15 comparison, which is what we wanted for Sirisia Zone
m.outR<-matchit(treatment~households+on_road+ph+tree05+sand+elev+pop10+soc07+avg_rain+dist_tarmac+vilmicro_yn,data=sir.raster, method="nearest",ratio=1, caliper=1.4)
summary(m.outR)

#Use the data output from matchit to create a matched dataset
m.dataR<-match.data(m.outR)
match_covR <- dplyr::select(m.dataR,treatment, households, ph, pop15, tree05, sand, elev, pop10, soc07, avg_rain, dist_tarmac)

#Perform a series of t-tests in order to check sample balance:
bal.estR<-sapply(match_covR[, -c(1)], function(x) {
  t.test(x~match_covR$treatment)$estimate
})
bal.pR<-sapply(match_covR[, -c(1)], function(x) {
  t.test(x~match_covR$treatment)$p.value
})
bal.pR<-t(t(bal.pR))
bal.estR<-t(bal.estR)
bal.matR<-cbind(bal.estR,bal.pR)
colnames(bal.matR)<-c("Comparison","Treatment","P-Value")
bal.dfR<-as.data.frame(bal.matR)
bal.dfR$Diff <- abs(bal.dfR$Treatment - bal.dfR$Comparison)
bal.dfR$AvgDiff <- mean(bal.dfR$Diff)
bal.dfR

#none of the variables are statistically significant, so the sample is well-balanced!
