##########################################################################
##########################################################################
##Script Name: simplifyWMAs.R
##Purpose of Script: Reads in the collected WMA spatial boundaries, does some 
## dissolves and formatting of tabular data, and saving all Water Management 
## Areas (WMA) boundaries in a singular layer. The exception to the all the 
## boundaries being in one layer are the WMA boundaries for Arizona ground 
## water, which is saved in its own layer. This was done because Arizona 
## manages its surface and ground water assets using different sets of 
## boundaries - when those assets are managed at all.
##
## Spatial Requirements: Spatial data for each state defining the WMA 
## boundaries.
## 
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 1/31/2020
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 7/27/2021
##
## Copyright (c) 2021 The Pennsylvania State University
##
##########################################################################
##########################################################################
options(stringsAsFactors=F)

##setting the paths of various directories
projCodeDir <- "/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/"
gdrBase <- "/Volumes/GoogleDrive/Shared drives/PCHES_Project1.2/Water rights project/Water institutions/Data/"
projGdrDir <- paste0(gdrBase, "waterRightsCumulations/")
dataDir <- paste0(projGdrDir, "inputData/")
rasterDir <- "/Users/mdl5548/Documents/PCHES/HYDE3_2/"

#"/Users/mdl5548/Box Sync/PCHES Project 1.2/Water institutions/State Water Management Area or Basin Maps/WECC"

##load libraries
library(rgeos)
library(rgdal)
library(readxl)
library(raster)
library(maptools)
library(pbapply)
library(parallel)
library(sf)
library(measurements)

##########################################################################
##########################################################################

##column names to be used for common layer
newNames <- c("basinNum", "basinName", "state")
##projection to use to reassemple the spatial data
projProj <- raster::crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

##split by state
##Idaho
#idaho <- wmasByState[wmasByState$State=="Idaho",]
#subColsIdaho <- idaho[,c("BasinNumbe", "State")]
idaho <- readOGR(dsn=paste0(dataDir, "idaho/adminbas/"), layer="adminbas")
idaho <- spTransform(idaho, projProj)
subColsIdaho <- idaho[,"BasinNumbe"]
subColsIdaho$basinName <- NA
subColsIdaho$state <- "Idaho"
names(subColsIdaho) <- newNames

##Washington
#washington <- wmasByState[wmasByState$State=="Washington",]
#subColsWashington <- washington[,c("State", "WRIA_NR", "WRIA_NM")]
washington <- readOGR(dsn=paste0(dataDir, "washington/WR_WAT_WRIA.gdb"), layer="Water_Resource_Inventory_Areas")
washington <- spTransform(washington, projProj)
subColsWashington <- washington[,c("WRIA_NR", "WRIA_NM")]
subColsWashington$state <- "Washington"
names(subColsWashington) <- newNames

##Oregon
#oregon <- wmasByState[wmasByState$State=="Oregon",]
#subColsOregon <- oregon[,c("State", "basin_nbr", "basin_name")]
oregon <- readOGR(dsn=paste0(dataDir, "oregon/owrd_admin_basins"), layer="owrd_admin_basins")
oregonFields <- oregon@data[,c("BASIN_NUM", "BASIN_NAME")]
oregonFields <- unique(oregonFields)
##dissolve features by basin name
dissolveOreg <- unionSpatialPolygons(oregon, oregon$BASIN_NAME)
##add attribute table to the dissolved features
pid <- sapply(slot(dissolveOreg,"polygons"),function(x){slot(x,"ID")})
matchBasinID <- match(pid, oregonFields$BASIN_NAME)
disOrgDF <- data.frame(basinNum=oregonFields$BASIN_NUM[matchBasinID], basinName=pid, state="Oregon", row.names=pid)
dissolveOreg <- SpatialPolygonsDataFrame(dissolveOreg, data=disOrgDF)
oregon <- spTransform(dissolveOreg, projProj)
subColsOregon <- spTransform(oregon, projProj)

##California
#california <- wmasByState[wmasByState$State=="California",]
california <- readOGR(dsn=paste0(dataDir, "california/GISlayers_calw221_shp"), layer="calw221")
##clean up the basin ID numbers 
caliFields <- california@data[,c("RBU", "HUNAME")]
caliFields$RBU <- as.character(caliFields$RBU)
caliFields$RBU[caliFields$HUNAME=="SOUTH VALLEY FLOOR"] <- "7551-7557-7558"
caliFields$RBU[caliFields$HUNAME=="SAN GABRIEL RIVER"] <- "4405-4845"
caliFields$RBU[caliFields$HUNAME=="SAN JOAQUIN VALLEY FLOOR"] <- "6535-6545"
caliFields$RBU[caliFields$HUNAME=="SANTA ANA RIVER"] <- "4481-4801"
caliFields <- unique(caliFields)
##dissolve features by basin name
dissolveCali <- unionSpatialPolygons(california, california$HUNAME)
##add attribute table to the dissolved features
pid <- sapply(slot(dissolveCali,"polygons"),function(x){slot(x,"ID")})
matchBasinID <- match(pid, caliFields$HUNAME)
disCaliDF <- data.frame(basinNum=caliFields$RBU[matchBasinID], basinName=pid, state="California", row.names=pid)
dissolveCali <- SpatialPolygonsDataFrame(dissolveCali, data=disCaliDF)

dissolveCali2 <- dissolveCali[-which(dissolveCali$basinNum%in%c(4406,3316)),]

subColsCali <- spTransform(dissolveCali, projProj)

##Colorado
#colorado <- wmasByState[wmasByState$State=="Colorado",]
#subColsColorado <- colorado[,c("State", "DIV", "BASIN")]
colorado <- readOGR(dsn=paste0(dataDir, "colorado/Water_Districts"), layer="Water_Districts")
colorado <- spTransform(colorado, projProj)
subColsColorado <- colorado[,c("DISTRICT", "NAME")]
subColsColorado$state <- "Colorado"
names(subColsColorado) <- newNames

##Arizona
#arizona <- wmasByState[wmasByState$State=="Arizona",]
#subColsArizona <- arizona[,c("State", "", "")]
arizonaSW <- readOGR(dsn=paste0(dataDir, "arizona/Surface_Watershed"), layer="Surface_Watershed")
arizonaSWFields <- arizonaSW@data[,c("WATERSHED_", "WATERSHED")]
arizonaSWFields <- unique(arizonaSWFields)
##dissolve features by basin name
dissArizonaSW <- unionSpatialPolygons(arizonaSW, arizonaSW$WATERSHED_)
##add attribute table to the dissolved features
pid <- sapply(slot(dissArizonaSW,"polygons"),function(x){slot(x,"ID")})
matchBasinID <- match(pid, arizonaSW$WATERSHED_)
disAZSurDF <- data.frame(basinNum=pid, basinName=arizonaSWFields$WATERSHED[matchBasinID], state="Arizona", row.names=pid)
dissArizonaSW <- SpatialPolygonsDataFrame(dissArizonaSW, data=disAZSurDF)
subColsArizonaSW <- spTransform(dissArizonaSW, projProj)
##adds the name of the missing surface watershed
subColsArizonaSW$basinName[subColsArizonaSW$basinNum=="14"]   <- "SANTA CRUZ RIVER"
##corrects a misnamed surface WMA
subColsArizonaSW$basinName[subColsArizonaSW$basinNum=="U8"]   <- "UPPER GILA RIVER"
arizonaGW <- readOGR(dsn=paste0(dataDir, "arizona/Groundwater_Basin"), layer="Groundwater_Basin")
arizonaGW <- spTransform(arizonaGW, projProj)
subColsArizonaGW <- arizonaGW[,c("NAME_ABBR", "BASIN_NAME")]
subColsArizonaGW$state <- "Arizona"
names(subColsArizonaGW) <- newNames
#arizonaGWS <- readOGR(dsn=paste0(dataDir, "arizona/Groundwater_Subbasin"), layer="Groundwater_Subbasin")
#arizonaGWS <- spTransform(arizonaGWS, projProj)

##Nevada
#nevada <- wmasByState[wmasByState$State=="Nevada",]
#subColsNevada <- nevada[,c("State", "BasinID", "BASINNAME")]
nevada <- readOGR(dsn=paste0(dataDir, "nevada/NDWR_GroundwaterBasins"), layer="NDWR_GroundwaterBasins")
nevada <- spTransform(nevada, projProj)
#nnn <- unionSpatialPolygons(nevada, nevada$HydroRegio, avoidUnaryUnion=T)
subColsNevada <- nevada[,c("BasinID", "BasinName")]
subColsNevada$state <- "Nevada"
names(subColsNevada) <- newNames

##Utah
#utah <- wmasByState[wmasByState$State=="Utah",]
#subColsUtah <- utah[,c("State", "", "BOOK_NAME")]
utah <- readOGR(dsn=paste0(dataDir, "utah/WaterRightsRegions.gdb"), layer="WaterRightsRegions")
utahFields <- unique(utah$AREA_CODE)
##dissolve features by basin name
dissolveUtah <- unionSpatialPolygons(utah, utah$AREA_CODE)
##add attribute table to the dissolved features
pid <- sapply(slot(dissolveUtah,"polygons"),function(x){slot(x,"ID")})
matchBasinID <- match(pid, utahFields)
disUtahDF <- data.frame(basinNum=utahFields[matchBasinID], basinName=NA, state="Utah", row.names=pid)
dissolveUtah <- SpatialPolygonsDataFrame(dissolveUtah, data=disUtahDF)
subColsUtah <- spTransform(dissolveUtah, projProj)

##New Mexico
#newmexico <- wmasByState[wmasByState$State=="New Mexico",]
#subColsNewMexico <- newmexico[,c("State", "", "BASIN")]
newmexico <- readOGR(dsn=paste0(dataDir, "newmexico/OSE_Groundwater_Basins.gdb"), layer="Declared_Goundwater_Basins")
#newmexicoSurWtr <- readOGR(dsn=paste0(dataDir, "newmexico/Surface_Water_Basins"), layer="Surface_Water_Basins")  ##have surface water basins, but rights only includes ground water column
newmexico <- spTransform(newmexico, projProj)
nmBasinCodeTab <- read.csv(paste0(dataDir, "newmexico/newmexicoBasinCodes.csv"))
newmexico <- merge(x=newmexico, y=nmBasinCodeTab, by.x="Basin", by.y="Description")
subColsNewMexico <- newmexico[,c("Code", "DescriptionFull")]
subColsNewMexico$state <- "NewMexico"
names(subColsNewMexico) <- newNames

##from original layer - may need to be updated############################
##read in WMA layer - made by someone else
wmasByState <- readOGR(dsn=paste0(dataDir, "allStateWMAs/"), layer="WECC_WMA")

##Montana
montana <- wmasByState[wmasByState$State=="Montana",]
montFields <- montana@data[,c("BASINNUM", "BASINNAME")]
montFields <- unique(montFields)
##dissolve features by basin name
dissolveMont <- unionSpatialPolygons(montana, montana$BASINNUM)
##add attribute table to the dissolved features
pid <- sapply(slot(dissolveMont,"polygons"),function(x){slot(x,"ID")})
matchBasinID <- match(pid, montFields$BASINNUM)
disMontDF <- data.frame(basinNum=montFields$BASINNUM[matchBasinID], basinName=montFields$BASINNAME[matchBasinID], state="Montana", row.names=pid)
dissolveMont <- SpatialPolygonsDataFrame(dissolveMont, data=disMontDF)
subColsMontana <- spTransform(dissolveMont, projProj)

##Wyoming
wyoming <- wmasByState[wmasByState$State=="Wyoming",]
wyoming <- wyoming[,c("DIVISION", "DISTRICT")]
wyoming$fullDiv <- paste0(wyoming$DIVISION, "_", wyoming$DISTRICT)
wyFields <- unique(wyoming$fullDiv)
##dissolve features by basin name
dissWyoming <- unionSpatialPolygons(wyoming, wyoming$fullDiv)
##add attribute table to the dissolved features
pid <- sapply(slot(dissWyoming,"polygons"),function(x){slot(x,"ID")})
matchBasinID <- match(pid, wyFields)
disWyDF <- data.frame(basinNum=wyFields[matchBasinID], basinName=NA, state="Wyoming", row.names=pid)
dissWyoming <- SpatialPolygonsDataFrame(dissWyoming, data=disWyDF)
subColsWyoming <- spTransform(dissWyoming, projProj)



##write out combined WMA layer
remakeWMAs <- rbind(subColsNevada, subColsMontana, subColsOregon, subColsWashington, subColsIdaho, subColsWyoming, subColsNewMexico, subColsArizonaSW, subColsUtah, 
                    subColsCali, subColsColorado)
remakeWMAs$uniID <- paste0(remakeWMAs$state, "_", remakeWMAs$basinNum)
writeOGR(remakeWMAs, dsn=paste0(dataDir, "allStateWMAs"), layer="simpleWMAs", driver="ESRI Shapefile", overwrite_layer=T) 

##Arizona ground water boundaries
subColsArizonaGW$uniID <- paste0(subColsArizonaGW$state, "_", subColsArizonaGW$basinNum)
writeOGR(subColsArizonaGW, dsn=paste0(dataDir, "allStateWMAs"), layer="azGroundWMAs", driver="ESRI Shapefile", overwrite_layer=T) 



  


#ogrListLayers(paste0(dataDir, "newmexico/OSE_Districts.gdb"))
#ogrListLayers(paste0(dataDir, "montana/MTWaterRights.gdb"))
#ogrListLayers(paste0(dataDir, "wyoming/WDOproject_BaseLayers.gdb"))
#ogrListLayers(paste0(dataDir, "wyoming/GWbasinPlanPublic.gdb"))


#pts <- readOGR(dsn=paste0(dataDir, "wyoming/POD"), layer="POD")
#wyWells <- readOGR(dsn=paste0(dataDir, "wyoming/Wells"), layer="Wells")

#grPts1 <- readOGR(dsn=paste0(dataDir, "wyoming/greenRiver/grbrtsa_shp"), layer="grbrtsa")
#grPts2 <- readOGR(dsn=paste0(dataDir, "wyoming/greenRiver/grbpod_shp"), layer="grbpoda")

#prPts1 <- readOGR(dsn=paste0(dataDir, "wyoming/platteRiver/SWDiversions.shp"), layer="SWDiversions")

#brPts <- readOGR(dsn=paste0(dataDir, "wyoming/bearRiver/waterrights_shp"), layer="waterrights")





