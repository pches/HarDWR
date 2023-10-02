##########################################################################
##########################################################################
## Script Name: dataPrep.R
## Purpose of Script: Takes all of the collected water rights data, cleans out 
## the records with missing values, and converts all of the flows or 
## volumes into CFS. For volumes, it is assumed to be volume/year, and 
## therefore is considered a flow. Records belong to the basin that the record 
## is stated to be it; this does mean that there are some records which 
## spatially fall outside of the stated basin which we are still considering to 
## belong to said basin.
##
## Special Requirements: Water rights collected from each relevant state 
## agency. Spatial boundaries defining the WMAs.
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 1/31/2020
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 10/2/2023
##
## Copyright (c) 2023 The Pennsylvania State University
##
##########################################################################
##########################################################################
##setting so that numbers will not be displayed using scientific notation and
##that string data will not automatically be read in as factors
options(stringsAsFactors=F, scipen=999)

##setting the paths of various directories, including where to read in
##data, additional code, and were to write the formatted data to
##local machine
#projCodeDir <- "/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/"
#projBoxDir <- "/Users/mdl5548/Library/CloudStorage/GoogleDrive-mdl5548@psu.edu/Shared drives/PCHES_Project1.2/Water rights project/Water institutions/Data/waterRightsCumulations/"
##parallels machine
##to access:
##singularity shell --bind '/media/psf/Home/Library/cloudStorage/GoogleDrive-mdl5548@psu.edu/Shared drives/PCHES_Project1.2/Water rights project/Water institutions/Data/waterRightsCumulations':/dataDir,/media/psf/Home/Documents/GitHub/waterRightsCumulationCurves:/codeDir ./waterRightAnalysis.sif
projCodeDir <- "/codeDir/"
projBoxDir <- "/dataDir/"
dataDir <- paste0(projBoxDir, "inputData/")
outDir <- paste0(projBoxDir, "output/")
##create the output data directory, if needed
if(dir.exists(outDir)==F){dir.create(outDir)} 

##load libraries
library(rgeos)
library(rgdal)
library(maptools)
library(knitr)
library(lubridate)
library(dplyr)
library(tidyr)
library(zoo)
library(pbapply)
library(measurements)
library(gdata)
library(raster)
library(BBmisc)
library(stringr)

##source custom functions
source(paste0(projCodeDir, "wrDataHarmonization_CustomFunctions.R"))

##common projection for spatial data
projProj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

##selected states to prepare data for
#states <- "idaho"
states <- c("idaho", "oregon", "california", "colorado", "arizona", "nevada", "utah", "newMexico", "montana", "wyoming", "washington")

##creating various output object list
rightsByState_surface <- rightsByState_ground <- h2oUseByState <- h2oDivByState <- spatialWMAByState <- wmaStateLabel <- wmaIDByState <- fullRightsRecs <- sectorClassByState <- plottingDim <- vector("list", length(states))

##the column name of the column which will hold the unique basin name in output
basinIDFieldName <- "basinNum" 
##the column name of the column which will hold the unique water right ID in output
waterRightIDFieldName <- "waterRightID"  

##read in the prepared WMA polygon boundaries
stateWMAs <- readOGR(paste0(dataDir, "allStateWMAs"), layer="simpleWMAs")
if(stateWMAs@proj4string@projargs!=projProj@projargs){
  stateWMAs <- spTransform(stateWMAs, projProj)
}

##########################################################################
##prepare the data for various states
##Idaho
if("idaho" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "idaho/")
  whichState <- which(states=="idaho")
  plottingDim[[whichState]] <- "tall"
  
  ##load base data and convert into data frame
  ##water right place of use
  h2oUseShp <- readOGR(dsn=paste0(stateDataDir, "WaterRightPlaceOfUse/"), layer="WaterRightPlaceOfUse")
  if(h2oUseShp@proj4string@projargs!=projProj@projargs){
    projh2oUse <- spTransform(h2oUseShp, projProj)
  }else{
    projh2oUse <- h2oUseShp
  }
  names(projh2oUse)[match(c("RightID","BasinNumbe","Source"), names(projh2oUse))] <- c(waterRightIDFieldName, basinIDFieldName, "source")
  h2oUse <- projh2oUse@data
  
  ##water right point of diversion
  h2oDivShp <- readOGR(dsn=paste0(stateDataDir, "WaterRightPointOfDiversion/"), layer="WaterRightPointOfDiversion")
  if(h2oDivShp@proj4string@projargs!=projProj@projargs){
    projh2oDiv <- spTransform(h2oDivShp, projProj)
  }else{
    projh2oDiv <- h2oDivShp
  }
  names(projh2oDiv)[match(c("RightID","BasinNumbe","Source"), names(projh2oDiv))] <- c(waterRightIDFieldName, basinIDFieldName, "source")
  h2oDiv <- projh2oDiv@data
  
  ##idaho WMAs
  spatialWMAByState[[whichState]] <- stateWMAs[stateWMAs$state=="Idaho",]
  wmaStateLabel[[whichState]] <- "WMA"
  
  ##Base data has some Water Use Codes = NA.  These are mostly federal water rights.  Replacement data provided by  
  ##Shelley Keen at the Idaho Department of Water Resources.
  fedUse <- read.csv(paste0(stateDataDir,"UseDescriptionTable.csv"))
  
  ##read in supplementary data
  supRights <- read.csv(paste0(stateDataDir,"all_docs_table_REFORMAT.csv"))
  colnames(supRights) <- c("WaterRight.x", "Total_Diversion", "Beneficial_Use", "From", "To", "Flow_Rate", "Volume")
  
  ##water use categories.
  ##format: the first column contains character strings that can be used for regular expression matching
  ##        the second column contains the sector to which the character string is assigned
  ##put the regular expressions into a table
  idSectorMatch <- read.csv(paste0(stateDataDir,"Idaho_sector_character_match.csv"))

  ##merging water use and water div tables
  ##select only the columns needed for further processing
  h2oRights <- merge(h2oDiv, h2oUse, by=waterRightIDFieldName, all=T)
  h2oRights$waterRightID <- as.numeric(as.character(h2oRights$waterRightID))  ##to change values to numeric from factor
  
  ##Code used to get the urls of federal rights, download water right docs
  linkPt1 <- "http://www.idwr.idaho.gov/apps/ExtSearch/RightReportAJ.asp?BasinNumber="
  linkPt2 <- "&TypewrWaterRight=True&SplitSuffix="
  linkPt3 <- "&SequenceNumber="
  
  basin <- sapply(strsplit(fedUse$WaterRight.x, "-"), "[[", 1)
  sequenceSuffix <- sapply(fedUse$WaterRight.x, function(x){strsplit(x, "-")[[1]][2]})
  ##identifies any alpha characters in the suffix of the basin identifiers
  suffixAlpha <- strsplit(sequenceSuffix, "[0-9]")
  ##unlists the alpha characters from blank strings
  suffixCleanAlpha <- sapply(suffixAlpha, function(x){unl<-unlist(x);
                                                      if(is.na(unl[1])==F){counl<-paste(unl,collapse="");
                                                        if(counl==""){return(NA)
                                                        }else{return(counl)}
                                                      }else{return(NA)}})
  ##remove the alpha characters from the basin identifiers
  suffixClean <- unlist(mapply(strsplit, x=sequenceSuffix, split=suffixCleanAlpha))
  ##convert NAs to blanks go the NA does not appear in the generated link
  suffixClean[which(is.na(suffixClean)==T)] <- ""
  suffixCleanAlpha[which(is.na(suffixCleanAlpha)==T)] <- ""
  
  ##create the url links for the federal wma data
  fedUse$WRReport <- paste0(linkPt1, basin, linkPt2, suffixCleanAlpha, linkPt3, suffixClean)
  ##remove, essentially, the NA, some entries have a WaterRight code that can't be used to generate the url
  fedUseSub <- subset(fedUse, grepl("-", as.character(fedUse$WaterRight)))
  names(fedUse)[which(names(fedUse)=="WRReport")] <- c("WRReport.x")
  
  ##merge base data with replacement federal data
  h2oRights <- h2oRights[-c(which(is.na(h2oRights$WaterUseCo)==T)),]
  h2oRights$basinNum.x <- as.numeric(as.character(h2oRights$basinNum.x)) ##change from factor to numeric
  h2oRights$VersionNum   <- as.numeric(as.character(h2oRights$VersionNum)) ##change from factor to numeric
  h2oRights <- bind_rows(h2oRights, fedUse)
  
  ##get ride of NA rows from the supplimental data
  supRights <- supRights[-c(which(is.na(supRights$WaterRight.x)==T)),]
  
  ##Format data to be numeric and fill in empty cells with NA
  supRights$Flow_Rate <- as.character(supRights$Flow_Rate)
  supRights$Flow_Rate <- as.numeric(strsplit(supRights$Flow_Rate, split = "CFS"))
  supRights$Total_Diversion <- as.character(supRights$Total_Diversion)
  supRights$Total_Diversion <- as.numeric(strsplit(supRights$Total_Diversion, split = "CFS"))
  supRights$Volume <- as.character(supRights$Volume)
  supRights$Volume <- as.numeric(strsplit(supRights[,7], split = "AFA"))
  
  ##find each unique right and merge based on the WaterRight.x
  nonDup <- filter(h2oRights, duplicated(h2oRights$WaterRight.x)==F)
  
  fullRights <- merge(supRights, nonDup, by="WaterRight.x", all=T)
  fullRights <- fullRights[,c(waterRightIDFieldName, "basinNum.x", "PriorityDa.x", "OverallMax", "source.x", "WaterUse", "Volume")]
  colnames(fullRights) <- c(waterRightIDFieldName, basinIDFieldName, "priorityDate", "CFS", "source", "origWaterUse", "volume")
  fullRights <- fullRights[fullRights$CFS>=0,]
  ##remove invalid water rights (waterRightID==NA or source=NA)
  fullRights <- fullRights[is.na(fullRights$waterRightID)==F,]
  fullRights <- fullRights[is.na(fullRights$source)==F,]
  fullRights <- fullRights[is.na(fullRights$priorityDate)==F,]
  fullRights <- fullRights[is.na(fullRights$basinNum)==F,]
  fullRights$source[fullRights$source!="GROUND WATER"] <- "SURFACE WATER"
  fullRights$waterUse <- NA
  ##preform sector matching now instead of later  
  sectCats <- c("irrigation", "domestic", "livestock", "fish", "industrial", "environmental", "other")
  for(sc in sectCats){
    #sc <- "domestic"
    subIDTab <- idSectorMatch[idSectorMatch$sector==sc,]
    fullRights$waterUse[grep(paste(subIDTab$character,collapse="|"), as.character(fullRights$origWaterUse))] <- sc
  }
  fullRightsRecs[[whichState]] <- fullRights
  fullWMAs <- as.numeric(unique(fullRights$basinNum))
  wmaIDByState[[whichState]] <- fullWMAs[is.na(fullWMAs)==F]
  
  ##for the spatial object to be passed on for plotting, only include those records which will be used in the analysis
  projh2oUse$source[projh2oUse$source!="GROUND WATER"] <- "SURFACE WATER"
  projh2oDiv$source[projh2oDiv$source!="GROUND WATER"] <- "SURFACE WATER"
  h2oUseByState[[whichState]] <- projh2oUse[which(projh2oUse$waterRightID %in% fullRights$waterRightID),]
  h2oDivByState[[whichState]] <- projh2oDiv[which(projh2oDiv$waterRightID %in% fullRights$waterRightID),]
  
  ##seperate rights by surface and ground
  ##adds to the object to be passed along for analysis
  rightsByState_ground[[whichState]] <- fullRights[fullRights$source=="GROUND WATER",]
  rightsByState_surface[[whichState]] <- fullRights[fullRights$source=="SURFACE WATER",]
}
##Washington
if("washington" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "washington/")
  whichState <- which(states=="washington")
  plottingDim[[whichState]] <- "wide"
  
  ##Contents of the GWIS_Data geodatabase
  ##D_Point - feature class, https://fortress.wa.gov/ecy/gispublic/DataDownload/wr/GWIS_Data/GWIS_Data_Dictionary/code01.html
  ##D_Point_WR_Doc - table, https://fortress.wa.gov/ecy/gispublic/DataDownload/wr/GWIS_Data/GWIS_Data_Dictionary/code04.html
  ##WR_Doc_POU1 - feature class, https://fortress.wa.gov/ecy/gispublic/DataDownload/wr/GWIS_Data/GWIS_Data_Dictionary/code02.html
  ##WR_Doc_POU1 had to be extracted in ArcGIS as had read issues in this script
  
  ##load state water rights data
  ##water right place of use
  h2oUseShp <- readOGR(dsn=stateDataDir, layer="WR_Doc_POU1")
  ##if needed, convert the spatial projection into the common projection
  if(h2oUseShp@proj4string@projargs!=projProj@projargs){
    projh2oUse <- spTransform(h2oUseShp, projProj)
  }else{
    projh2oUse <- h2oUseShp
  }
  ##apply waterRightIDFieldName, so that common for all states
  names(projh2oUse)[which(names(projh2oUse)=="WR_DOC_ID")] <- waterRightIDFieldName
  ##convert spatial to dataframe
  h2oUse <- projh2oUse@data
  
  ##water right point of diversion
  h2oDivShp <- readOGR(dsn=paste0(stateDataDir, "WR_GEO_WaterDiversions_ECY_NHD.gdb"), layer="WaterDiversions_ECY_NHD")
  ##if needed, convert the spatial projection into the common projection
  if(h2oDivShp@proj4string@projargs!=projProj@projargs){
    projh2oDiv <- spTransform(h2oDivShp, projProj)
  }else{
    projh2oDiv <- h2oDivShp
  }
  ##apply waterRightIDFieldName, so that common for all states
  names(projh2oDiv)[which(names(projh2oDiv)=="WR_DOC_ID")] <- waterRightIDFieldName
  ##remove records with no valid record identifies (WR_DOC_ID==NA), and subset columns by only those needed
  projh2oDiv <- projh2oDiv[is.na(projh2oDiv$waterRightID)==F, c(waterRightIDFieldName, "purpose_li", "doc_cfs_qt", "doc_gpm_qt", "doc_acre_f")]
  ##convert spatial to dataframe
  h2oDiv <- projh2oDiv@data
  
  ##washington WRIAs, used as WMAs
  spatialWMAByState[[whichState]] <- stateWMAs[stateWMAs$state=="Washington",]
  wmaStateLabel[[whichState]] <- "WRIA"
  
  ##classifying water use categories into WBM sectors
  sectorTab <- read.csv(paste0(stateDataDir,"WA_sectorMatch.csv"))
  colnames(sectorTab)[which(colnames(sectorTab)%in%"Sector")] <- "waterUse"
  sectorTab <- sectorTab[,sapply(c("Code", "waterUse"), function(x){grep(x, colnames(sectorTab))})]
  colnames(sectorTab)[1] <- "Code"
  
  ##read in tabular region data to get the Priority Date
  regionFiles <- list.files(stateDataDir, "RegionWater", full.names=T, recursive=T)
  regionData <- lapply(regionFiles, read.csv)
  regionData <- do.call(rbind.data.frame, regionData)
  regionData$WR.Doc.ID <- as.numeric(regionData$WR.Doc.ID)
  ##remove records with no valid record identifies (WR.Doc.ID==NA), and subset columns by only those needed
  regionData <- regionData[is.na(regionData$WR.Doc.ID)==F, c("WR.Doc.ID", "Priority.Date.Claim.First.Use", "WRIA")]
  
  ##merge the diversion and region records into one table
  h2oRights <- merge(x=h2oDiv, y=regionData, by.x=waterRightIDFieldName, by.y="WR.Doc.ID")
  ##remove duplicated rights, biasing the first records as with Idaho
  nonDupRights <- filter(h2oRights, duplicated(h2oRights$waterRightID)==F)
  ##calculation the CFS for ground water records
  gwRecs <- which(nonDupRights$doc_gpm_qt>0)
  nonDupRights$doc_cfs_qt[gwRecs] <- conv_unit(nonDupRights$doc_gpm_qt[gwRecs], "gal_per_min", "ft3_per_sec")
  nonDupRights <- nonDupRights[,-which(colnames(nonDupRights)=="doc_gpm_qt")] 
  colnames(nonDupRights) <- c(waterRightIDFieldName, "origWaterUse", "CFS", "volume", "priorityDate", basinIDFieldName)
  
  ##reformat date colume, to be in same format as Idaho year/month/day
  nonDupRights$priorityDate <- format(as.POSIXct(nonDupRights$priorityDate, format="%m/%d/%Y"), format="%Y/%m/%d")
  
  ##add source column, for completeness
  nonDupRights$source <- "SURFACE WATER"
  nonDupRights$source[gwRecs] <- "GROUND WATER"
  
  ##remove invalid water rights, cause by NA that will produce errors in analysis
  nonDupRights <- nonDupRights[is.na(nonDupRights$waterRightID)==F,]
  nonDupRights <- nonDupRights[is.na(nonDupRights$priorityDate)==F,]
  nonDupRights <- nonDupRights[is.na(nonDupRights$basinNum)==F,]
  ##for rights with multiple uses, select the first use as the only use, as with the idaho data
  nonDupRights$origWaterUse <- sapply(strsplit(nonDupRights$origWaterUse, " "), "[[", 1)
  ##adding water use of the rights for later processing, make sure there are no duplicates
  nonDupRights <- merge(x=nonDupRights, y=sectorTab, by.x="origWaterUse", by.y="Code", sort=F)
  nonDupRights <- nonDupRights[nonDupRights$CFS>=0,]
  
  ##assigning rights to output objects
  fullRightsRecs[[whichState]] <- nonDupRights
  ##getting and assigning unique WMA ids to output object
  fullWRIAs <- as.numeric(unique(nonDupRights$basinNum))
  wmaIDByState[[whichState]] <- fullWRIAs[is.na(fullWRIAs)==F]
  
  ##for the spatial object to be passed on for plotting, only include those records which will be used in the analysis
  subProjUse <- projh2oUse[which(projh2oUse$waterRightID %in% nonDupRights$waterRightID),]
  subProjDiv <- projh2oDiv[which(projh2oDiv$waterRightID %in% nonDupRights$waterRightID),]
  
  ##add source and basinNum to the spatial data, for plotting reference maps 
  subProjUse$source <- nonDupRights$source[match(subProjUse$waterRightID, nonDupRights$waterRightID)]
  subProjDiv$source <- nonDupRights$source[match(subProjDiv$waterRightID, nonDupRights$waterRightID)]
  subProjUse$basinNum <- nonDupRights$basinNum[match(subProjUse$waterRightID, nonDupRights$waterRightID)]
  subProjDiv$basinNum <- nonDupRights$basinNum[match(subProjDiv$waterRightID, nonDupRights$waterRightID)]
  h2oUseByState[[whichState]] <- subProjUse
  h2oDivByState[[whichState]] <- subProjDiv
  
  ##adds to the object to be passed along for analysis
  rightsByState_ground[[whichState]] <- nonDupRights[which(nonDupRights$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- nonDupRights[which(nonDupRights$source=="SURFACE WATER"),]
}
##Oregon
if("oregon" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "oregon/")
  whichState <- which(states=="oregon")
  plottingDim[[whichState]] <- "wide"
  
  ##oregon adminBasins - WMAs
  spatialWMAByState[[whichState]] <- stateWMAs[stateWMAs$state=="Oregon",]
  wmaStateLabel[[whichState]] <- "Admin Basin"
  
  ##all of the provided Oregon water rights use layers have no features, as this throws an error, they are not read in
  ##list the PoD file names, and isolate the file names to be read in as layers for the readOGR function
  listPODFiles <- list.files(paste0(stateDataDir, "wr_state_shp/"), pattern="_pod.shp$", full.names=T, recursive=T)
  isolateFileName <- sapply(strsplit(sapply(strsplit(listPODFiles, "/"), "[[", length(strsplit(listPODFiles, "/")[[1]])), ".shp"), "[[", 1)
  translateBasins <- readOGR(dsn=paste0(stateDataDir, "owrd_admin_basins"), layer="owrd_admin_basins")
  ##read in the various point shapefiles for the PoD records
  h2oDivShps <- lapply(isolateFileName, function(bas){shp<-readOGR(dsn=paste0(stateDataDir, "wr_state_shp/"), layer=bas);
                                                        basinShort<-sapply(strsplit(bas,"_"),"[[",2);
                                                        shp$basinNum<-translateBasins$BASIN_NUM[which(translateBasins$SHORTNAME==basinShort)[1]];
                                                        return(shp)})
  ##bind all of the PoD records into one object
  h2oDivShp <- do.call(rbind, h2oDivShps)
  ##if needed, convert the spatial records into the common projection
  if(h2oDivShp@proj4string@projargs!=projProj@projargs){
    projh2oDiv <- spTransform(h2oDivShp, projProj)
  }else{
    projh2oDiv <- h2oDivShp
  }
  ##apply waterRightIDFieldName, so that common for all states
  names(projh2oDiv)[which(names(projh2oDiv)=="pod_use_id")] <- waterRightIDFieldName
  ##differentiate between surface and ground water records
  projh2oDiv$wr_type[which(projh2oDiv$wr_type=="GW")] <- "GROUND WATER"
  projh2oDiv$wr_type[which(projh2oDiv$wr_type!="GROUND WATER")] <- "SURFACE WATER"
  ##subset the rights data to only keep the columns needed
  projh2oDiv <- projh2oDiv[,c("waterRightID", "use_code", "max_rate_c", "priority", "basinNum", "wr_type")]
  names(projh2oDiv) <- c(waterRightIDFieldName, "origWaterUse", "CFS", "priorityDate", basinIDFieldName, "source")  ##priorityData already in Year/Month/Day
  h2oDiv <- projh2oDiv@data
  
  ##classifying water use categories into WBM sectors
  sectorTab <- read.csv(paste0(stateDataDir,"OR_sectorMatch.csv"))
  colnames(sectorTab)[which(colnames(sectorTab)%in%"Sector")] <- "waterUse"
  colnames(sectorTab)[1] <- "Code"
  sectorTab <- sectorTab[,c("Code", "waterUse")]
  
  ##remove invalid water rights, cause by NA that will produce errors in analysis
  h2oDiv <- h2oDiv[is.na(h2oDiv$priorityDate)==F,]
  ##adding water use of the rights for later processing
  h2oDiv <- merge(x=h2oDiv, y=sectorTab, by.x="origWaterUse", by.y="Code", sort=F)
  h2oDiv <- h2oDiv[h2oDiv$CFS>=0,]
  fullRightsRecs[[whichState]] <- h2oDiv
  wmaIDByState[[whichState]] <- as.numeric(unique(h2oDiv$basinNum))
  
  ##for the spatial object to be passed on for plotting, only include those records which will be used in the analysis
  h2oUseByState[[whichState]] <- -9999
  h2oDivByState[[whichState]] <- projh2oDiv[projh2oDiv$waterRightID %in% h2oDiv$waterRightID,]
  
  ##need to rename source to work with plotting code
  rightsByState_ground[[whichState]] <- h2oDiv[which(h2oDiv$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- h2oDiv[which(h2oDiv$source=="SURFACE WATER"),]
}
##California
if("california" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "california/")
  whichState <- which(states=="california")
  plottingDim[[whichState]] <- "tall"
  
  ##california watershed units
  wShedUnit <- stateWMAs[stateWMAs$state=="California",]
  spatialWMAByState[[whichState]] <- wShedUnit
  wmaStateLabel[[whichState]] <- "Watershed Unit"
  
  ##list all of the relevant water right PoD files
  listPODFiles <- list.files(paste0(stateDataDir, "eWRIMS_directDatabase/"), pattern=".xls", full.names=T, recursive=T)
  filesByCounty <- listPODFiles[grep("/byCounty/", listPODFiles)]
  filesByUse <- listPODFiles[grep("/byUse/", listPODFiles)]
  filesByEntity <- listPODFiles[grep("/byEntity/", listPODFiles)]
  filesByWatershed <- listPODFiles[grep("/byWatershed/", listPODFiles)]
  ##read in the various types of records to create the full PoD for CA
  ##this is done as it was found that not all records can be collected from a single look up method
  ##finally, bind each type into a single data frame
  h2oDivCountyRecs <- do.call(rbind.data.frame, pblapply(filesByCounty, readInCaliPODRecs))
  h2oDivUseRecs <- do.call(rbind.data.frame, pblapply(filesByUse, readInCaliPODRecs))
  h2oDivEntityRecs <- do.call(rbind.data.frame, pblapply(filesByEntity, readInCaliPODRecs))
  h2oDivWatershedRecs <- do.call(rbind.data.frame, pblapply(filesByWatershed, readInCaliPODRecs))
  ##reorganize the columns from the various PoD types so that they are all formatted the same
  h2oDivCountyRecs <- h2oDivCountyRecs[,c("POD.ID", "Beneficial.Use", "POD.Direct.Diversion.Rate", "DD.Unit", "Status.Date", "Watershed", "Source", "Latitude", "Longitude")]
  h2oDivUseRecs <- h2oDivUseRecs[,c("POD.ID", "Beneficial.Use", "POD.Direct.Diversion.Rate", "DD.Unit", "Status.Date", "Watershed", "Source", "Latitude", "Longitude")]
  h2oDivEntityRecs <- h2oDivEntityRecs[,c("POD.ID", "Beneficial.Use", "POD.Direct.Diversion.Rate", "DD.Unit", "Status.Date", "Watershed", "Source", "Latitude", "Longitude")]
  h2oDivWatershedRecs <- h2oDivWatershedRecs[,c("POD.ID", "Beneficial.Use", "POD.Direct.Diversion.Rate", "DD.Unit", "Status.Date", "Watershed", "Source", "Latitude", "Longitude")]
  ##rename columns to standard names
  colnames(h2oDivCountyRecs) <- colnames(h2oDivUseRecs) <- colnames(h2oDivEntityRecs) <- colnames(h2oDivWatershedRecs) <- c(waterRightIDFieldName, "origWaterUse", "CFS", "CFS_units", "priorityDate", "basinName", "source", "latitude", "longitude")
  
  ##clean the California records of various items, including removing invalid water rights
  h2oRights <- lapply(unique(h2oDivUseRecs$origWaterUse), function(wUse){print(wUse);
                                                                          reorgCaliRecs(h2oDivCountyRecs[h2oDivCountyRecs$origWaterUse==wUse,], h2oDivUseRecs[h2oDivUseRecs$origWaterUse==wUse,], 
                                                                                     h2oDivEntityRecs[h2oDivEntityRecs$origWaterUse==wUse,], h2oDivWatershedRecs[h2oDivWatershedRecs$origWaterUse==wUse,])})
  h2oRights <- do.call(rbind.data.frame, h2oRights)
  ##correct the names of a couple of WMAs
  h2oRights$basinName[which(h2oRights$basinName=="SANTA  MARIA")] <- "SANTA MARIA"
  h2oRights$basinName[which(h2oRights$basinName=="BOLSA NEUVA")] <- "BOLSA NUEVA"
  ##adds the basin id number to the data frame
  addBasinIDNums <- merge(x=h2oRights, y=wShedUnit, by="basinName")
  ##remove duplicate rights, biasing the first record as with Idaho
  nonDupRights <- filter(addBasinIDNums, duplicated(addBasinIDNums$waterRightID)==F)
  nonDupRights <- nonDupRights[nonDupRights$CFS>=0,]
  
  ##classifying water use categories into WBM sectors
  sectorTab <- read.csv(paste0(stateDataDir,"CA_sectorMatch.csv"))
  colnames(sectorTab)[which(colnames(sectorTab)%in%"Sector")] <- "waterUse"
  colnames(sectorTab)[1] <- "Use"
  sectorTab <- sectorTab[,c("Use", "waterUse")]
  
  ##adding the water use classification to the full right record
  nonDupRights <- merge(x=nonDupRights, y=sectorTab, by.x="origWaterUse", by.y="Use", sort=F)
  setRights <- nonDupRights[,-c(which(colnames(nonDupRights) %in% c("latitude", "longitude", "state", "basinName")))]
  wmaIDByState[[whichState]] <- as.character(unique(setRights$basinNum))
  fullRightsRecs[[whichState]] <- setRights
  ##split rights into surface and ground
  rightsByState_ground[[whichState]] <- setRights[which(setRights$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- setRights[which(setRights$source=="SURFACE WATER"),]
  
  ##for the spatial object to be passed on for plotting, only include those records which will be used in the analysis
  h2oUseByState[[whichState]] <- -9999
  h2oDivByState[[whichState]] <- SpatialPointsDataFrame(nonDupRights[,c("longitude","latitude")], data=nonDupRights[,-c(which(colnames(nonDupRights) %in% c("latitude", "longitude", "state", "basinName")))], proj4string=projProj)
}
##Colorado
if("colorado" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "colorado/")
  whichState <- which(states=="colorado")
  plottingDim[[whichState]] <- "wide"
  
  ##colorado watershed units
  spatialWMAByState[[whichState]] <- stateWMAs[stateWMAs$state=="Colorado",]
  wmaStateLabel[[whichState]] <- "Water District"
  
  ##classifying water use categories into WBM sectors
  sectorTab <- read.csv(paste0(stateDataDir,"CO_sectorMatch.csv"))
  colnames(sectorTab)[which(colnames(sectorTab)%in%"Sector")] <- "waterUse"
  sectorTab <- sectorTab[,sapply(c("Code", "waterUse"), function(x){grep(x, colnames(sectorTab))})]
  
  ##water right net amounts, assumed to be equivalent to points of diversion
  listPODFiles <- list.files(paste0(stateDataDir, "waterRightNetAmounts/byDistrict/"), pattern=".csv", full.names=T, recursive=T)
  h2oRights <- do.call(rbind.data.frame, lapply(listPODFiles, read.csv))
  ##subset the rights data to only keep the columns needed
  h2oRights <- h2oRights[,c("WDID", "Decreed.Use.s.", "Net.Absolute", "Decreed.Units", "Appropriation.Date", "WD", "Water.Source", "Latitude", "Longitude", "UTM.X", "UTM.Y")]
  colnames(h2oRights)[1:7] <- c(waterRightIDFieldName, "origWaterUse", "CFS", "Decreed.Units", "priorityDate", basinIDFieldName, "source") 
  ##massage data to be in the same units and format as the other states in the project
  h2oRights$CFS <- as.numeric(h2oRights$CFS)  ##turn the water use amount into numeric
  h2oRights <- h2oRights[is.na(h2oRights$CFS)==F,]
  ##convert acre-foot per year to CFS
  h2oRights$CFS[h2oRights$Decreed.Units=="A"] <- acreFtYr2ft3Sec(h2oRights$CFS[h2oRights$Decreed.Units=="A"])
  h2oRights <- h2oRights[,-c(which(colnames(h2oRights)=="Decreed.Units"))]
  ##classify water sources into ground and surface water
  h2oRights$source[grep("GROUNDWATER", h2oRights$source)] <- "GROUND WATER"
  h2oRights$source[h2oRights$source!="GROUND WATER"] <- "SURFACE WATER"
  ##remove multiple uses for a single right, assuming that the first listed use is the more important
  h2oRights$origWaterUse <- substr(h2oRights$origWaterUse, 1, 1)
  h2oRights <- h2oRights[h2oRights$origWaterUse!="",]  ##remove blank entries
  ##reformat priority date, to be in same format as Idaho year/month/day
  h2oRights$priorityDate <- format(as.POSIXct(h2oRights$priorityDate, format="%m/%d/%Y"), format="%Y/%m/%d")
  
  ##need to double check NAs in Latitude and Longitude
  h2oRights$Latitude <- as.numeric(h2oRights$Latitude)
  h2oRights$Longitude <- as.numeric(h2oRights$Longitude)
  h2oRights$UTM.X <- as.numeric(h2oRights$UTM.X)
  h2oRights$UTM.Y <- as.numeric(h2oRights$UTM.Y)

  ##there are some duplicate water right IDs, however, they typically have differing dates and CFS, so they are being left in analysis
  nonDupRights <- unique(h2oRights)
  nonDupRights <- nonDupRights[nonDupRights$CFS>=0,]
  ##adding water use of the rights for later processing
  nonDupRights <- merge(x=nonDupRights, y=sectorTab, by.x="origWaterUse", by.y="Code", sort=F)
  setRights <- nonDupRights[,-c(which(colnames(nonDupRights) %in% c("Latitude", "Longitude", "UTM.X", "UTM.Y")))]
  wmaIDByState[[whichState]] <- as.character(unique(nonDupRights$basinNum))
  fullRightsRecs[[whichState]] <- setRights
  ##split rights into surface and ground
  rightsByState_ground[[whichState]] <- setRights[which(nonDupRights$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- setRights[which(nonDupRights$source=="SURFACE WATER"),]
  
  ##there are no records with clean UTM coordinate but no geo coordinates
  ##Due to this, just useing geo coordinates
  withGeo <- nonDupRights[which(is.na(nonDupRights$Latitude)==F & is.na(nonDupRights$Longitude)==F),]
  spatialRights <- SpatialPointsDataFrame(withGeo[,c("Longitude","Latitude")], data=withGeo[,-c(which(colnames(withGeo) %in% c("Longitude","Latitude","UTM.X","UTM.Y")))], proj4string=projProj)
  
  ##clean a handful of bad coordinates which are able to be cleaned, totally 5 records
  utm13InGeo <- withGeo[withGeo$Latitude>90 | withGeo$Longitude>180,]
  utm13InGeo$Longitude[utm13InGeo$Longitude<100] <- utm13InGeo$Latitude[utm13InGeo$Longitude<100]
  utm13InGeo$Latitude[utm13InGeo$Latitude>1000000] <- utm13InGeo$UTM.Y[utm13InGeo$Latitude>1000000]
  utm13SPRights <- SpatialPointsDataFrame(utm13InGeo[,c("Latitude","Longitude")], data=utm13InGeo[,-c(which(colnames(utm13InGeo) %in% c("Longitude","Latitude","UTM.X","UTM.Y")))], proj4string=CRS("+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "))  ##incorrect coordinate order
  projUTM13 <- spTransform(utm13SPRights, projProj)
  ##add the cleaned rights to the full body of spatial rights
  h2oDivByState[[whichState]] <- rbind(spatialRights, projUTM13)
  h2oUseByState[[whichState]] <- -9999
}
##Arizona - surface records
if("arizona" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "arizona/")
  whichState <- which(states=="arizona")
  plottingDim[[whichState]] <- "tall"
  
  ##Arizona watershed units - WMAs
  surfaceWSheds <- stateWMAs[stateWMAs$state=="Arizona",]
  groundWBasins <- readOGR(dsn=paste0(dataDir, "allStateWMAs"), layer="azGroundWMAs")
  wmaStateLabel[[whichState]] <- "Watersheds"
  
  ##classifying water use categories into WBM sectors
  sectorTabSOC <- read.csv(paste0(stateDataDir,"AZ_SOC_sectorMatch.csv"))
  colnames(sectorTabSOC)[which(colnames(sectorTabSOC)%in%"Sector")] <- "waterUse"
  colnames(sectorTabSOC)[1] <- "Code"
  sectorTabSOC <- sectorTabSOC[,c("Code", "waterUse")]
  sectorTabSWR <- read.csv(paste0(stateDataDir,"AZ_SWR_sectorMatch.csv"))
  colnames(sectorTabSWR)[which(colnames(sectorTabSWR)%in%"Sector")] <- "waterUse"
  colnames(sectorTabSWR)[1] <- "Use"
  sectorTabSWR <- sectorTabSWR[,c("Use", "waterUse")]
  sectorTabWellReg <- read.csv(paste0(stateDataDir,"AZ_WellReg_sectorMatch.csv"))
  colnames(sectorTabWellReg)[which(colnames(sectorTabWellReg)%in%"Sector")] <- "waterUse"
  colnames(sectorTabWellReg)[1] <- "Code"
  sectorTabWellReg <- sectorTabWellReg[,c("Code", "waterUse")]
  
  ##read in and organize the Arizona Well Registry data
  wellRDir <- paste0(stateDataDir, "wellRegistry/")
  azWellRegistry <- readOGR(dsn=wellRDir, layer="WellRegistry")
  azWellRWaterUse <- read.csv(paste0(wellRDir, "WELLS_WATER_USES.csv"))
  azWellRPermitA <- read.csv(paste0(wellRDir, "WELLS_PERMIT_WELL_ASSOCIATIONS.csv"))
  azWellRPermits <- read.csv(paste0(wellRDir, "WELLS_PERMITS.csv"))
  ##preemptively keep only the columns that are needed moving forward
  azWellRegistry <- azWellRegistry[,c("REGISTRY_I", "BASIN_NAME", "UTM_X_METE", "UTM_Y_METE")]
  azWellRWaterUse <- azWellRWaterUse[,c("WELL_REGISTRY_ID", "WUSE_CODE")]
  azWellRPermitA <- unique(azWellRPermitA[,c("WELL_REGISTRY_ID", "PT_PERMIT_NUMBER")])
  azWellRPermits <- unique(azWellRPermits[,c("PERMIT_NUMBER", "ACRE_FEET_ANNUM", "GALLONS_MINUTE", "BEGIN_DATE")])

  ##in the permit association table, remove IDs not in spatial data
  azWellRPermitA <- azWellRPermitA[azWellRPermitA$WELL_REGISTRY_ID%in%azWellRegistry$REGISTRY_I,]

  ##remove permits without dates
  datedOnly <-azWellRPermits[azWellRPermits$BEGIN_DATE!="",]
  datedOnly$BEGIN_DATE <- sapply(strsplit(datedOnly$BEGIN_DATE, " 0:"), "[[", 1)
  ##reformat priority date, to be in same format as Idaho year/month/day
  datedOnly$BEGIN_DATE <- paste(sapply(strsplit(datedOnly$BEGIN_DATE, "/"), "[[", 3), sapply(strsplit(datedOnly$BEGIN_DATE, "/"), "[[", 1), sapply(strsplit(datedOnly$BEGIN_DATE, "/"), "[[", 2), sep="/")
  ##first, remove those with no water measurement
  withWaterMeasure <- datedOnly[-which(is.na(datedOnly$ACRE_FEET_ANNUM)==T & is.na(datedOnly$GALLONS_MINUTE)==T),]
  ##convert flow to CFS
  withWaterMeasure$CFS <- conv_unit(withWaterMeasure$GALLONS_MINUTE, "gal_per_min", "ft3_per_sec")
  withWaterMeasure$CFS[is.na(withWaterMeasure$CFS)==T] <- acreFtYr2ft3Sec(withWaterMeasure$ACRE_FEET_ANNUM[is.na(withWaterMeasure$CFS)==T])
  withWaterMeasure <- withWaterMeasure[,c("PERMIT_NUMBER", "BEGIN_DATE", "CFS")]
  ##for multiple records of the same permit number, take the earliest date and greatest CFS
  singleWaterRecs <- lapply(unique(withWaterMeasure$PERMIT_NUMBER), function(x){idRecs<-withWaterMeasure[withWaterMeasure$PERMIT_NUMBER==x,];
                                                                                if(nrow(idRecs)==1){
                                                                                  return(idRecs)
                                                                                }else{
                                                                                  setFrame<-data.frame("PERMIT_NUMBER"=x, "BEGIN_DATE"=min(idRecs$BEGIN_DATE), "CFS"=max(idRecs$CFS));
                                                                                  return(setFrame)}})
  singleWaterRecs <- do.call(rbind.data.frame, singleWaterRecs)
  ##construct the water rights table
  addRegToPermit <- merge(x=unique(azWellRPermitA), y=unique(singleWaterRecs), by.x="PT_PERMIT_NUMBER", by.y="PERMIT_NUMBER")
  addRegToPermit <- unique(addRegToPermit[,c("WELL_REGISTRY_ID", "BEGIN_DATE", "CFS")])

  ##merging all of the well data into a single object, and then transforming into spatial data
  azWellRegAddAmnt <- merge(y=azWellRegistry, x=addRegToPermit, by.y="REGISTRY_I", by.x="WELL_REGISTRY_ID")
  azWellRegAddUse <- merge(x=azWellRegAddAmnt, y=azWellRWaterUse, by="WELL_REGISTRY_ID")
  azWellRegConvUse <- merge(x=azWellRegAddUse, y=sectorTabWellReg, by.x="WUSE_CODE", by.y="Code")
  azWellRegAddBasin <- merge(x=azWellRegConvUse, y=groundWBasins[,c("basinName", "basinNum")], by.x="BASIN_NAME", by.y="basinName")
  azWellData <- azWellRegAddBasin[,c("WUSE_CODE", "WELL_REGISTRY_ID", "basinNum", "CFS", "BEGIN_DATE", "waterUse")]
  names(azWellData) <- c("origWaterUse", "waterRightID", "grdBasinNum", "CFS", "priorityDate", "waterUse")
  azWellData$source <- "GROUND WATER"
  spatialWellData <- SpatialPointsDataFrame(coords=cbind(azWellRegAddBasin$UTM_X_METE,azWellRegAddBasin$UTM_Y_METE), azWellData, proj4string=CRS("+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs"))
  spatialWellData <- spTransform(spatialWellData, surfaceWSheds@proj4string@projargs)
  
  ##preform a spatial analysis to identify which WMA a record should belong to
  spatialWellData$surBasinNum <- NA
  for(ind in 1:length(surfaceWSheds)){
    ##identify the WMA and then isolate it in its own object
    isoPoly <- surfaceWSheds[ind,]
    ##R is not the most "out of box" efficient with processing spatial data. However, increased efficiency
    ##can be had with a bit of data frame manipulation. Specifically, when clipping a point cloud with a 
    ##polygon boundary, R will query each and every point to see if it falls within the polygon. However, 
    ##we can "prime" the checking process by removing points which we already know will not be within the 
    ##polygon. This is done by first sub-selecting those points which are located within the bounding 
    ##rectangle of the polygon. Then the clip is preformed to fit any unique shape the polygon boundary
    ##may have. While having two steps instead of one involves more coding, it is significantly faster
    ##for R to process.
    expPolyBnd <- raster::extent(isoPoly)+c(-0.2,0.2,-0.2,0.2) ##adding buffer to the polygon bounding rectangle. This is done to account for very strangly shaped polygons
    ##sub selecting a data frame by its table records is much faster in R than spatial functions
    subsetRecs <- spatialWellData[which(spatialWellData@coords[,1]>=expPolyBnd@xmin & spatialWellData@coords[,1]<=expPolyBnd@xmax & spatialWellData@coords[,2]>=expPolyBnd@ymin & spatialWellData@coords[,2]<=expPolyBnd@ymax),]
    
    ##checks to make sure that there are still records within the polygon bounding rectangle
    ##if there are, preform the spatial cropping
    if(nrow(subsetRecs)>0){
      innerRecs <- subsetRecs[isoPoly,]
      spatialWellData$surBasinNum[which(spatialWellData$waterRightID %in% innerRecs$waterRightID)] <- isoPoly$basinNum
    }
  }
  
  ##read in and organize the Arizona Surface Water claims
  azSWDir <- paste0(stateDataDir, "SWR/")
  azSurFillings <- readOGR(dsn=azSWDir, layer="SWR_fillings")  ##surface water rights
  ##remove NA uses
  azSurFillings <- azSurFillings[which(is.na(azSurFillings$USES)==F),]
  ##remove NA priority dates
  azSurFillings <- azSurFillings[which(is.na(azSurFillings$PRIOR_DATE)==F),]
  ##fill out the file number for those records in which it is missing for some reason
  missFileNo <- which(is.na(azSurFillings$FILE_NO)==T)
  azSurData <- azSurFillings@data
  azSurData$FILE_NO[missFileNo] <- paste0(azSurData$PROGRAM[missFileNo], "-", azSurData$APPNO[missFileNo])
  azSurSubCols <- azSurData[,c("FILE_NO", "PRIOR_DATE", "USES","WS_DESCR", "X_UTMNAD83", "Y_UTMNAD83")]
  azSurSubCols <- unique(azSurSubCols)
  
  ##organize the use column into multiple columns, to be able to subset by those values
  azSurUseCats <- c("STOCK", "IRRIGATION", "WILDLIFE", "DOMESTIC", "MINING", "MUNICIPAL", "POWER", "RECREATION", "INDUSTRIAL", "COMMERCIAL", "OTHER")
  azSurUnits <- c("AFA", "AFT", "AF", "GPA", "GAL", "CFS", "CFT", "MIT", "MIA", "MNT")
  ##add the WBM water uses to the Arizona Surface Water Data
  exUseData <- lapply(azSurSubCols$USES, exAZSurData, useCats=azSurUseCats, useUnits=azSurUnits)
  surUseTab <- do.call(rbind.data.frame, exUseData)
  surReaddUse <- cbind(azSurSubCols, surUseTab)
  ##remove USES column, as no longer needed
  surReaddUse <- surReaddUse[,-which(colnames(surReaddUse)%in%"USES")]
  ##remove records which were determined to be NA
  surReaddUse <- surReaddUse[is.na(surReaddUse$CFS)==F,]
  ##reformate priority date, to be in same format as Idaho year/month/day
  surReaddUse$PRIOR_DATE <- paste(sapply(strsplit(surReaddUse$PRIOR_DATE, "/"), "[[", 3), sapply(strsplit(surReaddUse$PRIOR_DATE, "/"), "[[", 1), sapply(strsplit(surReaddUse$PRIOR_DATE, "/"), "[[", 2), sep="/")
  
  ##due to an issue with the file extent, remakes the spatial data frame
  surReaddUse <- surReaddUse[which(surReaddUse$X_UTMNAD83>0 & surReaddUse$Y_UTMNAD83>0),]
  newFrame <- SpatialPointsDataFrame(coords=cbind(surReaddUse$X_UTMNAD83,surReaddUse$Y_UTMNAD83), data=surReaddUse, proj4string=azSurFillings@proj4string)
  projNewFrame <- spTransform(newFrame, surfaceWSheds@proj4string@projargs)
  
  ##for those records without a stated basin, determine basin by spatial overlay
  ##the full process is described above on line 593
  ##the expanded border is not 
  noBasinLocs <- which(is.na(projNewFrame$WS_DESCR)==T)
  noBasinTab <- projNewFrame[is.na(projNewFrame$WS_DESCR)==T,]
  for(polyInd in 1:length(surfaceWSheds)){
    poly <- surfaceWSheds[polyInd,]
    ptsInPoly <- noBasinTab[poly,]$FILE_NO
    projNewFrame$WS_DESCR[which(projNewFrame$FILE_NO %in% ptsInPoly)] <- poly$basinName
  }
  
  ##add the project sectors
  h2oSWRRights <- merge(x=projNewFrame, y=sectorTabSWR, by.x="origWaterUse", by.y="Use", sort=F)
  ##add the WMA numerical identifier to the table
  azShedTab <- data.frame(surBasinNum=surfaceWSheds$basinNum, basinName=surfaceWSheds$basinName)
  h2oSWRRights <- merge(x=h2oSWRRights, y=azShedTab, by.x="WS_DESCR", by.y="basinName", sort=F)
  ##remove columns that are no longer needed
  h2oSWRRights <- h2oSWRRights[,-which(names(h2oSWRRights) %in% c("X_UTMNAD83", "Y_UTMNAD83", "WS_DESCR"))]
  names(h2oSWRRights)[2:3] <- c(waterRightIDFieldName, "priorityDate")
  h2oSWRRights$source <- "SURFACE WATER"
  reorderSWR <- h2oSWRRights[,c("origWaterUse", "waterRightID", "surBasinNum", "CFS", "source", "priorityDate", "waterUse")]
  
  ##for those records without a stated basin, determine basin by spatial overlay
  ##the full process is described above on line 593
  reorderSWR$grdBasinNum <- NA
  for(ind in 1:length(groundWBasins)){
    isoPoly <- groundWBasins[ind,]
    expPolyBnd <- raster::extent(isoPoly)+c(-0.2,0.2,-0.2,0.2)
    subsetRecs <- reorderSWR[which(reorderSWR@coords[,1]>=expPolyBnd@xmin & reorderSWR@coords[,1]<=expPolyBnd@xmax & reorderSWR@coords[,2]>=expPolyBnd@ymin & reorderSWR@coords[,2]<=expPolyBnd@ymax),]
    
    if(nrow(subsetRecs)>0){
      innerRecs <- subsetRecs[isoPoly,]
      reorderSWR$grdBasinNum[which(reorderSWR$waterRightID %in% innerRecs$waterRightID)] <- isoPoly$basinNum
    }
  }
  

  ##read in and organize the Arizona Statement of Claimant data
  socDir <- paste0(stateDataDir, "Sept_2018/")
  azSOCMainFile <- read.csv(paste0(socDir, "SOC_MAIN.csv"))
  azSOCClaimUse <- read.csv(paste0(socDir, "SOC_CLAIMED_USES.csv"))
  azSOCSource <- read.csv(paste0(socDir, "SOC_SOURCE_OF_WATER.csv"))
  azSOCPrioDate <- read.csv(paste0(socDir, "SOC_CLAIMED_PRIORITY_DATE.csv"))
  azSOCAnnualAmt <- read.csv(paste0(socDir, "SOC_ANNUAL_AMOUNT_CLAIMED.csv"))
  azSOCCoords <- read.csv(paste0(socDir, "SOC_LOCATIONS.csv"))
  ##preemptivly keep only the columns that are needed moving forward
  azSOCMainFile <- azSOCMainFile[,c("ID", "FILE_NO", "WS")]
  azSOCClaimUse <- azSOCClaimUse[,c("MAIN_ID", "ID", "USE")]
  colnames(azSOCClaimUse)[2] <- "CU_ID" 
  azSOCSource <- azSOCSource[,c("CU_ID", "CODE")]
  azSOCPrioDate <- azSOCPrioDate[,c("CU_ID", "PRIORITY_MON", "PRIORITY_DAY", "PRIORITY_YEAR")]
  azSOCPrioDate <- azSOCPrioDate[which(is.na(azSOCPrioDate$PRIORITY_YEAR)==F),]
  ##adding first of year, or month, if month and day are missing. this is to keep records in as summarizing on year
  azSOCPrioDate$PRIORITY_MON[which(is.na(azSOCPrioDate$PRIORITY_MON)==T)] <- 1
  azSOCPrioDate$PRIORITY_DAY[which(is.na(azSOCPrioDate$PRIORITY_DAY)==T)] <- 1
  #azSOCPrioDate$priorityDate <- format(paste0(azSOCPrioDate$PRIORITY_YEAR, "/", azSOCPrioDate$PRIORITY_MON, "/", azSOCPrioDate$PRIORITY_DAY), format="%Y/%m/%d")
  azSOCPrioDate$priorityDate <- paste0(azSOCPrioDate$PRIORITY_YEAR, "/", azSOCPrioDate$PRIORITY_MON, "/", azSOCPrioDate$PRIORITY_DAY)
  ##check for valid data, as some dates in data set do not exist
  ##typically, these records are off by months not having a 31st day, but being declaired on the 31st
  ##as analyzing by year, setting those dates to the last day of those months
  dateValidity <- sapply(azSOCPrioDate$priorityDate, testDate)
  whichNulls <- which(sapply(dateValidity, is.null)==T)  
  azSOCPrioDate$priorityDate[whichNulls] <- gsub("/31", "/30", azSOCPrioDate$priorityDate[whichNulls])  ##assumes last day of months
  azSOCPrioDate$priorityDate[whichNulls] <- gsub("/29", "/28", azSOCPrioDate$priorityDate[whichNulls])  ##for 2/29, assumes last day of 2, likely not leap years
  azSOCPrioDate <- azSOCPrioDate[,c("CU_ID", "priorityDate")]
  ##convert flow to CFS
  azAmountFlow <- azSOCAnnualAmt[,c("CU_ID", "MAX_FLOW", "M_UNIT")]
  azAmountFlow <- azAmountFlow[which(is.na(azAmountFlow$MAX_FLOW)==F),]
  azAmountFlow <- azAmountFlow[which(is.na(azAmountFlow$M_UNIT)==F),]
  azAmountFlow <- azAmountFlow[azAmountFlow$M_UNIT!="",]
  azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="GPM"] <- conv_unit(azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="GPM"], "gal_per_min", "ft3_per_sec")
  azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="AMI"] <- azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="AMI"] * 0.025  ##Assumes that this is a per year measure
  azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="GPA"] <- conv_unit(azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="GPA"], "us_gal", "ft3") / 365.25
  azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="AFA"] <- acreFtYr2ft3Sec(azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="AFA"])
  azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="AF"] <- acreFtYr2ft3Sec(azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="AF"])  ##Assumes that AF is the same as AFA
  azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="MI"] <- azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="MI"] * 0.025  ##Assumes that MI is the same as AMI per year
  azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="GAL"] <- conv_unit(azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="GAL"], "us_gal", "ft3") / 365.25  ##Assumes that GAL is the same as GPA
  azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="GPD"] <- galDay2ft3Sec(azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="GPD"])
  azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="MIA"] <- azAmountFlow$MAX_FLOW[azAmountFlow$M_UNIT=="MIA"] * 0.025  ##Assumes that MIA is the same as AMI per year
  ##convert volume to CFS
  azAmountVol <- azSOCAnnualAmt[,c("CU_ID", "VOLUME", "V_UNIT", "M_UNIT")]
  azAmountVol <- azAmountVol[which(is.na(azAmountVol$VOLUME)==F),]
  azAmountVol <- azAmountVol[which(is.na(azAmountVol$V_UNIT)==F),]
  azAmountVol <- azAmountVol[azAmountVol$V_UNIT!="",]
  azAmountVol <- azAmountVol[azAmountVol$V_UNIT!="FT",]  ##Unsure how to interpret feet into a flow value
  azAmountVol <- azAmountVol[azAmountVol$M_UNIT=="",]
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="GPM"] <- conv_unit(azAmountVol$VOLUME[azAmountVol$V_UNIT=="GPM"], "gal_per_min", "ft3_per_sec")
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="AMI"] <- azAmountVol$VOLUME[azAmountVol$V_UNIT=="AMI"] * 0.025  ##Assumes that this is a per year measure
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="GPA"] <- conv_unit(azAmountVol$VOLUME[azAmountVol$V_UNIT=="GPA"], "us_gal", "ft3") / 365.25
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="AFA"] <- acreFtYr2ft3Sec(azAmountVol$VOLUME[azAmountVol$V_UNIT=="AFA"])
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="AF"] <- acreFtYr2ft3Sec(azAmountVol$VOLUME[azAmountVol$V_UNIT=="AF"])  ##Assumes that AF is the same as AFA
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="MI"] <- azAmountVol$VOLUME[azAmountVol$V_UNIT=="MI"] * 0.025  ##Assumes that MI is the same as AMI per year
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="GAL"] <- conv_unit(azAmountVol$VOLUME[azAmountVol$V_UNIT=="GAL"], "us_gal", "ft3") / 365.25  ##Assumes that GAL is the same as GPA
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="GPD"] <- galDay2ft3Sec(azAmountVol$VOLUME[azAmountVol$V_UNIT=="GPD"])
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="MIA"] <- azAmountVol$VOLUME[azAmountVol$V_UNIT=="MIA"] * 0.025  ##Assumes that MIA is the same as AMI per year
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="GMO"] <- azAmountVol$VOLUME[azAmountVol$V_UNIT=="GMO"] * 0.0000000508340599
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="GPW"] <- azAmountVol$VOLUME[azAmountVol$V_UNIT=="GPW"] * 0.0000002210327
  azAmountVol$VOLUME[azAmountVol$V_UNIT=="AFT"] <- acreFtYr2ft3Sec(azAmountVol$VOLUME[azAmountVol$V_UNIT=="AFT"])  ##Assumes that AFT is the same as AFA
  colnames(azAmountVol)[2] <- "MAX_FLOW"
  azSOCConvertAmt <- unique(rbind.data.frame(azAmountFlow[,c("CU_ID", "MAX_FLOW")], azAmountVol[,c("CU_ID", "MAX_FLOW")]))
  azSOCCoords <- azSOCCoords[,c("CU_ID", "UTM_X", "UTM_Y")]
  azSOCCoords <- azSOCCoords[which(is.na(azSOCCoords$UTM_X)==F),]
  azSOCCoords <- azSOCCoords[azSOCCoords$UTM_X!=0,]
  ##it was noticed that many of the CU_IDs have different coordinates
  ##average the coordinates to produce one set per CU_ID
  azSOCCoords <- merge(x=aggregate(data=azSOCCoords, UTM_X~CU_ID, FUN=mean), y=aggregate(data=azSOCCoords, UTM_Y~CU_ID, FUN=mean), by="CU_ID")
  
  ##construct the water rights table
  azAddClaim <- merge(x=azSOCMainFile, y=azSOCClaimUse, by.x="ID", by.y="MAIN_ID", all=F)
  azAddCFS <- merge(x=azAddClaim, y=azSOCConvertAmt, by="CU_ID")
  azAddSource <- merge(x=azAddCFS, y=azSOCSource, by="CU_ID")
  h2oSOCRights <- merge(x=azAddSource, y=azSOCPrioDate, by="CU_ID")
  ##preform some final data massaging to have values match other states
  h2oSOCRights$CODE[h2oSOCRights$CODE=="G"] <- "GROUND WATER"
  h2oSOCRights$CODE[h2oSOCRights$CODE!="GROUND WATER"] <- "SURFACE WATER"
  colnames(h2oSOCRights)[3:7] <- c(waterRightIDFieldName, basinIDFieldName, "origWaterUse", "CFS", "source")
  h2oSOCRights <- merge(x=h2oSOCRights, y=sectorTabSOC, by.x="origWaterUse", by.y="Code", sort=F)

  withGeo <- merge(x=h2oSOCRights, y=azSOCCoords, by="CU_ID")
  ##remove columns that are no longer needed
  h2oSOCRights <- h2oSOCRights[,-which(colnames(h2oSOCRights) %in% c("CU_ID", "ID"))]
  withGeo <- withGeo[,-which(colnames(withGeo) %in% c("CU_ID", "ID"))]
  spatialRights <- SpatialPointsDataFrame(withGeo[,c("UTM_X","UTM_Y")], data=withGeo[,-c(which(colnames(withGeo) %in% c("UTM_X","UTM_Y")))], proj4string=CRS("+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  spatialRights <- spTransform(spatialRights, projProj)
  
  names(spatialRights)[which(names(spatialRights)=="basinNum")] <- "surBasinNum"
  
  spatialRights$grdBasinNum <- NA
  
  
  
  fullSpRights <- rbind(spatialRights, reorderSWR, spatialWellData)
  fullSpRights <- fullSpRights[fullSpRights$CFS>=0,]
  fullRights <- fullSpRights@data
  
  ##would need to filter our records which would be the same from the disperate data sources.
  
  ##add the cleaned rights to the full body of spatial rights
  h2oDivByState[[whichState]] <- fullSpRights
  h2oUseByState[[whichState]] <- -9999
  
  wmaIDByState[[whichState]] <- as.character(unique(fullRights$surBasinNum))
  fullRightsRecs[[whichState]] <- fullRights
  ##split rights into surface and ground
  rightsByState_ground[[whichState]] <- fullRights[which(fullRights$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- fullRights[which(fullRights$source=="SURFACE WATER"),]
  
}
##nevada
if("nevada" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "nevada/")
  whichState <- which(states=="nevada")
  plottingDim[[whichState]] <- "tall"
  
  h2oDiv <- readOGR(paste0(stateDataDir, "POD_Sites"), layer="POD_Sites")
  #h2oUse <- readOGR(paste0(stateDataDir, "POD_Sites"), layer="POU_Sites")  ##h2oUse is commented out as contains no data not already in h2oDiv
  
  ##nevada watershed units
  spatialWMAByState[[whichState]] <- stateWMAs[stateWMAs$state=="Nevada",]
  wmaStateLabel[[whichState]] <- "Basin"
  
  ##water use categories.
  ##differ state to state
  sectorTab <- read.csv(paste0(stateDataDir,"NV_sectorMatch.csv"))
  colnames(sectorTab)[which(colnames(sectorTab)%in%"Sector")] <- "waterUse"
  sectorTab <- sectorTab[,c("mou", "waterUse")]
  
  ##subset the rights data to only keep the columns needed, and remove any without a valid id
  subH2oDiv <- h2oDiv[is.na(h2oDiv$app)==F,c("app", "div_balanc", "prior_dt", "mou", "source", "basin")]
  names(subH2oDiv) <- c(waterRightIDFieldName, "CFS", "priorityDate", "origWaterUse", "source", basinIDFieldName)
  ##remove NA priority dates
  subH2oDiv <- subH2oDiv[is.na(subH2oDiv$priorityDate)==F,]
  ##classify sources into ground or surface water
  subH2oDiv$source[subH2oDiv$source %in% c("UG", "OGW")] <- "GROUND WATER"
  subH2oDiv$source[subH2oDiv$source!="GROUND WATER"] <- "SURFACE WATER"
  
  ##project the rights into the project projection
  ##adding water use of the rights for later processing
  subH2oDiv <- merge(x=subH2oDiv, y=sectorTab, by.x="origWaterUse", by.y="mou", sort=F)
  projRights <- spTransform(subH2oDiv, projProj)
  h2oRights <- as.data.frame(projRights)
  h2oRights <- h2oRights[h2oRights$CFS>=0,]
  wmaIDByState[[whichState]] <- as.character(unique(projRights$basinNum))
  ##add the cleaned rights to the full body of spatial rights
  h2oDivByState[[whichState]] <- projRights
  h2oUseByState[[whichState]] <- -9999
  
  fullRightsRecs[[whichState]] <- h2oRights
  rightsByState_ground[[whichState]] <- h2oRights[which(h2oRights$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- h2oRights[which(h2oRights$source=="SURFACE WATER"),]
}
##utah
if("utah" %in% states){
  stateDataDir <- paste0(dataDir, "utah/")
  whichState <- which(states=="utah")
  plottingDim[[whichState]] <- "tall"
  
  h2oDiv <- readOGR(paste0(stateDataDir, "Utah_Points_of_Diversion"), layer="Utah_Points_of_Diversion")
  #h2oUse <- readOGR(paste0(stateDataDir, "Utah_Place_of_Use"), layer="Utah_Place_of_Use")  ##h2oUse does not have the data needed to be included with the points of diversion. Unlikely to be needed to be included
  ##subset the rights data to only keep the columns needed, and remove any without a valid id
  subH2oDiv  <- h2oDiv[,c("WRNUM", "TYPE", "PRIORITY", "USES", "CFS")]
  subH2oDiv <- subH2oDiv[is.na(subH2oDiv$PRIORITY)==F,]
  subH2oDiv <- subH2oDiv[is.na(subH2oDiv$USES)==F,]
  #subH2oDiv <- subH2oDiv[(is.na(subH2oDiv$PRIORITY)==F & is.na(h2oDiv$USES)==F),]
  names(subH2oDiv)[1:4] <- c(waterRightIDFieldName, "source", "priorityDate", "waterCode")
  
  ##for now remove all records that are Non Production, until get a solid answers from the aboves
  subH2oDiv <- subH2oDiv[grep("-", subH2oDiv$waterRightID),]
  subH2oDiv$basinNum <- sapply(strsplit(subH2oDiv$waterRightID, "-"), "[[", 1)
  
  ##to mirror Idaho, only keep the first water use, for now
  subH2oDiv$waterCode <- substring(subH2oDiv$waterCode, 1, 1)
  
  ##clean up priority dates
  holdPrioDate <- as.numeric(substring(subH2oDiv$priorityDate, 1, 4))
  ##correct the two records which have the year at the end of the date string
  subH2oDiv$priorityDate[which(holdPrioDate==709)] <- paste0(substring(subH2oDiv$priorityDate[which(holdPrioDate==709)], 5, 8), substring(subH2oDiv$priorityDate[which(holdPrioDate==709)], 1, 4))
  subH2oDiv$priorityDate[which(holdPrioDate==722)] <- paste0("19", substring(subH2oDiv$priorityDate[which(holdPrioDate==722)], 7, 8), substring(subH2oDiv$priorityDate[which(holdPrioDate==722)], 1, 4))
  ##remove 5 records which do not include a year
  subH2oDiv <- subH2oDiv[-c(which(holdPrioDate %in% c(1,7))),]
  ##reformate priority date, to be in same format as Idaho year/month/day
  subH2oDiv$priorityDate <- paste0(substring(subH2oDiv$priorityDate, 1, 4), "/", ifelse(nchar(subH2oDiv$priorityDate)<8, "01", substring(subH2oDiv$priorityDate, 5, 6)), "/", ifelse(nchar(subH2oDiv$priorityDate)<8, "01", substring(subH2oDiv$priorityDate, 7, 8)))
  
  ##check for valid data, as some dates in data set do not exist
  ##typically, these records are off by months not having a 31st day, but being declared on the 31st
  ##as analyzing by year, setting those dates to the last day of those months
  dateValidity <- sapply(subH2oDiv$priorityDate, testDate)
  whichNulls <- which(sapply(dateValidity, is.null)==T)  
  subH2oDiv$priorityDate[whichNulls] <- gsub("/00/00", "/01/01", subH2oDiv$priorityDate[whichNulls])  ##for 0 months and 0 days, turn into 01/01 for that year
  subH2oDiv$priorityDate[whichNulls] <- gsub("/00/", "/01/", subH2oDiv$priorityDate[whichNulls])  ##assumes those with 0 months are 01
  subH2oDiv$priorityDate[whichNulls] <- gsub("/  /", "/01/", subH2oDiv$priorityDate[whichNulls])  ##assumes those with blank months are 01
  subH2oDiv$priorityDate[whichNulls] <- gsub("02/29", "02/28", subH2oDiv$priorityDate[whichNulls])  ##for 2/29, assumes last day of 2, likely not leap years
  subH2oDiv$priorityDate[whichNulls] <- gsub("02/31", "02/28", subH2oDiv$priorityDate[whichNulls])  ##for 2/31, assumes last day of 2, likely not leap years
  subH2oDiv$priorityDate[whichNulls] <- gsub("/31", "/30", subH2oDiv$priorityDate[whichNulls])  ##assumes last day of months
  
  ##classify sources into ground or surface water
  subH2oDiv$source[subH2oDiv$source %in% c("Underground", "Abandonded Well")] <- "GROUND WATER"
  subH2oDiv$source[subH2oDiv$source!="GROUND WATER"] <- "SURFACE WATER"
  ##for rights with multiple uses, select the first use as the only use, as with the idaho data
  subH2oDiv$waterCode <- substr(subH2oDiv$waterCode, 1, 1)
  
  ##utah watershed units
  spatialWMAByState[[whichState]] <- stateWMAs[stateWMAs$state=="Utah",]
  wmaStateLabel[[whichState]] <- "Region"
  
  ##classifying water use categories into WBM sectors
  sectorTab <- read.csv(paste0(stateDataDir,"UT_sectorMatch.csv"))
  colnames(sectorTab)[which(colnames(sectorTab)%in%"Sector")] <- "waterUse"
  sectorTab <- sectorTab[,c("code", "waterUse")]
  
  ##project the rights into the project projection
  ##adding water use of the rights for later processing
  subH2oDiv <- merge(x=subH2oDiv, y=sectorTab, by.x="waterCode", by.y="code", sort=F)
  projRights <- spTransform(subH2oDiv, projProj)
  h2oRights <- as.data.frame(projRights)
  h2oRights <- h2oRights[h2oRights$CFS>=0,]
  wmaIDByState[[whichState]] <- as.character(unique(projRights$basinNum))
  ##add the cleaned rights to the full body of spatial rights
  h2oDivByState[[whichState]] <- projRights
  #h2oUseByState[[whichState]] <- NULL
  h2oUseByState[[whichState]] <- -9999
  
  fullRightsRecs[[whichState]] <- h2oRights
  rightsByState_ground[[whichState]] <- h2oRights[which(h2oRights$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- h2oRights[which(h2oRights$source=="SURFACE WATER"),]
}
##New Mexico
if("newMexico" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "newmexico/")
  whichState <- which(states=="newMexico")
  plottingDim[[whichState]] <- "tall"
  
  h2oDiv <- readOGR(paste0(stateDataDir, "OSE_Points_of_Diversion"), layer="OSE_Points_of_Diversion")
  
  ##new mexico watershed units
  spatialWMAByState[[whichState]] <- stateWMAs[stateWMAs$state=="NewMexico",]
  wmaStateLabel[[whichState]] <- "Basin"
  
  ##classifying water use categories into WBM sectors
  sectorTab <- read.csv(paste0(stateDataDir,"NM_sectorMatch.csv"))
  colnames(sectorTab)[which(colnames(sectorTab)%in%c("Code.Value","Sector"))] <- c("code", "waterUse")
  sectorTab <- sectorTab[,c("code", "waterUse")]
  
  ##subset the rights data to only keep the columns needed, and remove any without a valid id
  subH2oDiv <- h2oDiv[,c("pod_nbr", "pod_basin", "total_div", "use", "surface_co", "grnd_wtr_s", "start_date", "finish_dat")]
  ##adds some possible dates, for those with start dates but not finish dates
  subH2oDiv$finish_dat[is.na(subH2oDiv$finish_dat)==T] <- subH2oDiv$start_date[is.na(subH2oDiv$finish_dat)==T]
  subH2oDiv$source <- "SURFACE WATER"
  subH2oDiv$source[is.na(subH2oDiv$grnd_wtr_s)==F] <- "GROUND WATER"
  subH2oDiv <- subH2oDiv[is.na(subH2oDiv$finish_dat)==F, -c(which(names(subH2oDiv) %in% c("surface_co", "grnd_wtr_s", "start_date")))]
  names(subH2oDiv) <- c(waterRightIDFieldName, basinIDFieldName, "CFS", "origWaterUse", "priorityDate", "source")
  ##combines the basin and water number in order to keep as many records as possible, other wise more duplicates thrown out
  subH2oDiv$waterRightID <- paste(subH2oDiv$basinNum, subH2oDiv$waterRightID, sep="-")
  ##reformat priority date, to be in same format as Idaho year/month/day
  subH2oDiv$priorityDate <- gsub("-", "/", sapply(strsplit(subH2oDiv$priorityDate, "T"), "[[", 1))
  ##convert data AFA into CFS
  subH2oDiv$CFS <- acreFtYr2ft3Sec(subH2oDiv$CFS)
  ##adding water use of the rights for later processing
  subH2oDiv <- merge(x=subH2oDiv, y=sectorTab, by.x="origWaterUse", by.y="code", sort=F)
  
  ##project the rights into the project projection
  projRights <- spTransform(subH2oDiv, projProj)
  h2oRights <- as.data.frame(projRights)
  h2oRights <- h2oRights[h2oRights$CFS>=0,]
  wmaIDByState[[whichState]] <- as.character(unique(projRights$basinNum))
  ##add the cleaned rights to the full body of spatial rights
  h2oDivByState[[whichState]] <- projRights
  h2oUseByState[[whichState]] <- -9999
  
  fullRightsRecs[[whichState]] <- h2oRights
  rightsByState_ground[[whichState]] <- h2oRights[which(h2oRights$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- h2oRights[which(h2oRights$source=="SURFACE WATER"),]
}
##Montana
if("montana" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "montana/")
  whichState <- which(states=="montana")
  plottingDim[[whichState]] <- "wide"
  
  h2oDiv <- readOGR(paste0(stateDataDir, "MTWaterRights.gdb"), layer="WRDIV")
  h2oUse <- readOGR(paste0(stateDataDir, "MTWaterRights.gdb"), layer="WRPOU")
  
  ##montana watershed units
  spatialWMAByState[[whichState]] <- stateWMAs[stateWMAs$state=="Montana",]
  wmaStateLabel[[whichState]] <- "Adjudication Basin"
  
  ##classifying water use categories into WBM sectors
  sectorTab <- read.csv(paste0(stateDataDir,"MT_sectorMatch.csv"))
  colnames(sectorTab)[which(colnames(sectorTab)%in%"Sector")] <- "waterUse"
  colnames(sectorTab)[1] <- "Purpose"
  sectorTab <- sectorTab[,c("Purpose", "waterUse")]
  
  ##subset the rights data to only keep the columns needed, and remove any without a valid id
  subH2oDiv <- h2oDiv[,c("WRNUMBER","ENFPRIDATE", "SRCTYPE")]
  subH2oUse <- h2oUse[,c("WRNUMBER", "PURPOSE", "FLWRTCFS", "FLWRTGPM")]
  names(subH2oDiv) <- c(waterRightIDFieldName, "priorityDate", "source")
  names(subH2oUse) <- c(waterRightIDFieldName, "origWaterUse", "CFS", "GPM")
  subH2oUse <- unique(as.data.frame(subH2oUse))
  ##clean up some of the use records
  subH2oUse <- subH2oUse[is.na(subH2oUse$origWaterUse)==F,]
  subH2oUse <- subH2oUse[(is.na(subH2oUse$CFS)==F | is.na(subH2oUse$GPM)==F),]
  ##for now, for those records with multiple uses, only use the first
  subH2oUse <- filter(subH2oUse, duplicated(subH2oUse$waterRightID)==F)
  ##if CFS is not available, convert GPM to CFS
  gpmAvail <- which(is.na(subH2oUse$CFS)==T & is.na(subH2oUse$GPM)==F)
  subH2oUse$CFS[gpmAvail] <- conv_unit(subH2oUse$GPM[gpmAvail], "gal_per_min", "ft3_per_sec")
  subH2oUse <- subH2oUse[,-which(names(subH2oUse)=="GPM")]
  
  ##clean up the div records
  subH2oDiv <- subH2oDiv[is.na(subH2oDiv$priorityDate)==F,]
  subH2oDiv$priorityDate <- gsub("-", "/", subH2oDiv$priorityDate, "[[", 1)
  subH2oDiv <- subH2oDiv[is.na(subH2oDiv$source)==F,]
  subH2oDiv$source[subH2oDiv$source=="GROUNDWATER"] <- "GROUND WATER"
  subH2oDiv$source[subH2oDiv$source!="GROUND WATER"] <- "SURFACE WATER"
  
  ##merge the div and use data, only keeping common
  h2oRights <- merge(x=subH2oDiv, y=subH2oUse, by=waterRightIDFieldName, all=F)
  ##adding water use of the rights for later processing
  h2oRights <- merge(x=h2oRights, y=sectorTab, by.x="origWaterUse", by.y="Purpose", sort=F)
  ##project the rights into the project projection
  projRights <- spTransform(h2oRights, projProj)
  projRights$basinNum <- sapply(strsplit(projRights$waterRightID, " "), "[[", 1)
  fullRights <- as.data.frame(projRights)
  fullRights <- fullRights[fullRights$CFS>=0,]
  wmaIDByState[[whichState]] <- as.character(unique(projRights$basinNum))
  ##add the cleaned rights to the full body of spatial rights
  h2oDivByState[[whichState]] <- projRights
  h2oUseByState[[whichState]] <- -9999
  
  fullRightsRecs[[whichState]] <- fullRights
  rightsByState_ground[[whichState]] <- fullRights[which(fullRights$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- fullRights[which(fullRights$source=="SURFACE WATER"),]
}
##Wyoming
if("wyoming" %in% states){
  ##set state specific directories
  stateDataDir <- paste0(dataDir, "wyoming/")
  whichState <- which(states=="wyoming")
  plottingDim[[whichState]] <- "wide"
  
  ##read in the water rights data
  rightsFiles <- list.files(stateDataDir, "All_POD_WaterRight_Search_Results.csv", full.names=T, recursive=T)
  h2oDivs <- pblapply(rightsFiles, function(nam){tab<-read.csv(nam);
                                                subTab<-tab[,c("WR.Number", "Total.Flow.CFS...Appropriation.GPM.", "PriorityDate", "Uses", "Facility.type", "Longitude", "Latitude")];
                                                splitFile <- strsplit(nam, "/")[[1]]
                                                subTab$basinNum <- paste(gsub("div", "",  splitFile[grep("div", splitFile)]), gsub("dist", "",  splitFile[grep("dist", splitFile)]), sep="_")
                                                return(subTab)})
  h2oDivs <- do.call(rbind.data.frame, h2oDivs)
  h2oDivs$basinNum[h2oDivs$basinNum=="1_15"] <- "1_15-5"
  
  ##cleans the water right data
  colnames(h2oDivs)[1:5] <- c(waterRightIDFieldName, "CFS", "priorityDate", "origWaterUse", "source")
  ##first, remove records without valid pieces of data
  h2oDivs <- h2oDivs[which(is.na(h2oDivs$CFS)==F | h2oDivs$CFS==""),]
  ##convert the ground water records from GPM to CFS
  h2oDivs$CFS[which(h2oDivs$source=="Well")] <- conv_unit(h2oDivs$CFS[which(h2oDivs$source=="Well")], "gal_per_min", "ft3_per_sec")
  ##remove records with negative CFS, these are most commonly abandoned wells
  h2oDivs <- h2oDivs[h2oDivs$CFS>=0,]
  
  h2oDivs <- h2oDivs[h2oDivs$priorityDate!="",]
  h2oDivs <- h2oDivs[h2oDivs$origWaterUse!="",]
  ##reformate priority date, to be in same format as Idaho year/month/day
  h2oDivs$priorityDate <- paste0(sapply(strsplit(h2oDivs$priorityDate, "/"), "[[", 3), "/", sapply(strsplit(h2oDivs$priorityDate, "/"), "[[", 1), "/", sapply(strsplit(h2oDivs$priorityDate, "/"), "[[", 2))
  ##for records with multiple water uses, select the first use
  h2oDivs$origWaterUse <- sapply(strsplit(h2oDivs$origWaterUse, ";"), "[[", 1)
  
  ##classify sources into ground or surface water
  h2oDivs$source[h2oDivs$source=="Well"] <- "GROUND WATER"
  h2oDivs$source[h2oDivs$source!="GROUND WATER"] <- "SURFACE WATER"
  
  ##remove duplicated rights, biasing the first records as with Idaho
  h2oRights <- filter(h2oDivs, duplicated(h2oDivs$waterRightID)==F)
  
  ##wyoming watershed units
  spatialWMAByState[[whichState]] <- stateWMAs[stateWMAs$state=="Wyoming",]
  wmaStateLabel[[whichState]] <- "Water Districts"
  
  ##classifying water use categories into WBM sectors
  sectorTab <- read.csv(paste0(stateDataDir,"WY_sectorMatch.csv"))
  colnames(sectorTab)[which(colnames(sectorTab)%in%"Sector")] <- "waterUse"
  colnames(sectorTab)[1] <- "Code"
  sectorTab <- sectorTab[,c("Code", "waterUse")]
  
  ##adding water use of the rights for later processing
  h2oRights$origWaterUse <- str_trim(h2oRights$origWaterUse)
  h2oRights <- unique(merge(x=h2oRights, y=sectorTab, by.x="origWaterUse", by.y="Code", sort=F))
  h2oRights <- h2oRights[h2oRights$CFS>=0,]
  
  wmaIDByState[[whichState]] <- as.character(unique(h2oRights$basinNum))
  ##add the cleaned rights to the full body of spatial rights
  spRights <- h2oRights[-c(which(is.na(h2oRights$Longitude)==T | is.na(h2oRights$Latitude)==T)),]
  h2oDivByState[[whichState]] <- SpatialPointsDataFrame(spRights[,c("Longitude","Latitude")], data=spRights[,-c(which(colnames(spRights) %in% c("Latitude", "Longitude")))], proj4string=projProj)
  h2oUseByState[[whichState]] <- -9999
  
  fullRightsRecs[[whichState]] <- h2oRights
  rightsByState_ground[[whichState]] <- h2oRights[which(h2oRights$source=="GROUND WATER"),]
  rightsByState_surface[[whichState]] <- h2oRights[which(h2oRights$source=="SURFACE WATER"),]
}

##saving the created output objects to disk
save(file=paste0(outDir, "reorganizedData/stateWaterRightsHarmonized.RData"), "rightsByState_ground", "rightsByState_surface", "projProj", "wmaStateLabel", 
     "h2oUseByState", "h2oDivByState", "spatialWMAByState", "wmaIDByState", "fullRightsRecs", "states", "plottingDim")


