##########################################################################
##########################################################################
## Script Name: dataPrepCustomFunctions.R
## Purpose of Script: A file which holds the custom function written 
## specifically to assist with reading in and formatting water rights
## data from the 11 states. This file is called by almost all of the 
## other script files for one function or another.
##
## Special Requirements: None - this is meant to be called by the 
## dataPrep.R script.
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 5/23/2023
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 10/2/2023
##
## Copyright (c) 2023 The Pennsylvania State University
##
##########################################################################
##########################################################################
testDate <- function(inDate){
  #################
  #inDate <- jjj[1960]
  #################
  
  return(tryCatch(year(inDate), error=function(e) NULL))
}
##########################################################################
##########################################################################
##A function to format the downloaded water rights from California. The downloaded
##files were multisheet MS Excel files. This function extracts portions from
##several sheets, and merges them into a single data frame. 
##
##The 'byUse' argument is not currently used. There were originally plans to add
##filtering by water right water use to this function, but this was not implemented
readInCaliPODRecs <- function(caliFile, byUse=T){
  #################
  #caliFile <- filesByUse[1]. ##the name of the file on disk, path may be included
  #byUse <- T.                ##not used
  #################
  
  ##read in the 4 sheets of the Excel file
  sheets <- 1:4 ##1-WaterRights basic information, 2-ApplicationInfo, 3-PointsOfDiversion, 4-BeneficialUses 
  fileSheets <- lapply(sheets, function(sh){read.xls(caliFile, sheet=sh)})
  
  ##extract the base of each record
  recordsBase <- fileSheets[[3]][,c("Application.Number", "POD.ID", "Latitude", "Longitude", "POD.Direct.Diversion.Rate", "DD.Unit", "Source", "Watershed")]
  ##merge the record base with the priority date
  getDate <- merge(x=recordsBase, y=fileSheets[[1]][,c("Application.Number", "Status.Date")], by="Application.Number")
  ##merge the record data with the use of the water right
  finishTouch <- merge(x=getDate, y=fileSheets[[4]][,c("Application.Number", "Beneficial.Use")], by="Application.Number")
  
  return(finishTouch)
}
##########################################################################
##########################################################################
##reorganizes the California records, and converts all units to CFS
reorgCaliRecs <- function(countyRecs, useRecs, entityRecs, wShedRecs){
  #################
  #countyRecs <- h2oDivCountyRecs[h2oDivCountyRecs$waterUse==wUse,]
  #useRecs <- h2oDivUseRecs[h2oDivUseRecs$waterUse==wUse,]
  #entityRecs <- h2oDivEntityRecs[h2oDivEntityRecs$waterUse==wUse,]
  #wShedRecs <- h2oDivWatershedRecs[h2oDivWatershedRecs$waterUse==wUse,]
  #################
  ##objects used to measure the number of records removed for various reasons
  nrecCounty <- nrow(countyRecs)
  nrecUse <- nrow(useRecs)
  nrecEnt <- nrow(entityRecs)
  nrecWsd <- nrow(wShedRecs)
  ##objects which hold valid values for various fields
  validCFSUnits <- c("Acre-feet", "Acre-feet per Year", "Cubic Feet per Second", "	Gallons", "Gallons per Day", "Gallons per Minute")
  ##keep only those records with a valid unit type, as unable to properly convert
  valCntCFSUnit <- countyRecs[countyRecs$CFS_units %in% validCFSUnits,]
  valUseCFSUnit <- useRecs[useRecs$CFS_units %in% validCFSUnits,]
  valEntCFSUnit <- entityRecs[entityRecs$CFS_units %in% validCFSUnits,]
  valWsdCFSUnit <- wShedRecs[wShedRecs$CFS_units %in% validCFSUnits,]
  
  upNRCounty <- nrecCounty - nrow(valCntCFSUnit)
  upNRUse <- nrecUse - nrow(valUseCFSUnit)
  upNREnt <- nrecEnt - nrow(valEntCFSUnit)
  upNRWsd <- nrecWsd - nrow(valWsdCFSUnit)
  print(paste("Invalid Units:", sum(c(upNRCounty, upNRUse, upNREnt, upNRWsd)), sep=" "))
  
  ##combine records by county and records by water use
  commonRecs1 <- valCntCFSUnit[valCntCFSUnit$waterRightID %in% valUseCFSUnit$waterRightID,]
  notInUse <- valCntCFSUnit[!(valCntCFSUnit$waterRightID %in% valUseCFSUnit$waterRightID),]
  notInCounty <- valUseCFSUnit[!(valUseCFSUnit$waterRightID %in% valCntCFSUnit$waterRightID),]
  combineRecs1 <- rbind.data.frame(commonRecs1, notInUse, notInCounty)
  
  ##combine the records from the previous combination and records by entity
  notInCombine1 <- valEntCFSUnit[!(valEntCFSUnit$waterRightID %in% combineRecs1$waterRightID),]
  combineRecs2 <- rbind.data.frame(combineRecs1, notInCombine1)
  
  ##combine the records from the previous combination and records by watersheds
  notInCombine2 <- valWsdCFSUnit[!(valWsdCFSUnit$waterRightID %in% combineRecs2$waterRightID),]
  fullyCombinedRecs <- rbind.data.frame(combineRecs2, notInCombine2)
  nrowFCRecs <- nrow(fullyCombinedRecs)
  
  ##remove records without a stated basin
  fullyCombinedRecs <- fullyCombinedRecs[is.na(fullyCombinedRecs$basinName)==F,]
  fullyCombinedRecs <- fullyCombinedRecs[fullyCombinedRecs$basinName!="",]
  print(paste("Invalid Basin:", nrowFCRecs - nrow(fullyCombinedRecs), sep=" "))
  nrowFCRecs <- nrow(fullyCombinedRecs)
  ##remove records without a stated source
  fullyCombinedRecs <- fullyCombinedRecs[is.na(fullyCombinedRecs$source)==F,]
  print(paste("Invalid Source:", nrowFCRecs - nrow(fullyCombinedRecs), sep=" "))
  nrowFCRecs <- nrow(fullyCombinedRecs)
  ##remove records without a priority date
  fullyCombinedRecs <- fullyCombinedRecs[is.na(fullyCombinedRecs$priorityDate)==F,]
  fullyCombinedRecs <- fullyCombinedRecs[fullyCombinedRecs$priorityDate!="",]
  print(paste("Invalid Priority Date:", nrowFCRecs - nrow(fullyCombinedRecs), sep=" "))
  nrowFCRecs <- nrow(fullyCombinedRecs)
  
  ##convert all flow values to CFS
  fullyCombinedRecs$CFS <- as.numeric(fullyCombinedRecs$CFS)
  cfsUnits <- unique(fullyCombinedRecs$CFS_units)
  if(length(cfsUnits)>1 & cfsUnits[1]!="Cubic Feet per Second"){
    if("Acre-feet per Year" %in% cfsUnits){
      afyIndex <- which(fullyCombinedRecs$CFS_units=="Acre-feet per Year")
      fullyCombinedRecs$CFS[afyIndex] <- acreFtYr2ft3Sec(fullyCombinedRecs$CFS[afyIndex])
    }
    if("Gallons per Day" %in% cfsUnits){
      gpdIndex <- which(fullyCombinedRecs$CFS_units=="Gallons per Day")
      fullyCombinedRecs$CFS[gpdIndex] <- galDay2ft3Sec(fullyCombinedRecs$CFS[gpdIndex])
    }
    if("Gallons per Minute" %in% cfsUnits){
      gpmIndex <- which(fullyCombinedRecs$CFS_units=="Gallons per Minute")
      fullyCombinedRecs$CFS[gpmIndex] <- conv_unit(fullyCombinedRecs$CFS[gpmIndex], "gal_per_min", "ft3_per_sec")
    }
    if("Gallons" %in% cfsUnits){  ##from examining a few documents, assume per year
      gIndex <- which(fullyCombinedRecs$CFS_units=="Gallons")
      fullyCombinedRecs$CFS[gIndex] <- conv_unit(fullyCombinedRecs$CFS[gIndex], "us_gal", "ft3") / 365.25
    }
    if("Acre-feet" %in% cfsUnits){  ##from examining a few documents, assume per year
      afIndex <- which(fullyCombinedRecs$CFS_units=="Acre-feet")
      fullyCombinedRecs$CFS[afIndex] <- acreFtYr2ft3Sec(fullyCombinedRecs$CFS[afIndex])
    }
  }
  
  ##reclassify sources
  gwInd <- lapply(c("GROUNDWATER", "Groundwater", "GROUND WATER"), grep, x=fullyCombinedRecs$source)
  fullyCombinedRecs$source <- "SURFACE WATER"
  fullyCombinedRecs$source[unlist(gwInd)] <- "GROUND WATER"
  
  ##reformat date colume, to be in same format as Idaho year/month/day
  fullyCombinedRecs$priorityDate <- format(as.POSIXct(fullyCombinedRecs$priorityDate, format="%m/%d/%Y"), format="%Y/%m/%d")
  
  ##remove columns no longer needed
  fullyCombinedRecs <- fullyCombinedRecs[,-c(which(colnames(fullyCombinedRecs)=="CFS_units"))]
  
  ##make sure the coordinates are numeric
  fullyCombinedRecs$latitude <- as.numeric(fullyCombinedRecs$latitude)
  fullyCombinedRecs$longitude <- as.numeric(fullyCombinedRecs$longitude)
  
  return(fullyCombinedRecs)
}
##########################################################################
##########################################################################
##A function to convert gallons per day to cubic feet per second
galDay2ft3Sec <- function(val){
  #################
  #val <- 1
  #################
  ft3Sec <- val * 0.00000154723
  return(ft3Sec)
}
##########################################################################
##########################################################################
##A function to convert acre feet per year to cubic feet per second
acreFtYr2ft3Sec <- function(val){
  #################
  #val <- 1
  #################
  ft3Sec <- val * 0.00138036
  return(ft3Sec)
}
##########################################################################
##########################################################################
##Extract the water use and calculate CSF for each record in the surface 
##water dataset. 
exAZSurData <- function(useRec, useCats, useUnits){
  ###############
  #useRec <- azSurFillings$USES[2]  ##The record to gather information for
  #useCats <- azSurUseCats          ##the AZ water use classification table
  #useUnits <- azSurUnits           ##the character of the units for the given record
  ###############
  ##if COWS / HORSES is available, convert to STOCK
  if(grepl("COWS / HORSES", useRec)){
    useRec <- gsub("COWS / HORSES", "COWS", useRec)
    useCats <- c(useCats, "COWS")
  }
  
  ##determine which water uses are available
  splitRec <- strsplit(useRec, " ")[[1]]
  catsInRec <- names(sort(unlist(sapply(useCats, grep, x=splitRec))))
  
  ##get the unit used in the record
  unitPos <- which(splitRec %in% useUnits)
  if(length(unitPos)==0){
    ##if no units, no water use
    setOutput <- data.frame(origWaterUse=NA, CFS=NA)
    return(setOutput)
  }
  
  h2oUse <- splitRec[unitPos-1]
  if(is.null(catsInRec)==T & length(h2oUse)>=1){
    catsInRec <- "OTHER"
  }else if(grepl("###############", useRec)){
    ##remove hashtag amounts
    setOutput <- data.frame(origWaterUse=NA, CFS=NA)
    return(setOutput)
  }
  numH2OUse <- as.numeric(gsub(",", "", h2oUse))
  
  if(length(numH2OUse)>length(catsInRec)){
    numH2OUse <- numH2OUse[1:length(catsInRec)]
    unitPos <- unitPos[1:length(catsInRec)]
  }
  
  ##convert water use to CFS
  ##Not all of the volume/flow units could be found. Therefore, several
  ##and educated guess has been assumed for many of the unit acronyms.
  ##AF - known as Acre Feet. Assumed to be Acre Feet per Year
  ##AFA - Known as Acre Feet per Annum. 
  ##AFT - Assumed to be Acre Feet Total, or Acre Feet per Year
  ##GPA - known as Gallons per Acre. Assumed to be Gallons per Acre per Year
  ##GAL - Assumed to be Gallons Total. Assumed to be per Year
  ##CFS - Known to be Cubic Feet per Second
  ##CFT - Assumed to be Cubit Foot Total, assumed annually
  ##MIT -
  ##MIA - 
  ##MNT - An unknown flow unit. However, all values associated with this 
  ##      unit are 0.
  ##AMI - 
  unitText <- splitRec[unitPos]
  outCFS <- sapply(1:length(unitText), function(n){utxt<-unitText[n];
                                                    if(utxt=="AFA"){
                                                      return(acreFtYr2ft3Sec(numH2OUse[n]))
                                                    }else if(utxt=="AFT"){
                                                      return(acreFtYr2ft3Sec(numH2OUse[n]))
                                                    }else if(utxt=="AF"){
                                                      return(acreFtYr2ft3Sec(numH2OUse[n]))
                                                    }else if(utxt=="GPA"){
                                                      return(conv_unit(numH2OUse[n], "us_gal", "ft3") / 365.25)
                                                    }else if(utxt=="GAL"){
                                                      return(conv_unit(numH2OUse[n], "us_gal", "ft3") / 365.25)  
                                                    }else if(utxt=="CFS"){
                                                      return(numH2OUse[n])
                                                    }else if(utxt=="CFT"){
                                                      return(numH2OUse[n] / 31556952)
                                                    }else if(utxt=="MIT"){  ##Assumes that MIT is the same as AMI per year
                                                      return(numH2OUse[n] * 0.025)
                                                    }else if(utxt=="MIA"){  ##Assumes that MIA is the same as AMI per year
                                                      return(numH2OUse[n] * 0.025)
                                                    }else if(utxt=="MNT"){
                                                      return(0)  
                                                    }})
  ##if needed, transform cows into stock
  if("COWS" %in% catsInRec){
    if("STOCK" %in% catsInRec ){
      whichCOWS <- grep("COWS",catsInRec)
      if(length(catsInRec)==length(outCFS)){
        outCFS[grep("STOCK",catsInRec)] <- sum(c(outCFS[grep("STOCK",catsInRec)], outCFS[whichCOWS]))
        ##remove cows, as no longer needed
        catsInRec <- catsInRec[-whichCOWS]
        outCFS <- outCFS[-whichCOWS]
      }else{
        ##remove cows, as no longer needed
        catsInRec <- catsInRec[-whichCOWS]
      }
    }else{
      ##convert cows to stock
      catsInRec[grep("COWS",catsInRec)] <- "STOCK"
    }
  }
  
  ##set up the output based on the number of input CFS values
  ##Assigns all water use to the use category with the most use. If its not broken down
  ##to different water amounts, assumes that the first category
  setOutput <- data.frame(origWaterUse=catsInRec[which.max(outCFS)], CFS=sum(outCFS))
  return(setOutput)
}
##########################################################################
##########################################################################