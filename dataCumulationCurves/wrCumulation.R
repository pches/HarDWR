##########################################################################
##########################################################################
## Script Name: WMAAgg.R
## Purpose of Script: Reads in the formatted water rights and spatial data from
## the above two scripts, and manages the cumulation calculations (lines 1-61).
## Lines 66-94 write out the tables required by WBM, summaries of the 
## cumulation calculations. This script also manages the creation of various
## images. These images include maps of the water rights points of diversion
## and points of use and the cumulation curve graphs (total and by sector; 
## lines 102-145). The lines after 145 are a large collection of testing code
## that was not used in the final analysis.
##
## Special Requirements: formatted water rights; spatial WMAs
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 11/13/2019
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 6/9/2023
##
## Copyright (c) 2021 The Pennsylvania State University
##
##########################################################################
##########################################################################
##a script to accumulate the water use rights for the Western states by Water Management Area (WMA), by year, and by classified water use sector.
options(stringsAsFactors=F)

##setting the paths of various directories
##local machine
projCodeDir <- "/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/"
projBoxDir <- "/Users/mdl5548/Library/CloudStorage/GoogleDrive-mdl5548@psu.edu/Shared drives/PCHES_Project1.2/Water rights project/Water institutions/Data/waterRightsCumulations/"
##parallels machine
#projCodeDir <- "/media/psf/Home/Documents/GitHub/waterRightsCumulationCurves/"
#projBoxDir <- "/media/psf/Home/Library/CloudStorage/GoogleDrive-mdl5548@psu.edu/Shared drives/PCHES_Project1.2/Water rights project/Water institutions/Data/waterRightsCumulations/"
dataDir <- paste0(projBoxDir, "output/")

##load libraries
library(rgeos)
library(rgdal)
library(maptools)
library(knitr)
library(lubridate)
library(dplyr)
library(tidyr)
library(BBmisc)
library(zoo)
library(pbapply)
library(reshape2)
library(ggplot2)
library(ggfortify)
library(ggthemes)
library(parallel)

##source custom functions
source(paste0(projCodeDir, "wrCumulation_CustomFunctions.R"))

##load cleaned water rights data
##to be used to cumulate the water flows by WMAs
reorgWaterRights <- list.files(paste0(dataDir, "reorganizedData/"), "stateWaterRightsHarmonized.RData", recursive=T, full.names=T)
load(reorgWaterRights)

##check for the oldest and most recent priority years
##This is used to determine the beginning and ending years for the cumulation
##first, set up some empty objects to recieve data data
chkMinSur <- c()
chkMaxSur <- c()
chkMinGrd <- c()
chkMaxGrd <- c()
##second, cycle though each state and collect the minimum and maximum priority years
for(st in 1:length(states)){
  ##extract the minimum and maximum priority years for each year
  stSurYears <- as.numeric(sapply(strsplit(rightsByState_surface[[st]]$priorityDate, "/"), "[[", 1))
  stGrdYears <- as.numeric(sapply(strsplit(rightsByState_ground[[st]]$priorityDate, "/"), "[[", 1))
  ##place the minimum and maximum year for each state in a vector
  chkMinSur[st] <- min(stSurYears)
  chkMaxSur[st] <- max(stSurYears)
  chkMinGrd[st] <- min(stGrdYears)
  chkMaxGrd[st] <- max(stGrdYears)
}
##The allYrMin and allYrMax variables define which years the cumulation will be calculated for
##In the creation of the cumulative curves for our water market analysis, we allowed the data to 
##set the minimum bounding year and set the maximum year at 2100. However, it is also an option
##to manually set either variable, or to have the data determine what the values would be.
allYrMin <- min(c(chkMinSur, chkMinGrd)). ##the minimum year is 217, from a water right in Arizona
#allYrMax <- max(c(chkMaxSur, chkMaxGrd))
allYrMax <- 2100 

##proceed with creating the state cumulative curves - all states
##That output is a list with two items per WMA per state. The first is the cumulative 
##curves with CFS in the individual sector columns. The second is the cumulative curves
##with the percent of total CFS each sector represents for the given year.
wmasCalcGrd <- mapply(calcWMASectorVars, rights=rightsByState_ground, state=states, wmaID=wmaIDByState, startDate=allYrMin, endDate=allYrMax, SIMPLIFY=F)
wmasCalcSur <- mapply(calcWMASectorVars, rights=rightsByState_surface, state=states, wmaID=wmaIDByState, startDate=allYrMin, endDate=allYrMax, SIMPLIFY=F)

##"Unpacks" the output of the calcWMASectorVars function into lists in which only
##one type of table is in each list object. ...TotsGrd is actual CFS values for groundwater,
##...PersGrd is the percent sector of the total CFS for groundwater, ...TotsSur is actual CFS
##values for surface water, and PersSur is the percent sector of total CFS for surface water
wmasRightsTotsGrd <- lapply(wmasCalcGrd, function(lst){lst[[1]]})
wmasRightsPersGrd <- lapply(wmasCalcGrd, function(lst){lst[[2]]})
wmasRightsTotsSur <- lapply(wmasCalcSur, function(lst){lst[[1]]})
wmasRightsPersSur <- lapply(wmasCalcSur, function(lst){lst[[2]]})
##applies the state names to each of the above organized lists
##the original intent was to be able to refer to the list name to find the state data, 
##but in practice the code tended to refer back to the 'states' object
names(wmasRightsTotsGrd) <- names(wmasRightsPersGrd) <- names(wmasRightsTotsSur) <- names(wmasRightsPersSur) <- states


##For each state, calculates the cumulative flow in AFA. This was an initial calculation preformed
##on the Idaho data, but is not used with the current market analysis. Kept for completeness.
##idaho only
#idahoWMAVolSums <- calcWaterVol(fullRights, wmaIDByState[[1]])
##all states
wmasVolSums <- mapply(calcWaterVol, rightsTab=fullRightsRecs, wmaID=wmaIDByState)


##Saves the calculated cumulative curves to disc as R objects.
save(file=paste0(dataDir, "reorganizedData/statesCalcCumulative.RData"), "wmasRightsTotsGrd", "wmasRightsPersGrd", 
     "wmasRightsTotsSur", "wmasRightsPersSur", "wmasVolSums", "states", "wmaIDByState")
#load(paste0(dataDir, "reorganizedData/statesCalcCumulative.RData"))


##Reads in a CSV with the WMA IDs used with WBM at UNH. Within WBM, the ID structures used
##by the various states was not usable. Therefore, UNH assigned a unique ID number to each
##WMA. The conversion from the state ID to the UNH ID is held within this file, and will be
##later used to correctly name the WMA cumulative curve files.
namConvData <- read.csv(paste0(projBoxDir, "unhNamingConvention/simpleWMAs_v2_ID.csv"))


##write out the cumulative summary tables
mapply(function(st, wmaID, wrtg, wrpg, wrts, wrps){
                  ###############
                  #x <- 4
                  #st <- states[x]
                  #wmaID <- wmaIDByState[[x]]
                  #wrtg <- wmasRightsTotsGrd[[x]]
                  #wrpg <- wmasRightsPersGrd[[x]]
                  #wrts <- wmasRightsTotsSur[[x]]
                  #wrps <- wmasRightsPersSur[[x]]
                  ##############
                  ##sets up output directories for output cumulative curve data files
                  dataOut<-paste0(dataDir,"sectorCumuSummaries/",st,"/");
                  summsOutGWTots<-paste0(dataOut,"groundWater/totals/unmodified/");
                  summsOutGWPers<-paste0(dataOut,"groundWater/percentages/unmodified/");
                  summsOutSWTots<-paste0(dataOut,"surfaceWater/totals/unmodified/");
                  summsOutSWPers<-paste0(dataOut,"surfaceWater/percentages/unmodified/");
                  if(dir.exists(summsOutGWTots)==F){dir.create(summsOutGWTots, recursive=T)};
                  if(dir.exists(summsOutGWPers)==F){dir.create(summsOutGWPers, recursive=T)};
                  if(dir.exists(summsOutSWTots)==F){dir.create(summsOutSWTots, recursive=T)};
                  if(dir.exists(summsOutSWPers)==F){dir.create(summsOutSWPers, recursive=T)};
                  ##create the write out file names
                  #summsOutTotGWFiles<-paste0(summsOutGWTots, capitalizeStrings(st), "_", names(wrtg), ".csv");
                  #summsOutTotSWFiles<-paste0(summsOutSWTots, capitalizeStrings(st), "_", names(wrtg), ".csv");
                  #summsOutPerGWFiles<-paste0(summsOutGWPers, capitalizeStrings(st), "_", names(wrtg), ".csv");
                  #summsOutPerSWFiles<-paste0(summsOutSWPers, capitalizeStrings(st), "_", names(wrtg), ".csv");
                  
                  ##Extracts only the UNH translation records for a particular state
                  stTrans <- namConvData[namConvData$state==capitalizeStrings(st),];
                  #wID <- "11" 
                  
                  ##Finds and assigns the UNH unique ID from the state provided ID for each WMA
                  transWMANum <- sapply(names(wrtg), function(wID){getWMANum<-stTrans$ID[stTrans$basinNum==wID]
                                                                    if(length(getWMANum)==0){
                                                                      checkRmLead<-gsub("(?<![0-9])0+","",wID,perl=T)
                                                                      getWMANum<-stTrans$ID[stTrans$basinNum==checkRmLead]
                                                                    }
                                                                    return(getWMANum)});
                  ##It should be noted that New Mexico has three wmaIDs that do not match up with geographic boundaries: LWD, SP, SD.
                  ##This creates output files for them, but were manually deleted before handing off to UNH.
                  
                  ##Create the cumulative curve file names
                  summsOutTotGWFiles<-paste0(summsOutGWTots, "WMA_", transWMANum, "_GW.csv");
                  summsOutTotSWFiles<-paste0(summsOutSWTots, "WMA_", transWMANum, "_SW.csv");
                  summsOutPerGWFiles<-paste0(summsOutGWPers, "WMA_", transWMANum, "_GW.csv");
                  summsOutPerSWFiles<-paste0(summsOutSWPers, "WMA_", transWMANum, "_SW.csv");
                  ##Write out the cumulative curve files to disk
                  mapply(write.csv, x=wrpg, file=summsOutPerGWFiles, row.names=F)
                  mapply(write.csv, x=wrps, file=summsOutPerSWFiles, row.names=F)
                  #mapply(write.csv, x=wrtg, file=summsOutTotGWFiles, row.names=F)
                  #mapply(write.csv, x=wrts, file=summsOutTotSWFiles, row.names=F)
       }, 
       st=states, wmaID=wmaStateLabel, wrtg=wmasRightsTotsGrd, wrpg=wmasRightsPersGrd, wrts=wmasRightsTotsSur, wrps=wmasRightsPersSur)



##sets up output directories for plotting of various images to visualize the cumulative curve data
plotsOut <- paste0(dataDir, "plots/")
if(dir.exists(plotsOut)==F){dir.create(plotsOut, recursive=T)}
##A function to preform the plotting of various images related to the cumulative curves
mapply(function(st, size, wma, wmaBounds, wrUse, wrDiv, wrtg, wrpg, wrts, wrps, wrvs){
                  ###############
                  #x<-7
                  #st<-states[x]
                  #size<-plottingDim[[x]]
                  #wma<-wmaStateLabel[[x]]
                  #wmaBounds<-spatialWMAByState[[x]]
                  #wrUse<-h2oUseByState[[x]]
                  #wrUse<--9999
                  #wrDiv<-h2oDivByState[[x]]
                  #wrtg<-wmasRightsTotsGrd[[x]]
                  #wrpg<-wmasRightsPersGrd[[x]]
                  #wrts<-wmasRightsTotsSur[[x]]
                  #wrps<-wmasRightsPersSur[[x]]
                  #wrvs<-wmasVolSums[[x]]
                  ###############
                  print(st)
                  ##First, plots all WMAs within the given state
                  ##This is done for individual overview of the WMAs from within each state.
                  ##States are separated into 'tall' and 'wide' based on the shape of the state and to 
                  ##maximize the plotting space available.
                  if(size=="tall"){
                    plotAdminBounds(wmaBounds, plotsOut, st, wma, 950, 1200);
                  }else if(size=="wide"){
                    plotAdminBounds(wmaBounds, plotsOut, st, wma, 1200, 950);
                  }else{
                    print("Unsure how to plot the state data")
                  }
                  print("plotted WMA bounds")
                  
                  ##Create spatial maps of both the PoDs and PoUs, if available, overlayed on a boundary of the WMA
                  plotPointOfUseandDiv(wmaBounds, wrUse, wrDiv, plotsOut, st, wma, 1200, 1200);
                  print("plotted use and div")
                  
                  ##The plotCumuLines function creates two line graphs based on the cumulative data for a given WMA.
                  ##The first graph plots the cumulation values by sector
                  ##The second graph plots the total cumulation values of all sectors on a single line.
                  ##Originally, both graphs were created in the same plot space, but on WMA with very large flows
                  ##the total cumulation tended to dominate the space and the details of all but the greatest 
                  ##sector flows was not clearly seen.
                  plotCumuLines(wrtg, plotsOut, st, wma, "ground", 900, 800);
                  plotCumuLines(wrts, plotsOut, st, wma, "surface", 900, 800);
                  print("plotted cumu lines")
                  
                  ##The plotCumuStacked function creates a stacked graphs of the cumulative percentage for each
                  ##sector. Instead of hard cumulative flow values, the percent of the total cumulative flow
                  ##is used to define the shape of each sector. This shows the relative prominence of a given 
                  ##sector compared with others for a given time period.
                  plotCumuStacked(wrpg, plotsOut, st, wma, "ground", 900, 800);
                  plotCumuStacked(wrps, plotsOut, st, wma, "surface", 900, 800);
                  print("plotted cumu stacked lines")
                  
                  ##A line graph of the cumulative AFA data. Created for some states, but again not used
                  ##in the final analysis.
                  #if(is.null(wrvs)==F){
                  #  plotCumuAFA(wrvs, plotsOut, st, wma, 900, 800)
                  #}
              }, 
       st=states, size=plottingDim, wma=wmaStateLabel, wmaBounds=spatialWMAByState, wrUse=h2oUseByState, wrDiv=h2oDivByState, wrtg=wmasRightsTotsGrd, wrpg=wmasRightsPersGrd,  
       wrts=wmasRightsTotsSur, wrps=wmasRightsPersSur, wrvs=wmasVolSums)



