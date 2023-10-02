##########################################################################
##########################################################################
## Script Name: WMAAggFunctions.R
## Purpose of Script: A file which holds the custom function written 
## specifically for this project. This file is called by almost all of the 
## other script files for one function or another.
##
## Special Requirements: None - this is meant to be called by other scripts.
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 11/12/2019
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 6/12/2023
##
## Copyright (c) 2023 The Pennsylvania State University
##
##########################################################################
##########################################################################
##custom function for PCHES WMA Aggregations
##########################################################################
##########################################################################
##This function subsets rights based on WMA, water source, sector, and saves 4 types of intermediate output. 1) Rights in each subset sector,
##ordered by priority date, will be saved under "RIGHTS_SECTOR_Sector_WMA_Id_Source.csv." 2) The cfs of each year will be saved under "CFS_PER_YEAR_Sector_WMA_Id_Source.csv."
##3) The sum of cfs over time will be saved under "CUMM_AMT_PER_YR_Sector_WMA_Id_Source.csv." 4) A data frame of all sector's cumulative cfs over time will
##be saved as "ALL_SEC_CUML_WMA_Id_Source.csv."
calcWMASectorVars <- function(rights, state, wmaID, startDate, endDate){
  #################
  #n <- 4
  #rights <- rightsByState_surface[[n]]  ##is the dataframe input.
  #state <- states[n]
  #wmaID <- wmaIDByState[[n]]  ##a vector of numeric administrative basin identifying numbers. E.g. WMA 11, WMA 37, WMA 98.
  #startDate <- allYrMin
  #endDate <- 2100
  #################

  ##Create a vector of unique sector variables
  sectCats <- c("irrigation", "domestic", "livestock", "fish", "industrial", "environmental", "other")
  
  ##calculate the cumulative total by WMA, by sector, and by year
  if(state=="arizona"){
    ##Arizona is unique here as it is the only state to have two set of WMA boundaries, one for surface water
    ##and the other for groundwater
    ##Currently only sorts records by surface basins, may need to recalculate base on ground water basins
    rightsByWMABySect <- pblapply(wmaID, function(x,tab,sects){wmaTab<-tab[(tab$surBasinNum==x & is.na(tab$surBasinNum)==F),];
                                                                ##create an output object data frame in which to organize the cumulative totals for water rights
                                                                rightsByWMA <- data.frame(Year=startDate:endDate, Irrigation=NA, Domestic=NA, Livestock=NA, 
                                                                                            Fish=NA, Industrial=NA, Environmental=NA, Other=NA)
                                                                
                                                                ##Fill out the the WMA data frame with the water rights from that WMA
                                                                for(sect in sectCats){
                                                                  ##Subset industry and make it its own data frame with the priority date in the first column and 
                                                                  ##the Overall Max CFS in the second
                                                                  sectorRecs <- wmaTab[grepl(sect, as.character(wmaTab$waterUse)),]
                                                                  if(nrow(sectorRecs)>0){
                                                                    ##Sort sector according to priority date.
                                                                    sectorRecs <- sectorRecs[order(ymd(sectorRecs$priorityDate), decreasing=F),]
                                                                    ##aggregate cfs by year
                                                                    cfsYrTotal <- aggregate(data=sectorRecs, CFS~year(priorityDate), FUN=sum)
                                                                    ##if any priority years are unrealistic, then set them to the binding years
                                                                    cfsYrTotal$`year(priorityDate)`[cfsYrTotal$`year(priorityDate)`>endDate]<-endDate
                                                                    ##preform the cumulation of each sector
                                                                    cfsCumuSum <- data.frame(year=cfsYrTotal$`year(priorityDate)`, cumuCFS=cumsum(cfsYrTotal$CFS))
                                                                    ##add the CFS aggregation to the output cumulation table
                                                                    rightsByWMA[which(rightsByWMA$Year %in% cfsCumuSum$year), which(colnames(rightsByWMA)==capitalizeStrings(sect))] <- cfsCumuSum$cumuCFS
                                                                  }
                                                                }
                                                                ##for each NA, replace with previous non-NA value
                                                                rightsByWMA[1,is.na(rightsByWMA[1,])]<-0;
                                                                fillTab<-na.locf(rightsByWMA);
                                                                return(fillTab)}, tab=rights, sects=sectCats)  #sects=sectClasses)
  }else{
    rightsByWMABySect <- pblapply(wmaID, function(x,tab,sects){wmaTab<-tab[(tab$basinNum==x & is.na(tab$basinNum)==F),];
                                                                ##create an output object data frame in which to organize the cumulative totals for water rights
                                                                rightsByWMA <- data.frame(Year=startDate:endDate, Irrigation=NA, Domestic=NA, Livestock=NA, 
                                                                                           Fish=NA, Industrial=NA, Environmental=NA, Other=NA)
                                                                
                                                                ##Fill out the the WMA data frame with the water rights from that WMA
                                                                for(sect in sectCats){
                                                                  ##Subset industry and make it its own data frame with the priority date in the first column and 
                                                                  ##the Overall Max CFS in the second
                                                                  sectorRecs <- wmaTab[grepl(sect, as.character(wmaTab$waterUse)),]
                                                                  if(nrow(sectorRecs)>0){
                                                                    ##Sort sector according to priority date.
                                                                    sectorRecs <- sectorRecs[order(ymd(sectorRecs$priorityDate), decreasing=F),]
                                                                    ##aggregate cfs by year
                                                                    cfsYrTotal <- aggregate(data=sectorRecs, CFS~year(priorityDate), FUN=sum)
                                                                    ##if any priority years are unrealistic, then set them to the binding years
                                                                    cfsYrTotal$`year(priorityDate)`[cfsYrTotal$`year(priorityDate)`>endDate]<-endDate
                                                                    ##preform the cumulation of each sector
                                                                    cfsCumuSum <- data.frame(year=cfsYrTotal$`year(priorityDate)`, cumuCFS=cumsum(cfsYrTotal$CFS))
                                                                    ##add the CFS aggregation to the output cumulation table
                                                                    rightsByWMA[which(rightsByWMA$Year %in% cfsCumuSum$year), which(colnames(rightsByWMA)==capitalizeStrings(sect))] <- cfsCumuSum$cumuCFS
                                                                  }
                                                                }
                                                                ##for each NA, replace with previous non-NA value
                                                                rightsByWMA[1,is.na(rightsByWMA[1,])]<-0;
                                                                fillTab<-na.locf(rightsByWMA);
                                                                return(fillTab)}, tab=rights, sects=sectCats)
  }
  #x<-1; x<-48; x<-8; x<-"001"; x<-"1_2; x<-"RG"; x<-"17"

  ##total cumulative water use by WMA
  wmaRightsTotal <- lapply(rightsByWMABySect, function(datTab){totTab<-data.frame(Year=datTab$Year,CUML=rowSums(datTab[,2:ncol(datTab)]),datTab[,2:ncol(datTab)]);
                                                        return(totTab)})
  
  ##calculates percentage 
  wmaRightsPer <- lapply(wmaRightsTotal, function(datTab){perTab<-cbind.data.frame(datTab$Year,datTab$CUML,(datTab[,3:ncol(datTab)]/datTab$CUML)*100);
                                                      colnames(perTab)<-colnames(datTab);
                                                      ##convert the nans to 0s
                                                      perTab[is.na(perTab)]<-0;
                                                      return(perTab)})

  ##Organizes objects to be returned
  names(wmaRightsTotal) <- names(wmaRightsPer) <- wmaID
  return(list(wmaRightsTotal,wmaRightsPer))
}
##########################################################################
##########################################################################
##This function calculates the cumulative AFA sum of all rights with a volume amount appropriated.
calcWaterVol <- function(rightsTab, wmaID){
  #################
  #rightsTab <- fullRights
  #wmaID <- wmaIDByState[[1]]
  #################
  
  if("volume" %in% names(rightsTab)){
    ##extract date and volume data from the full data table
    volByWMA <- lapply(wmaID, function(x,tab,sects){wmaTab<-tab[tab$basinNum==x,];
                                                    volTab<-wmaTab[which(is.na(wmaTab$volume)==F),c("priorityDate", "volume")];
                                                    datOrd<-volTab[order(ymd(volTab[,"priorityDate"]),decreasing=F),];
                                                    datOrd[,"volume"] <- as.numeric(datOrd[,"volume"]);
                                                    return(datOrd)}, tab=rightsTab)
    
    ##aggregate cfs volume by year
    aggVolByYr <- lapply(volByWMA, function(xTab){aggTab<-aggregate(data=xTab, volume~year(ymd(priorityDate)),FUN=sum);
                                                  colnames(aggTab)<-c("Year","AFA");
                                                  return(aggTab)})
    
    ##cumulative sum over years
    afaCumuSums <- lapply(aggVolByYr, function(xTab){data.frame(Year=xTab$Year, AfaSumCumu=cumsum(xTab$AFA))})
    names(afaCumuSums) <- wmaID
    
    return(afaCumuSums)
  }else{
    return(NULL)
  }
}
##########################################################################
##########################################################################
##a function to plot the administrative WMAs for each state
plotAdminBounds <- function(adminBounds, plotLoc, state, boundLab, pixX, pixY){
  #################
  #adminBounds <- wmaBounds  ##the administrative boundaries to plot
  #boundLab <- wma
  #plotLoc <- plotsOut  ##the major location for which plots will be written out to
  #state <- st
  #pixX <- 900  ##the x of the output image, in pixels
  #pixY <- 1200  ##the y of the output image, in pixels
  #pixX <- 1200
  #pixY <- 900
  #################
  
  ##sets up output file structure
  stBndPltOut <- paste0(plotLoc,state,"/adminBounds/")
  if(dir.exists(stBndPltOut)==F){dir.create(stBndPltOut,recursive=T)}
  
  ##setting up the ggplot base - all state admin bounds plotted blank
  stateBase <- ggplot(adminBounds) + aes(long,lat,group=group) + geom_polygon(fill=NA,color="#969696") + theme_tufte()
  
  ##plots each individual boundary plot
  pblapply(adminBounds$basinNum, function(x){png(paste0(stBndPltOut,state,"_",boundLab,x,".png"), width=pixX, height=pixY);
                                              print(stateBase + geom_polygon(data=adminBounds[adminBounds$basinNum==x,],fill="blue",color="black"));
                                              dev.off()})                                    
}
##########################################################################
##########################################################################
##a function to map the point of use spatial data
cyclePlots <- function(admBound, waterUse, pntDiv, plotLocPoU, plotLocPoD, state, lab, passX, passY){
  #################
  #x<-30
  #x<-"7551-7557-7558"
  #admBound <- wmaBounds[wmaBounds$basinNum==x,]
  #lab <- wma
  #waterUse <- wrUse[wrUse$basinNum==x,]
  #pntDiv <- wrDiv[wrDiv$basinNum==x,]
  #plotLocPoU <- stPoUPltOuts
  #plotLocPoD <- stPoDPltOuts
  #state <- st
  #passX <- pixX
  #passY <- pixY
  #################
  
  polyBase <- ggplot(admBound) + aes(long,lat,group=group) + geom_polygon(fill=NA,color="#000000") + theme_tufte()
  ptBase <- ggplot(admBound) + aes(long,lat) + geom_polygon(fill=NA,color="#000000") + theme_tufte()
  adminID <- unique(admBound$basinNum)
  
  ###########################
  
  if(class(waterUse)=="numeric"){
    ##plot the points of use (all) for each wma
    png(paste0(plotLocPoU[1],state,"_",lab,adminID,"_PoUall.png"), width=passX, height=passY)
    print(polyBase + geom_polygon(data=waterUse,fill="#00ffff96",color="#969696"))
    dev.off()
    
    ##plot the points of use (ground water) for each wma
    png(paste0(plotLocPoU[2],state,"_",lab,adminID,"_PoUgrd.png"), width=passX, height=passY)
    if("GROUND WATER" %in% waterUse$source){
      print(polyBase + geom_polygon(data=waterUse[waterUse$source=="GROUND WATER",],fill="#65432196",color="#969696"))
    }else{
      print(polyBase)
    }
    dev.off()
    
    ##plots the points of use (surface water) for each wma
    png(paste0(plotLocPoU[3],state,"_",lab,adminID,"_PoUsur.png"), width=passX, height=passY)
    if("SURFACE WATER" %in% waterUse$source){
      print(polyBase + geom_polygon(data=waterUse[waterUse$source=="SURFACE WATER",],fill="#0000ff96",color="#969696"))
    }else{
      print(polyBase)
    }
    dev.off()
  }
  
  ###########################
  
  ##plot the points of diversion (all) for each wma
  png(paste0(plotLocPoD[1],state,"_",lab,adminID,"_PoDall.png"), width=passX, height=passY)
  print(autoplot(pntDiv,p=ptBase,colour="#00ffff96"))
  dev.off()
  
  ##plot the points of diversion (ground water) for each wma
  png(paste0(plotLocPoD[2],state,"_",lab,adminID,"_PoDgrd.png"), width=passX, height=passY)
  if("GROUND WATER" %in% pntDiv$source){
    print(autoplot(pntDiv[pntDiv$source=="GROUND WATER",],p=ptBase,colour="#65432196"))
  }else{
    print(polyBase)
  }
  dev.off()
  
  ##plots the points of diversion (surface water) for each wma
  png(paste0(plotLocPoD[3],state,"_",lab,adminID,"_PoDsur.png"), width=passX, height=passY)
  if("SURFACE WATER" %in% pntDiv$source){
    print(autoplot(pntDiv[pntDiv$source=="SURFACE WATER",],p=ptBase,colour="#0000ff96"))
  }else{
    print(polyBase)
  }
  dev.off()
}
##########################################################################
##########################################################################
##a function to map the point of use spatial data
plotPointOfUseandDiv <- function(adminBounds, waterUShp, waterDShp, plotLoc, state, boundLab, pixX, pixY){
  #################
  #adminBounds <- wmaBounds
  #boundLab <- wma
  #waterUShp <- wrUse
  #waterDShp <- wrDiv
  #plotLoc <- plotsOut
  #state <- st
  #pixX <- 1200  ##the x of the output image, in pixels
  #pixY <- 1200  ##the y of the output image, in pixels
  #################
  
  ##sets up output file structure
  sources <- c("allSources","ground", "surface")
  stPoUPltOuts <- paste0(plotLoc,state,"/",sources,"/pointOfUse/")
  sapply(stPoUPltOuts,function(x){if(dir.exists(x)==F){dir.create(x,recursive=T)}})
  stPoDPltOuts <- paste0(plotLoc,state,"/",sources,"/pointOfDiv/")
  sapply(stPoDPltOuts,function(x){if(dir.exists(x)==F){dir.create(x,recursive=T)}})
  
  ##hands data to plotting code wrapper
  if(class(waterUShp)=="numeric"){
    ##if water use is missing
    pblapply(adminBounds$basinNum, function(x){cyclePlots(adminBounds[adminBounds$basinNum==x,], NULL, 
                                                          waterDShp[waterDShp$basinNum==x,], stPoUPltOuts, stPoDPltOuts, state, boundLab, pixX, pixY)})
  }else{
    ##as expected
    pblapply(adminBounds$basinNum, function(x){cyclePlots(adminBounds[adminBounds$basinNum==x,], waterUShp[waterUShp$basinNum==x,], 
                                                          waterDShp[waterDShp$basinNum==x,], stPoUPltOuts, stPoDPltOuts, state, boundLab, pixX, pixY)})
  }
}
##########################################################################
##########################################################################
##a function to auto select the colors to create the correct color gradient for ggplot2
selectCols <- function(availDat){
  #################
  #availDat <- colnames(tab)[-c(1,ncol(tab))]
  #################
  colorGrad <- vector()
  
  if("Irrigation" %in% availDat){
    colorGrad <- c(colorGrad, "#bebebe")
  }
  if("Domestic" %in% availDat){
    colorGrad <- c(colorGrad, "#0000ff")
  }
  if("Livestock" %in% availDat){
    colorGrad <- c(colorGrad, "#ff0000")
  }
  if("Fish" %in% availDat){
    colorGrad <- c(colorGrad, "#006400")
  }
  if("Industrial" %in% availDat){
    colorGrad <- c(colorGrad, "#551a8b")
  }
  if("Environmental" %in% availDat){
    colorGrad <- c(colorGrad, "#00ff00")
  }
  if("Other" %in% availDat){
    colorGrad <- c(colorGrad, "#ffa500")
  }
  
  if(length(colorGrad)==1){
    colorGrad <- c(colorGrad, colorGrad, colorGrad)
  }
  
  return(colorGrad)
}
##########################################################################
##########################################################################
##plot the cumulative amount for different sectors, over time, in a given adminstrative basin.
##as well as the total CFS
plotCumuLines <- function(wmaCumuDat, plotLoc, state, boundLab, source, pixX, pixY){
  #################
  #wmaCumuDat <- wrtg
  #plotLoc <- plotsOut
  #state <- st
  #boundLab <- wma
  #source <- "ground"
  #pixX <- 900  ##the x of the output image, in pixels
  #pixY <- 800  ##the y of the output image, in pixels
  #################
  
  ##sets up output file structure
  cumuSectRightsPltOut <- paste0(plotLoc,state,"/",source,"/cumuRightsBySector/")
  if(dir.exists(cumuSectRightsPltOut)==F){dir.create(cumuSectRightsPltOut,recursive=T)}
  
  ##cfs by sector line plots
  lapply(1:length(wmaCumuDat),function(x){chkCols<-colSums(wmaCumuDat[[x]]);
                                  if(length(which(chkCols==0))>0){
                                    subTab<-wmaCumuDat[[x]][,-c(which(chkCols==0))];  ##only keep columns with data, defined as having a sum greater than 0
                                  }else{subTab<-wmaCumuDat[[x]]}
                                  if(class(subTab)=="data.frame"){
                                    plotCols<-selectCols(colnames(subTab)[-c(1:2)]);
                                    chzTab<-melt(subTab[,-c(which(colnames(subTab)=="CUML"))],id.vars="Year");
                                    png(paste0(cumuSectRightsPltOut,state,"_",boundLab,names(wmaCumuDat)[x],"_cumuRightsBySector.png"), width=pixX, height=pixY)
                                    print(ggplot(chzTab, aes(Year, value, group=variable)) + geom_line(aes(color=variable),size=2) + theme_tufte() +
                                          scale_color_manual(values=plotCols, name="Sector") + labs(x="Year", y="Cumulative CFS") +
                                            theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text.x=element_text(size=16),
                                                  axis.text.y=element_text(size=16), legend.title=element_text(size=20,hjust=0.5), legend.text=element_text(size=20)))
                                    dev.off()}})
  
  ##total cfs plot
  lapply(1:length(wmaCumuDat),function(x){totCols<-c(which(colnames(wmaCumuDat[[x]])%in%c("Year","CUML")));  ##when all tables are formatted the same, as they should be
                                    subTab<-wmaCumuDat[[x]][,totCols];
                                    png(paste0(cumuSectRightsPltOut,state,"_",boundLab,names(wmaCumuDat)[x],"_cumuRightsTotalCFS.png"), width=pixX, height=pixY)
                                    print(ggplot(subTab, aes(Year, CUML)) + geom_line(size=2) + theme_tufte() + labs(x="Year", y="Cumulative CFS") +
                                            theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text.x=element_text(size=16),
                                                  axis.text.y=element_text(size=16), legend.title=element_text(size=20), legend.text=element_text(size=20))) 
                                    dev.off()})
}
##########################################################################
##########################################################################
##a function to plot the percent volume used for each sector, by wma
plotCumuStacked <- function(wmaCumuDat, plotLoc, state, boundLab, source, pixX, pixY){
  #################
  #wmaCumuDat <- wrpg
  #plotLoc <- plotsOut
  #state <- st
  #source <- "ground"
  #pixX <- 800  ##the x of the output image, in pixels
  #pixY <- 900  ##the y of the output image, in pixels
  #################
  
  ##sets up output file structure
  cumuSectPerPltOut <- paste0(plotLoc,state,"/",source,"/cumuPercentBySector/")
  if(dir.exists(cumuSectPerPltOut)==F){dir.create(cumuSectPerPltOut,recursive=T)}
  
  lapply(1:length(wmaCumuDat),function(x){chkCols<-colSums(wmaCumuDat[[x]]);
                                  if(length(which(chkCols==0))>0){
                                    subTab<-wmaCumuDat[[x]][,-c(which(chkCols==0))];  ##only keep columns with data, defined as having a sum greater than 0
                                  }else{subTab<-wmaCumuDat[[x]]}
                                  if(class(subTab)=="data.frame"){
                                    plotCols<-selectCols(colnames(subTab)[-c(1:2)]);
                                    chzTab<-melt(subTab[,-c(which(colnames(subTab)=="CUML"))],id.vars="Year");
                                    #chzTab$variable<-factor(chzTab$variable,levels=rev(levels(chzTab$variable)));
                                    #chzTab$variable<-factor(capitalizeStrings(as.character(chzTab$variable)),levels=rev(capitalizeStrings(as.character(levels(chzTab$variable)))));
                                    png(paste0(cumuSectPerPltOut,state,"_",boundLab,names(wmaCumuDat)[x],"_cumuRightsStacked.png"), width=pixX, height=pixY)
                                    print(ggplot(chzTab, aes(Year, value, group=variable)) + geom_area(aes(fill=variable)) + theme_tufte() +
                                          scale_fill_manual(values=plotCols, name="Sector") + labs(x="Year", y="% Total Flow Allocation") +
                                          theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text.x=element_text(size=16),
                                                axis.text.y=element_text(size=16), legend.title=element_text(size=20,hjust=0.5), legend.text=element_text(size=20)))
                                    dev.off()}})
}
##########################################################################
##########################################################################
##a function to plot cumulative AFA volume by wma
plotCumuAFA <- function(wmaCumuDat, plotLoc, state, boundLab, pixX, pixY){
  #################
  #wmaCumuDat <- idahoWMAVolSums
  #plotLoc <- plotsOut
  #state <- "idaho"
  #pixX <- 900  ##the x of the output image, in pixels
  #pixY <- 800  ##the y of the output image, in pixels
  #################
  
  ##sets up output file structure
  afaVolPltOut <- paste0(plotLoc,state,"/cumuAFAVolume/")
  if(dir.exists(afaVolPltOut)==F){dir.create(afaVolPltOut,recursive=T)}

  lapply(1:length(wmaCumuDat),function(x){png(paste0(afaVolPltOut,state,"_",boundLab,names(wmaCumuDat)[x],"_cumuRightsTotalAFA.png"), width=pixX, height=pixY)
                                          print(ggplot(wmaCumuDat[[x]], aes(Year, AfaSumCumu)) + geom_line(size=2) + theme_tufte() + labs(x="Year", y="Cumulative AFA", fill="Sector") +
                                                  theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text.x=element_text(size=16),
                                                        axis.text.y=element_text(size=16), legend.title=element_text(size=20), legend.text=element_text(size=20)))
                                          dev.off()})
}
##########################################################################
##########################################################################
