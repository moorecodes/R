###############################################################
###                                                         ###
###   Adam Moore, adam.moore@pdx.edu                        ###
###                                                         ###
###   Get Scats Cycle Volume Data                           ###
###                                                         ###
###   11/14/2013                                            ###
###                                                         ###
###############################################################

################################################################################
# Extract Scats volume data from the raw cycle-by-cycle file. This variable contains volume, but the timestamps are incorrect.
  getScatsExtract <- function(startTime,endTime,intersectionOfInterest,writeToFile=F) {
################################################################################

# Setup
#----------------
  ScatsRawDir <- "//Stash/marston/Active_Projects/12-02 Corridor Livability and Performance Measures/_Data/SCATS data/Cycle-by-Cycle Volumes_Raw"
  OutputDir <- "//Stash/marston/Active_Projects/12-02 Corridor Livability and Performance Measures/_Data/SCATS data/Cycle-by-Cycle Volumes_Processed"

  detectorOrder <- as.table(matrix(c(9,14,"EBLT",      #table of detectors corresponding to strategic approaches and laneage
                                     10,1,"WBTH",
                                     10,5,"WBTH",
                                     11,3,"NBSBTH",
                                     11,4,"NBSBTH",
                                     12,13,"WBLT",
                                     13,6,"EBTH",
                                     13,22,"EBTH"),ncol=3,byrow=T))
  intersectionIdentifiers <- as.table(matrix(c(4065,"Milwaukie",     #table of intersection codes corresponding to intersections
                                               4706,"13th",
                                               4067,"21st",
                                               4068,"26th",
                                               4069,"33rd",
                                               4070,"39th",
                                               4071,"43rd",
                                               4604,"47th",
                                               4072,"50th",
                                               4073,"52nd",
                                               4141,"65th",
                                               4064,"69th",
                                               4133,"71st",
                                               4075,"72nd"),ncol=2,byrow=T))

# Make empty dataframe (to be filled in iteratively)
#------------------
  ScatsExtract <- data.frame(Date=as.POSIXct(character()),Intersection=integer(),
                              SA=integer(),Detector=integer(),DS=integer(),VO=integer(),
                              VK=integer(),ADS=integer(),PT=integer(),PH=integer())

# Find File and Populate Volume Dataframe
#------------------
 #find file based on desired date  
  FileName <- list.files(path=ScatsRawDir, pattern=format(startTime, "%Y-%m-%d"), full.name=T)
  File <- file(FileName)
  open(File)
 #determine how long the date character string is, in order to know when the timestamp starts in the headers
  lengthTillTime <- nchar(weekdays(startTime))+1+nchar(format(startTime,"%d-%Y"))+1+nchar(months(startTime))+2
 #set iteration at 1
  i=1

 #iteratively populate dataframe
  while (length(line <- readLines(File,n=1,warn=F)) > 0) {
    if (substr(line,1,9)==weekdays(startTime)) {    #look to see if we're in the header
      cycleTime <- substr(line,lengthTillTime,lengthTillTime+4)    #if we're in the header, extract the time
      dateTime <- as.POSIXct(paste(format(startTime,"%Y-%m-%d"),cycleTime),format="%Y-%m-%d %H:%M")     #use the extracted time to build a "Y-m-d H:M" timestamp
    }
    if (substr(line,2,8)==paste(intersectionIdentifiers[grep(intersectionOfInterest,intersectionIdentifiers[,2]),1],"SA")) {     #look to see if we're in the body
        ScatsExtract[i,1] <- as.POSIXct(dateTime,format="%Y-%m-%d %H:%M")     #if we're in the body, first place the timestamp from the most recent header
        ScatsExtract[i,2] <- intersectionIdentifiers[grep(intersectionOfInterest,intersectionIdentifiers[,2]),1]
        ScatsExtract[i,3] <- substr(line,11,12)     #pull out the strategic approach
        ScatsExtract[i,5] <- substr(line,26,28)     #pull out the first DS
        ScatsExtract[i,6] <- substr(line,30,32)     #pull out the first VO
        ScatsExtract[i,7] <- substr(line,34,36)     #pull out the first VK
        ScatsExtract[i,8] <- substr(line,78,81)     #pull out the ADS
        ScatsExtract[i,9] <- substr(line,22,23)     #pull out the phase time
        ScatsExtract[i,10] <- substr(line,19,19)    #pull out the phase
        ScatsExtract[i+1,1] <- as.POSIXct(dateTime,format="%Y-%m-%d %H:%M")
        ScatsExtract[i+1,2] <- intersectionIdentifiers[grep(intersectionOfInterest,intersectionIdentifiers[,2]),1]
        ScatsExtract[i+1,3] <- substr(line,11,12)
        ScatsExtract[i+1,5] <- substr(line,39,41)    #pull out the second DS
        ScatsExtract[i+1,6] <- substr(line,43,45)    #pull out the second VO
        ScatsExtract[i+1,7] <- substr(line,47,49)    #pull out the second VK
        ScatsExtract[i+1,8] <- substr(line,78,81)
        ScatsExtract[i+1,9] <- substr(line,22,23)
        ScatsExtract[i+1,10] <- substr(line,19,19)
        ScatsExtract[i+2,1] <- as.POSIXct(dateTime,format="%Y-%m-%d %H:%M")
        ScatsExtract[i+2,2] <- intersectionIdentifiers[grep(intersectionOfInterest,intersectionIdentifiers[,2]),1]
        ScatsExtract[i+2,3] <- substr(line,11,12)
        ScatsExtract[i+2,5] <- substr(line,52,54)
        ScatsExtract[i+2,6] <- substr(line,56,58)
        ScatsExtract[i+2,7] <- substr(line,60,62)
        ScatsExtract[i+2,8] <- substr(line,78,81)
        ScatsExtract[i+2,9] <- substr(line,22,23)
        ScatsExtract[i+2,10] <- substr(line,19,19)
        ScatsExtract[i+3,1] <- as.POSIXct(dateTime,format="%Y-%m-%d %H:%M")
        ScatsExtract[i+3,2] <- intersectionIdentifiers[grep(intersectionOfInterest,intersectionIdentifiers[,2]),1]
        ScatsExtract[i+3,3] <- substr(line,11,12)
        ScatsExtract[i+3,5] <- substr(line,65,67)
        ScatsExtract[i+3,6] <- substr(line,69,71)
        ScatsExtract[i+3,7] <- substr(line,73,75)
        ScatsExtract[i+3,8] <- substr(line,78,81)
        ScatsExtract[i+3,9] <- substr(line,22,23)
        ScatsExtract[i+3,10] <- substr(line,19,19)
      i=i+4
    }
  }
  close(File)

  ScatsExtract <- ScatsExtract[-grep("-",ScatsExtract$DS),]    #delete the rows that don't have any data (since we disregarded this while populating)

  ScatsExtract$Detector <- detectorOrder[,2]    #place the detector order. NOTE that this is a rough way of doing this, i.e. not much logic.
  ScatsExtract$Movement <- detectorOrder[,3]    #place movements

  ScatsExtract$Intersection <- as.integer(ScatsExtract$Intersection)
  ScatsExtract$SA <- as.integer(ScatsExtract$SA)
  ScatsExtract$Detector <- as.integer(ScatsExtract$Detector)
  ScatsExtract$DS <- as.integer(ScatsExtract$DS)
  ScatsExtract$VO <- as.integer(ScatsExtract$VO)
  ScatsExtract$VK <- as.integer(ScatsExtract$VK)
  ScatsExtract$ADS <- as.integer(ScatsExtract$ADS)
  ScatsExtract$PT <- as.integer(ScatsExtract$PT)
  ScatsExtract$PH <- as.integer(ScatsExtract$PH)

  str(ScatsExtract)

  if (writeToFile == T) {
    write.csv(ScatsExtract,paste(OutputDir,paste(format(startTime,"%Y-%m-%d"),intersectionOfInterest,"CycleExtracted.csv",sep="_"),sep="/"),row.names=F)
  }

  return(ScatsExtract)
  }



################################################################################
# Extract some miscellaneous data from the Scats raw volume data
  getScatsExtractMisc <- function(startTime,endTime,intersectionOfInterest,writeToFile=F) {
################################################################################

# Setup
#----------------
  ScatsRawDir <- "//Stash/marston/Active_Projects/12-02 Corridor Livability and Performance Measures/_Data/SCATS data/Cycle-by-Cycle Volumes_Raw"
  OutputDir <- "//Stash/marston/Active_Projects/12-02 Corridor Livability and Performance Measures/_Data/SCATS data/Cycle-by-Cycle Volumes_Processed"

# Make empty dataframe (to be filled in iteratively)
#------------------
  ScatsMisc <- data.frame(Date=as.POSIXct(character()),CT=integer(),
                         SA=integer(),DS=integer(),A=integer(),B=integer(),
                         C=integer(),D=integer(),E=integer(),f=integer(),G=integer())

# Find File and Populate Miscellaneous Dataframe
#------------------
#find file based on desired date  
  FileName <- list.files(path=ScatsRawDir, pattern=format(startTime,"%Y-%m-%d"), full.name=T)    
  File <- file(FileName)
  open(File)
 #determine how long the date character string is, in order to know when the timestamp starts in the headers
  lengthTillTime <- nchar(weekdays(startTime))+1+nchar(format(startTime,"%d-%Y"))+1+nchar(months(startTime))+2
 #set iteration at 1
  i=1

 #iteratively populate dataframe
  while (length(line <- readLines(File,n=1,warn=F)) > 0) {
    if (substr(line,1,9)==weekdays(startTime)) {    #look to see if we're in the header
      cycleTime <- substr(line,lengthTillTime,lengthTillTime+4)    #if we're in the header, extract the time
      dateTime <- as.POSIXct(paste(format(startTime,"%Y-%m-%d"),cycleTime),format="%Y-%m-%d %H:%M")     #use the extracted time to build a "Y-m-d H:M" timestamp
      ScatsMisc[i,1] <- as.POSIXct(dateTime,format="%Y-%m-%d %H:%M")
      ScatsMisc[i,2] <- substr(line,lengthTillTime+35,lengthTillTime+37)     #pull out the cycle time
      ScatsMisc[i,3] <- substr(line,lengthTillTime+52,lengthTillTime+53)     #pull out SA
      ScatsMisc[i,4] <- substr(line,lengthTillTime+59,lengthTillTime+61)     #pull out DS
    }
    if (substr(line,1,1)=="A") {     #look to see if we're in the phase allotments
      ScatsMisc[i,5] <- substr(line,4,5)     #pull out phase A
      ScatsMisc[i,6] <- substr(line,11,12)     #pull out phase B
      ScatsMisc[i,7] <- substr(line,17,18)     #pull out phase C
      ScatsMisc[i,8] <- substr(line,22,23)     #pull out phase D
      ScatsMisc[i,9] <- substr(line,28,29)     #pull out phase E
      ScatsMisc[i,10] <- substr(line,33,34)     #pull out phase F
      ScatsMisc[i,11] <- substr(line,39,40)     #pull out phase G
      i=i+1
    }
  }
  close(File)

  ScatsMisc$CT <- as.integer(ScatsMisc$CT)
  ScatsMisc$SA <- as.integer(ScatsMisc$SA)
  ScatsMisc$DS <- as.integer(ScatsMisc$DS)

  str(ScatsMisc)

  if (writeToFile == T) {
    write.csv(ScatsMisc,paste(OutputDir,paste(format(startTime,"%Y-%m-%d"),intersectionOfInterest,"CycleMisc.csv",sep="_"),sep="/"),row.names=F)
  }

  return(ScatsMisc)
  }



################################################################################
# Get stage start and end time, since the raw volume data do not include seconds (bummer)
  getScatsStage <- function(startTime,endTime,intersectionOfInterest,writeToFile=F) {
################################################################################
  
  # Setup
  #----------------
  ScatsRawDir <- "//Stash/marston/Active_Projects/12-02 Corridor Livability and Performance Measures/_Data/SCATS data/Cycle-by-Cycle Volumes_Raw"
  OutputDir <- "//Stash/marston/Active_Projects/12-02 Corridor Livability and Performance Measures/_Data/SCATS data/Cycle-by-Cycle Volumes_Processed"
  ScatsStageDir <- paste("//Stash/marston/Active_Projects/12-02 Corridor Livability and Performance Measures/_Data/SCATS data/Phases",format(startTime,"%Y-%m-%d"),sep="/")

 #find file based on desired date
  FileName <- list.files(path=ScatsStageDir, pattern=intersectionOfInterest, full.name=T)
  ScatsStage <- read.table(FileName,header=F,skip=1,fill=T,col.names=c("Day","Date","StartTime","EndTime","Duration","Phase","GapOut"))
  ScatsStage$StartTimeStamp <- as.POSIXct(paste(ScatsStage$Date,ScatsStage$StartTime),format="%d-%b-%Y %H:%M:%S")
  ScatsStage$EndTimeStamp <- as.POSIXct(paste(ScatsStage$Date,ScatsStage$EndTime),format="%d-%b-%Y %H:%M:%S")
  ScatsStage <- ScatsStage[ScatsStage$StartTimeStamp >= startTime & ScatsStage$StartTimeStamp <= endTime,]

  ScatsStage <- unique(ScatsStage[,-c(1,2,3,4)])
  ScatsStage <- ScatsStage[complete.cases(ScatsStage),]
  names(ScatsStage)[grep("Phase",names(ScatsStage))] <- "Stage"

  if (writeToFile == T) {
    write.csv(ScatsStage,paste(OutputDir,paste(format(startTime,"%Y-%m-%d"),intersectionOfInterest,"StageTime.csv",sep="_"),sep="/"),row.names=F)
  }
  
  return(ScatsStage)
  }



################################################################################
# Couple cycle start and end times with the extracted volume data. This gives volume per cycle, as well as cycle length.
  getScatsCycleStats <- function(startTime,endTime,intersectionOfInterest,writeToFile=F) {
################################################################################
 #Call a few other Scats functions to get the necessary volumes and stages
  ScatsExtract <- getScatsExtract(startTime,endTime,intersectionOfInterest)
  ScatsStage <- getScatsStage(startTime,endTime,intersectionOfInterest)

 #Load hydroGOF package for calculating RMSE
  library(hydroGOF)
  #library(R.utils)

  Vol <- ScatsExtract$PT[ScatsExtract$Detector == 3]
  Stage <- ScatsStage$Duration[ScatsStage$Stage == "D"]
  
  setforward <- NA
  setback <- NA

  lowRMSE <- rmse(Vol,Stage)

  for (i in 1:5) {
    remove <- -((length(Stage)-(i-1)):length(Stage))
    stage <- Stage[remove]
    stage <- insert(stage,1,rep(NA,length(remove)))
    lowestRMSE <- min(rmse(Vol,stage),lowRMSE,na.rm=T)
    if (lowestRMSE < lowRMSE) {
      setback <- i
    }
    lowRMSE <- lowestRMSE
  }
  for (i in 1:5) {
    remove <- -(1:i)
    stage <- Stage[remove]
    stage <- insert(stage,length(stage)+1,rep(NA,length(remove)))
    lowestRMSE <- min(rmse(Vol,stage),lowRMSE,na.rm=T)
    if (lowestRMSE < lowRMSE) {
      setforward <- -i
      setback <- NA
    }
    lowRMSE <- lowestRMSE
  }

  startCycle <- ScatsStage$StartTimeStamp[ScatsStage$Stage == "A"]
  startCycle <- as.character(startCycle)

  if (!is.na(setback) == T) {
    remove <- -((length(startCycle)-(setback-1)):length(startCycle))
    alignedStartCycle <- startCycle[remove]
    alignedStartCycle <- insert(alignedStartCycle,1,rep(NA,length(remove)))
  }
  if (!is.na(setforward) == T) {
    remove <- -(1:setforward)
    alignedStartCycle <- startCycle[remove]
    alignedStartCycle <- insert(alignedStartCycle,length(stage)+1,rep(NA,length(remove)))
  }

  endCycle <- ScatsStage$EndTimeStamp[which(ScatsStage$Stage %in% "A")-1]
  endCycle <- as.character(endCycle)
  endCycle[1] <- NA
  alignedEndCycle <- endCycle

  ScatsExtract$CycleStart <- NA
  for (i in 1:length(unique(ScatsExtract$Date))) {
    ScatsExtract$CycleStart[min(grep(unique(ScatsExtract$Date)[i],ScatsExtract$Date))] <- alignedStartCycle[i]
  }

  ScatsExtract$CycleEnd <- NA
  for (i in 1:length(unique(ScatsExtract$Date))) {
    ScatsExtract$CycleEnd[max(grep(unique(ScatsExtract$Date)[i],ScatsExtract$Date))] <- alignedEndCycle[i]
  }

  CycleStats <- data.frame(CycleStart=startCycle[-length(startCycle)])
  for (i in 1:nrow(CycleStats)) {
    if (i==nrow(CycleStats)) {
      CycleStats$CycleVolume[i] <- sum(ScatsExtract$VO[seq(from=grep(startCycle[i],ScatsExtract$CycleStart),to=nrow(ScatsExtract),by=1)])
    }else{
      CycleStats$CycleVolume[i] <- sum(ScatsExtract$VO[seq(from=grep(startCycle[i],ScatsExtract$CycleStart),to=grep(startCycle[i+1],ScatsExtract$CycleStart)-1,by=1)])
    }
  }
  CycleStats$CycleStart <- as.POSIXct(strptime(CycleStats$CycleStart, "%Y-%m-%d %H:%M:%S"))

  for (i in 1:nrow(CycleStats)) {
    if (i == nrow(CycleStats)) {
      CycleStats$CycleLength[i] <- NA
    }else{
      CycleStats$CycleLength[i] <- difftime(CycleStats$CycleStart[i+1],CycleStats$CycleStart[i],units="secs")
    }
  }

  if (writeToFile == T) {
    write.csv(CycleStats,paste(OutputDir,paste(format(startTime,"%Y-%m-%d"),intersectionOfInterest,"CycleVolumes.csv",sep="_"),sep="/"),row.names=F)
  }

  return(CycleStats)
  }



################################################################################
# Get second-by-second stages
  getScatsSecondsStage <- function(startTime,endTime,intersectionOfInterest,writeToFile=F) {
################################################################################
  ScatsStage <- getScatsStage(startTime,endTime,intersectionOfInterest)

 #just the stages
  svol <- data.frame(date=seq.POSIXt(from=min(ScatsStage$StartTimeStamp),to=max(ScatsStage$EndTimeStamp),by="1 sec"))
  svol$Stage <- NA
  for (i in 1:(nrow(svol)-1)) {
      svol$Stage[i] <- as.character(ScatsStage$Stage)[svol$date[i]>=ScatsStage$StartTimeStamp & svol$date[i]<ScatsStage$EndTimeStamp]
  }
  svol <- svol[-nrow(svol),]

  return(svol)
  }



################################################################################
# END
################################################################################
