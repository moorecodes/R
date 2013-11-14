###############################################################
###                                                         ###
###   Adam Moore, adam.moore@pdx.edu                        ###
###   Powell Corridor Performance Study                     ###
###   Manual Video Counts                                   ###
###                                                         ###
###############################################################

################################################################################
#Pull in the manual video counts
  getManualVideoCounts <- function(startTime,endTime) {
################################################################################
    
# Setup
#----------------
  manualCountsDir <- "//Stash/marston/Active_Projects/12-02 Corridor Livability and Performance Measures/_Data/Data Collection Video Counts"

  #startTime <- as.POSIXct("2013-05-01 08:00:00")
  #endTime <- as.POSIXct("2013-05-01 08:30:00")


# Get Data
#----------------
  FileName <- list.files(path=manualCountsDir, pattern=format(startTime, "%Y-%m-%d"), full.name=T)
  mcounts <- read.csv(FileName, header=T)
  mcounts$Time <- as.POSIXct(strptime(mcounts$Time, "%m/%d/%Y %H:%M:%S"))


# Summarize
#----------------
  WB <- mcounts$d1+mcounts$d1heavy+mcounts$d1bus+mcounts$d5+mcounts$d5heavy+mcounts$d5bus-mcounts$WBRT-mcounts$WBRTheavy
    WB[WB<0] <- 0
  WBheavy <- mcounts$d1heavy+mcounts$d5heavy-mcounts$WBRTheavy
    WBheavy[WBheavy<0] <- 0
  WBbus <- mcounts$d1bus+mcounts$d5bus
    WBbus[WBbus<0] <- 0
  WBRT <- mcounts$WBRT+mcounts$WBRTheavy
    WBRT[WBRT<0] <- 0
  WBRTheavy <- mcounts$WBRTheavy
    WBRTheavy[WBRTheavy<0] <- 0
  WBLT <- mcounts$d13+mcounts$d13heavy
    WBLT[WBLT<0] <- 0
  WBLTheavy <- mcounts$d13heavy
    WBLTheavy[WBLTheavy<0] <- 0
  EB <- mcounts$d22+mcounts$d22heavy+mcounts$d22bus+mcounts$d6+mcounts$d6heavy+mcounts$d6bus
    EB[EB<0] <- 0
  EBheavy <- mcounts$d22heavy+mcounts$d6heavy
    EBheavy[EBheavy<0] <- 0
  EBbus <- mcounts$d22bus+mcounts$d6bus
    EBbus[EBbus<0] <- 0
  EBRT <- mcounts$d2+mcounts$d2heavy
    EBRT[EBRT<0] <- 0
  EBRTheavy <- mcounts$d2heavy
    EBRTheavy[EBRTheavy<0] <- 0
  EBLT <- mcounts$d14+mcounts$d14heavy
    EBLT[EBLT<0] <- 0
  EBLTheavy <- mcounts$d14heavy
    EBLTheavy[EBLTheavy<0] <- 0
  NB <- mcounts$d3+mcounts$d3heavy+mcounts$d3bus-mcounts$NBRT-mcounts$NBRTheavy
    NB[NB<0] <- 0
  NBheavy <- mcounts$d3heavy-mcounts$NBRTheavy
    NBheavy[NBheavy<0] <- 0
  NBbus <- mcounts$d3bus
    NBbus[NBbus<0] <- 0
  NBRT <- mcounts$NBRT+mcounts$NBRTheavy
    NBRT[NBRT<0] <- 0
  NBRTheavy <- mcounts$NBRTheavy
    NBRTheavy[NBRTheavy<0] <- 0
  NBLT <- mcounts$d7+mcounts$d7heavy
    NBLT[NBLT<0] <- 0
  NBLTheavy <- mcounts$d7heavy
    NBLTheavy[NBLTheavy<0] <- 0
  SB <- mcounts$d4+mcounts$d4heavy+mcounts$d4bus-mcounts$SBRT-mcounts$SBRTheavy
    SB[SB<0] <- 0
  SBheavy <- mcounts$d4heavy-mcounts$SBRTheavy
    SBheavy[SBheavy<0] <- 0
  SBbus <- mcounts$d4bus
    SBbus[SBbus<0] <- 0
  SBRT <- mcounts$SBRT+mcounts$SBRTheavy
    SBRT[SBRT<0] <- 0
  SBRTheavy <- mcounts$SBRTheavy
    SBRTheavy[SBRTheavy<0] <- 0
  SBLT <- mcounts$d8+mcounts$d8heavy
    SBLT[SBLT<0] <- 0
  SBLTheavy <- mcounts$d8heavy
    SBLT[SBLT<0] <- 0
  totalVol <- WB+WBRT+WBLT+EB+EBRT+EBLT+NB+NBRT+NBLT+SB+SBRT+SBLT
  totalVol.withoutsometurns <- WB+WBRT+WBLT+EB+EBLT+NB+NBRT+SB+SBRT  #to try to match with scats data, since scats may be missing some detectors
  WBspillback <- mcounts$spillback_westbound
  mvol <- data.frame(WB,WBRT,WBLT,EB,EBRT,EBLT,NB,NBRT,NBLT,SB,SBRT,SBLT,
                     WBheavy,WBRTheavy,WBLTheavy,EBheavy,EBRTheavy,EBLTheavy,NBheavy,NBRTheavy,NBLTheavy,SBheavy,SBRTheavy,SBLTheavy,
                     WBbus,EBbus,NBbus,SBbus,
                     totalVol,totalVol.withoutsometurns,
                     WBspillback)
  mvol$date <- mcounts$Time

  return(mvol)
  }
