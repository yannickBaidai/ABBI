#'#*******************************************************************************************************************
#'@title : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2020-06-25
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  This script format and aggregate raw data 
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

rm(list = ls()[!ls() %in% get("DO_NOT_DELETE")])
invisible(gc())


### Functions and libraries loading
library(plyr)
library(dplyr)
library(foreach)
library(ggplot2)
parallel <- TRUE
cat(crayon::italic("\n\t+ 1.0. Loading functions from:"), crayon::underline$blue(FUNCTIONS_DIR), "\n")
invisible(lapply(list.files(path = FUNCTIONS_DIR, pattern = "*.R", full.names = T, recursive = F),
                 function(x){
                   cat("\t    - Loading: ", basename(x), "\n")
                   source(x, echo = F)
                 }))

### Label for graphs
time_lbl  <- tolower(TIME_SCALE)
space_lbl <- ifelse(is.numeric(SPATIAL_SCALE), 
                    paste0("per ", SPATIAL_SCALE, "Deg. square"),
                    paste("in", basename(SPATIAL_SCALE)))

### 1. Build m (Catches)
source(file.path(BUILDDATA_ROUTINES, "1.1-Build_m.R"))

### 2. Build p (Nfob)
source(file.path(BUILDDATA_ROUTINES, "1.2-Build_buoyDensity.R"))

### 3. Build f and acART
source(file.path(BUILDDATA_ROUTINES, "1.3-Build_f.R"))

### 4. Merging inputs data in a single dataframe
	# 4.1. Merge Buoy density and Fad agregative metrics
	cat(crayon::green("\t    -  Merging Buoy density with proportion of inhabited fOBS..."))
	f1Data       <- read.csv2(file = file.path(f1_OUTPUTS,   "aggregated_fadData.csv"), stringsAsFactors =F)
	buoyDensData <- read.csv2(file = file.path(Nfob_OUTPUTS, "aggregated_dailyBuoyDensityData.csv"), stringsAsFactors =F)
	DATA <- merge.data.frame(x=f1Data, y = buoyDensData)
	cat("Done.\n")

	# 4.2. Merge data with catches-------------------------------------------------------------------------
	cat(crayon::green("\t    -  Merging catches with FAD data..."))
	catchData <- read.csv2(file = file.path(m_OUTPUTS, "aggregated_catchdata.csv"), stringsAsFactors =F)
	DATA <- merge.data.frame(x=DATA, y = catchData)
	cat("Done.\n")

  # Adding ocean and cleaning data off the studied zone
  DATA$ocean <- ifelse(DATA$x >= 20, "Indian", "Atlantic")
  DATA <- subset(DATA, zone != "OFF_ZONE")
  mergedata_fn <- file.path(OUTPUTS_DIR, "aggregated_catchFadData.csv")
  write.csv2(DATA, file =  mergedata_fn, row.names = F)
  cat("Done.\n")


### verbosity for final files
cat(crayon::cyan$bold("Output folders:\n"))
try(cat("\t   + Catches data       :",crayon::blue$italic$underline(catchdata_fn),"\n"), silent = T)
try(cat("\t   + Buoy Density data  :", crayon::blue$italic$underline(buoyDens_fn),"\n"), silent = T)
try(cat("\t   + Aggregative metrics data :",crayon::blue$italic$underline(aggrFadData_fn),"\n"), silent = T)
try(cat("\t   + Catch and Fad data       :",crayon::blue$italic$underline(mergedata_fn),"\n"), silent = T)

