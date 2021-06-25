 #'#*******************************************************************************************************************
#'@title : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2020-06-25
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  This script format and aggregate NFOB data 
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


######## 2. BUILDING FOB DENSITY: Nfob (p)------------------------------------------------------------------------------------------------
cat(crayon::italic("\n\t+ 1.2. Building Buoy density\n"))

### Generating buoy density data from raw buoys location (if it doesn't exist)
formatted_buoyDens_fn <- file.path(Nfob_OUTPUTS, "dailyBuoyDensityData.csv")
if(COMPUTE_BUOYDENSITY==T | !file.exists(formatted_buoyDens_fn))
{
  cat(crayon::green("\t    -  Computing buoy location data from:", crayon::blue$underline(Nfob_DATA_DIR), "\n"))
  
  ### 2.1. Get buoy density data (for all years and brand in the data folder)
  buoysDens_fn <- list.dirs(path = Nfob_DATA_DIR, full.names = T, recursive = F)
  if(length(buoysDens_fn)==0){
    stop("Check parameter: <", crayon::bold$red("Nfob_DATA_DIR"), ">\n",  
         "\t-Echosounder data not found in the defined folder: ", crayon::underline$white(Nfob_DATA_DIR))
  }
  
  ### Loading data
  require(foreach)
  buoyDens <- foreach(i = 1:length(buoysDens_fn), .combine = plyr::rbind.fill)%do%
  {
    fl <- buoysDens_fn[i]
    dens <- NULL
    year <- as.numeric(basename(fl))
    if(is.na(year))
    {
      cat(crayon::silver("\t         -  Skipping folder (Folders to be processed should be name with an integer representing the year): "), crayon::cyan$underline(fl), "\n")
    }else if(!year %in% YEARS)
    {
      cat(crayon::silver("\t         -  Skipping folder (Not required year): "), crayon::cyan$underline(fl), "\n")
    }else
    {
      ### 1. Check if consolidated files are present in folder
      fl_n <- list.files(path = fl, pattern = "buoysDataLocation.RData", full.names = T, recursive = F)
      if(length(fl_n)==1)
      {
        cat(crayon::yellow("\t         -  Loading and Processing: ", crayon::cyan$underline(fl_n), "..."))
        load(fl_n)
		    suppressWarnings(try(rm(buoysDataLocationFilterSummary), silent = T))
        dens <- get_dailyBuoyCount(buoysDataLocation,
                                   spatialScale = SPATIAL_SCALE,
                                   timeScale =TIME_SCALE)
        rm(buoysDataLocation)
        cat("Done.\n")
      }else
      {
        fls <- list.files(path = fl, pattern = "buoys_data_location_filtered.RData", full.names = T, recursive = T)
        k <- 0
        cat(crayon::yellow("\t         -  Loading and Processing: "), crayon::cyan$underline(fl), "\n")
        
        ### Init parallel backend if required
        if(parallel)
        {
          require(doParallel)
          require(parallel)
          clus <- parallel::makeCluster(10)
          doParallel::registerDoParallel(cl = clus)
        }else
        {
          registerDoSEQ()
        }
        
        ### Calculating buoy density
        dens <- foreach(fli = fls, .combine = plyr::rbind.fill, .verbose = T, .export = "countBuoys")%dopar%
        {
          progress <- paste0(k,"/",length(fls))
          cat("\r  + Buoy folder:", basename(dirname(fli)), "|", progress)
          k <- k+1
          
          ### Loading data
          load(fli); rm(buoysDataLocationFilterSummary)
          curdens <- NULL
          if(nrow(buoysDataLocation)>0){
            curdens <- get_dailyBuoyCount(buoysDataLocation,
                                          spatialScale = SPATIAL_SCALE,
                                          timeScale =TIME_SCALE)
          }
          rm(buoysDataLocation)
          curdens
        }
        
        ###  Close parallel backend if initialized
        if(parallel)
        {
          stopCluster(clus)
        }
        colNums <- 1:ncol(dens)
        groupColNum <- which(c("year", "zone", "x", "y", "date", "timescale") %in% colnames(dens))
        sumColNum   <- setdiff(colNums, groupColNum)
        dens <- plyr::ddply(.data = dens, .variables = plyr::.(year, zone, x, y, date, timescale), 
                            .fun = function(tab)
                            {
                              res <- apply(X = tab[,sumColNum], MARGIN = 2, FUN = sum, na.rm=T)
                              return(res)
                            })
        cat(" ...Done.\n")
      }
    }
    dens
  }
  # return Data
  write.csv2(buoyDens, file = formatted_buoyDens_fn, row.names = F)
}

###  Reading and filtering buoy density data
cat(crayon::green("\t    -  Loading buoy location data from:", crayon::blue$underline(formatted_buoyDens_fn), "..."))
buoyDens <- read.csv2(file = formatted_buoyDens_fn, stringsAsFactors = F)%>%
  dplyr::filter(zone != "OFF_ZONE",
                zone_delimitation(lon = x, lat = y, zones = as.numeric(as.character(zone)), 
                                  boundingBox = BOUNDING_BOX))
cat("Done.\n")

###  2.2. Aggregate density data within the defined spatio-temporal unit
cat(crayon::green("\t    -  Aggregating Buoy density on the spatio-temporal unit..."))
buoyDensData <- buoyDens%>%
  dplyr::group_by(year, timescale, zone, x, y)%>%
  dplyr::summarise(dailyAvgDens = mean(buoyCount, na.rm=T),
                   sd_dens = sd(buoyCount, na.rm=T),
                   se_dens = sd(buoyCount, na.rm=T)/sqrt(n()),
                   avgMSI = mean(MSI, na.rm=T),
                   avgM3I = mean(M3I, na.rm=T),
                   avgM4I = mean(M4I, na.rm=T),
                   avgM3p = mean(`M3.`, na.rm=T),.groups="keep")
buoyDens_fn <- file.path(Nfob_OUTPUTS, "aggregated_dailyBuoyDensityData.csv")
write.csv2(buoyDensData, file =  buoyDens_fn, row.names = F)
cat("Done.\n")

