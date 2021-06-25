#'#*******************************************************************************************************************
#'@title : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2020-06-25
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  This script generates time series of abundance from the ABBI methodology
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

rm(list = ls()[!ls() %in% get("DO_NOT_DELETE")])
invisible(gc())


### Functions and libraries loading
cat(crayon::italic("\n\t+ 3.0. Initialize resources for the analysis:\n"))

### Loading functions
require(plyr)
require(dplyr)
require(ggplot2)
require(ggsn)
require(zoo)
require(foreach)
cat(crayon::green("\t    -  Loading functions from:", crayon::blue$underline(FUNCTIONS_DIR), "\n"))
invisible(lapply(list.files(path = FUNCTIONS_DIR, pattern = "*.R", full.names = T, recursive = F),
                 function(x){
                   cat("\t         - Loading: ", basename(x), "\n")
                   source(x, echo = F)
                 }))


# ## Check and load resources files --------------------------------------------------------------------------
### CRT values
CRT_FILE <- file.path(RESOURCES_DIR, "CRT_values.csv")
cat(crayon::green("\t    -  Loading CRT values from:", crayon::blue$underline(CRT_FILE), "..."))
if(file.exists(CRT_FILE)){
  CRTs_df <- read.csv2(CRT_FILE) 
  CRT_THEO <- subset(CRTs_df, ocean==OCEAN & species==SPECIES)$CRT
}else{
  stop("File for CRTs values not found at :\n")
  cat("\t\t", crayon::blue$underline(CRT_FILE))
}
cat("Done.\n")




######  0. Loading joint aggregated catch and FAD data (by space-time units)base data.frame-------------------------------------------
cat(crayon::italic("\n\t+ 2. Computing model inputs parameters\n"))

### Loading joint aggregated catch and DFAD data-------------------------------------------
cat(crayon::green("\t    -  Loading joint aggregated catch and DFAD data (FOB_data)..."))
fob_DATA <- read.csv2(file.path(OUTPUTS_DIR, "aggregated_catchFadData.csv"))%>%
  dplyr::filter(year %in% YEARS, 
                fishing_mode =="fob", 
                species %in% SPECIES,
                (is.na(weight_category) | weight_category == SIZE_CLASS))%>%
  dplyr::mutate(avgM3I = ifelse(is.na(avgM3I), 0, avgM3I),
                avgM3p = ifelse(is.na(avgM3p), 0, avgM3p))%>%
  dplyr::rename(catches = avg_catches, 
                f1 = avg_f1, 
                buoyDensity = dailyAvgDens,
                buoyDensityM3I = avgM3I, 
                buoyDensityM3p = avgM3p)%>%
  dplyr::mutate(timestamp = dplyr::case_when(
    toupper(TIME_SCALE) == "QUARTER" ~ as.Date(as.POSIXct(zoo::as.yearqtr(paste0(year, "-Q", timescale),format = "%Y-Q%q"))),
    toupper(TIME_SCALE) == "MONTH"   ~ as.Date((as.POSIXct(zoo::as.yearmon(paste0("01", timescale, year),format = "%d%B%Y"))))))
cat("Done.\n")


#### 1. Time series of FOBS---------------------------------------------------------------------------------------
# 1.1. Estimate number of FOBS (Nfob) from french active buoys----------------------------------------------------------
#### 1.1.1. Estimate total FAD number (from raising factors from Katara & al 2018)-------------------------------
cat(crayon::green("\t    -  Computing time series of FOB density <p>..."))
      
#'@note: *FR_prop = FAD_FR / NFAD**
#'#'----------------------------------------------------------------
#' Fig 19 p17 of Katara and al 2018
#'------------------------------------------------------------------
cat(crayon::yellow("\t          - Loading French/Spanish DFAD ratio (Katara et al, 2018)..."))

#'@note: 
#'--------------------------------------------------------------#
#'      Katara data are aggregated by month from 2010 to 2017
#'  daily time scale will use values over  the corresponding 
#'  month. Years > 2017 will use average 2017 value.
#'--------------------------------------------------------------#
Sys.setlocale("LC_TIME", "English")

katara_data <- read.csv(file = file.path(RESOURCES_DIR, "cpue_workshop_Katara_2018.csv"))%>%
 dplyr::rename(french_prop=frac.french)%>%
 dplyr::mutate(year=lubridate::year(as.Date(date)))%>%
 dplyr::filter(year >= min(YEARS))%>%
 dplyr::mutate(timescale = dplyr::case_when(
                 #toupper(TIME_SCALE) == "MONTH" ~ months.date(as.Date(date)),
                 toupper(TIME_SCALE) == "QUARTER" ~ as.numeric(substr(quarters.Date(as.Date(date)), 2,2)),
                 toupper(TIME_SCALE) == "MONTH"   ~ lubridate::month(as.Date(date))),
               timestamp =  dplyr::case_when(
                  toupper(TIME_SCALE) == "QUARTER" ~ as.Date(as.POSIXct(zoo::as.yearqtr(paste0(year, "-", timescale),format = "%Y-%q"))),
                  toupper(TIME_SCALE) == "MONTH"   ~ as.Date(as.POSIXct(zoo::as.yearmon(paste0("01/", timescale,"/", year),format = "%d/%m/%Y")))))
write.csv2(katara_data, file = file.path(Nfob_OUTPUTS, "katara_data.csv"), row.names = F)
cat("Done.\n")


#### Compute raising factor per year and timescale
#' Raising : *NFAD = (1/ FR_prop).FAD_fr*
katara_data <- katara_data%>%
 dplyr::group_by(year, timescale)%>%
 dplyr::summarise(avg_french_prop = mean(french_prop, na.rm=T), .groups="keep")%>%
 dplyr::mutate(raisingFad = 1/avg_french_prop)

####  Assessing raising fad
fob_DATA  <- merge.data.frame(x=fob_DATA, y = katara_data[,c("year", "timescale", "raisingFad")], all.x=T)

   
####  Providing raising factor for year higher than 2017 (assumed constant raising after 2017)
raisingFad2018 <- mean(subset(katara_data, year==2017)$raisingFad, na.rm=T)
fob_DATA$raisingFad[fob_DATA$year>=2018] <- raisingFad2018

##### Estimating Total number of DFADs and uncertainty:
#'   *Nse=fad = raising. se=FAD_FR*
cat(crayon::yellow("\t          - Estimating total number of DFADs..."))
fob_DATA <- fob_DATA%>%
  dplyr::mutate(NFad = raisingFad*buoyDensity,
                se_NFad = raisingFad*se_dens)
cat("Done.\n")

#### 1.1.2. Estimate logs number (Nlog - from raising factors from Amael work 2019)-----------------------------------------------------------
#'@note:  *NLog = (logs_prop/(1-logs_prop)).NFAD*
#'        *raisingLogs = logs_prop/(1-logs_prop)*
#'-------------------------------------------------------------------
#' From Dupaix work (2020): proportions of DFADs vs Logs observed by
#' on-board observers on purse seiners (FRench fleet)
#'-------------------------------------------------------------------
cat(crayon::yellow("\t          - Estimating logs raising factors..."))
logFad_prop <- read.csv2(file = file.path(RESOURCES_DIR, "DFAD-LOG_proportions_Dupaix2020.csv"))%>%
        dplyr::filter(year %in% YEARS)%>%
        dplyr::select(year, Logs, DFADs)%>%
        dplyr::mutate(raisingLog = Logs/(100-Logs))
write.csv2(logFad_prop, file = file.path(Nfob_OUTPUTS, "logFad_prop"), row.names = F)

# Assign yearly values to fob_data     
fob_DATA$raisingLog    <- logFad_prop$raisingLog[match(fob_DATA$year, logFad_prop$year)]
fob_DATA <- fob_DATA%>%
  dplyr::mutate(NLog = NFad*raisingLog,
                se_NLog = se_NFad*raisingLog)
cat("Done.\n")


#### 1.1.3. Estimate total FOB number (NFob) and associated uncertainty-------------------------------------------------
#'@note:   *NFOB = NFAD + NLog*  
#'#'    *se_NFOB =  se_NFAD + se_NLog*
#'-------------------------------
cat(crayon::yellow("\t          -  Estimating total number of FOBs..."))
fob_DATA <- fob_DATA%>%
  dplyr::mutate(NFob = NFad + NLog,
                se_NFob = se_NFad + se_NLog)

#### Saving Model inputs
write.csv2(x = fob_DATA, file=file.path(OUTPUTS_DIR, "modelInputs_timeSeries.csv"), row.names = F)
cat("Done.\n")



# 2. Time series of CAT and Abundance ----------------------------------------------------------------
#### 2.1 Absolute abundance
cat(crayon::yellow("\t          -  Computing time series of CAT and absolute species abundance.\n"))

#'@notes------------------------------
#' *Nfree = m.f/(phi.CRT)*
#' *Nassoc = m.f.p*
#' *N = Nassoc + Nfree*
#'------------------------------------
get_abundance <- function(m, f, p, CRT, phi)
{
  CAT <- 1/(phi*p) 
  Nassoc <- m*f*p
  Nfree  <- (m*f)/(phi*CRT)
  
  total_biomass <- m*f*p*((CAT+CRT)/CRT)
  return(data.frame(CAT =CAT, total_biomass=total_biomass, Nfree=Nfree, Nassoc=Nassoc))
}

#'@note:
#' a cell corresponding to S (the estimated surface) of 5° is about 313 600 km² 
#' S0 (local interaction zone) is about 6000 km²
#' thus S0/S is around 0.02 and mu(association probability) is around 0.004
#PHI <- 8e-5
PHI <- signif((6000/(SPATIAL_SCALE*111)^2) * c(0.004, 0.0065, 0.01, 0.04, 0.065, 0.1), digits = 1)
mostPlausible_PHI <- min(PHI)


# Sensitivity of Ntotal and Nfree to phi---------------------------------------------------------
estimatedTimeseries <- foreach(phi_i = PHI, .combine=rbind)%do%
{
  dot <- dplyr::select(fob_DATA, zone, x, y, year, timescale, timestamp, catches, se_catches, f1, se_f1, NFob, se_NFob)
  dot$phi <- phi_i
  #dot$CRT <- CRT_THEO
  dot_crt <- foreach(crt_i = c(3, CRT_THEO, 15, 30), .combine=rbind)%do%
  {
    dot$CRT <- crt_i
    cbind.data.frame(dot, get_abundance(m = dot$catches, f = dot$f1, p = dot$NFob, CRT = dot$CRT, phi = dot$phi))
  }
}


if(toupper(TIME_SCALE)=="QUARTER"){
  estimatedTimeseries <- estimatedTimeseries%>%
    dplyr::mutate(timescale =  paste("Quarter", timescale))
}
estimatedTimeseries <- estimatedTimeseries%>%
  dplyr::mutate(se_Nassoc = Nassoc * ((se_catches/catches) + (se_f1/f1) + (se_NFob/NFob)),
                se_Nfree = Nfree*((se_catches/catches) + (se_f1/f1)),
                se_total_biomass = se_Nfree + se_Nassoc,
                phi = as.numeric(format((phi), scientific = T, digits = 1)))

cat("Done.\n")


#### 2.2 Relative abundance----------------------------------------------------------------------
cat(crayon::yellow("\t          -   Computing time series of relative abundance..."))
estimatedTimeseries <- as.data.frame(estimatedTimeseries%>%
                                       dplyr::group_by(zone, phi)%>%
                                       dplyr::mutate(Ntot_ref     = unique(total_biomass[which.min(timestamp)]), 
                                                     se_Ntot_ref = unique(se_total_biomass[which.min(timestamp)]), 
                                                     
                                                     Nfree_ref    = unique(Nfree[which.min(timestamp)]), 
                                                     se_Nfree_ref = unique(se_Nfree[which.min(timestamp)]), 
                                                     
                                                     Nassoc_ref    = unique(Nassoc[which.min(timestamp)]),
                                                     se_Nassoc_ref = unique(se_Nassoc[which.min(timestamp)]))%>%
                                       
                                       dplyr::mutate(Itotal   = total_biomass/Ntot_ref,
                                                     Ifree    = Nfree/Nfree_ref,
                                                     Iassoc   = Nassoc/Nassoc_ref)%>%
                                       
                                       dplyr::mutate(se_Itotal = Itotal*((se_total_biomass/total_biomass)+(se_Ntot_ref/Ntot_ref)),
                                                     se_Ifree = Ifree*((se_Nfree/Nfree)+(se_Nfree_ref/Nfree_ref)),
                                                     se_Iassoc = Iassoc*((se_Nassoc/Nassoc)+(se_Nassoc_ref/Nassoc_ref))))

write.csv2(x = estimatedTimeseries, file = file.path(OUTPUTS_DIR, "abundance_estimates.csv"), row.names = F)
cat("Done.\n")


##### 2.3. Average abundance calculated from the different strata---------------------------------------------
cat(crayon::yellow("\t          -   Computing average abundance from the different strata..."))
if(!REF_TIME %in% unique(estimatedTimeseries$timestamp)){
  REF_TIME <- min(estimatedTimeseries$timestamp)
  
  beepr::beep(2)
  warnings(crayon::red$bold("Reference value of relative abundance estimates changed to :", REF_TIME))
}
globalAbundance_avg <- as.data.frame(estimatedTimeseries %>%
                                       dplyr::group_by(timestamp, phi, CRT)%>%
                                       dplyr::summarise(total_biomass.sd = sd(total_biomass, na.rm=T),
                                                        Nassoc.sd        = sd(Nassoc, na.rm=T),
                                                        Nfree.sd         = sd(Nfree, na.rm=T),
                                                        
                                                        total_biomass = mean(total_biomass, na.rm=T),
                                                        Nassoc        = mean(Nassoc, na.rm=T),
                                                        Nfree         = mean(Nfree, na.rm=T),
                                                        
                                                        dataPoints    = n(),
                                                        .groups="keep")%>%
                                       dplyr::group_by(phi, CRT)%>%
                                       dplyr::mutate(Itotal = total_biomass/unique(total_biomass[timestamp==REF_TIME]),
                                                     Itotal.sd = total_biomass.sd/unique(total_biomass[timestamp==REF_TIME]),
                                                     
                                                     Iassoc = Nassoc/unique(Nassoc[timestamp==REF_TIME]),
                                                     Iassoc.sd = Nassoc.sd/unique(Nassoc[timestamp==REF_TIME]),
                                                     
                                                     Ifree = Nfree/unique(Nfree[timestamp==REF_TIME]),
                                                     Ifree.sd = Nfree.sd/unique(Nfree[timestamp==REF_TIME])))
write.csv2(globalAbundance_avg, file = file.path(OUTPUTS_DIR, "globalAbundanceAvg.csv"), row.names = F)
cat("Done.\n")
