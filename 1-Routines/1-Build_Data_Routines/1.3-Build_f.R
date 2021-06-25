#'#*******************************************************************************************************************
#'@title  : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2020-06-25
#'@email  : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  This script format and aggregate F and aCART data 
#'#*******************************************************************************************************************
#'@revision
#'    [2021-04-29] 
#'    1- Suppressing Fad trajectories cleaning (isolated points) and ACAT/ACRT computations. (f1 is calculated from 
#'    baseDataFad)
#'    2- (L68) Applying a threshold on the number of days (30) used to compute the quaterly average of f1
#'    3- (L126) Applying threshold on the number of set used to compute the species occurence.
#'
#'#*******************************************************************************************************************


######## 3. BUILDING FAD AGGREGATIVE METRICS (f and acART)------------------------------------------------------------
cat(crayon::italic("\n\t+ 1.3. Building <f> FAD aggregative metrics\n"))

### 3.1. Get presence/absence data (for all years in the data folder)
fadData_fn <- list.files(path = f1_DATA_DIR, pattern = "predicted_BuoysDataEcho.RData", full.names = T, recursive = T)
if(length(fadData_fn)==0){
  stop("Check parameter: <", crayon::bold$red("f1_DATA_DIR"), ">\n",  
       "\t-Echosounder data not found in the defined folder: ", crayon::underline$white(f1_DATA_DIR))
}

###  Loading data
cat(crayon::green("\t    -  Loading tuna presence/absence data from:", crayon::blue$underline(f1_DATA_DIR), "\n"))
baseFadData <- foreach(fn =fadData_fn, .combine = rbind)%do%
{
  cat("\t         -  Loading and Processing: ", crayon::cyan$underline(fn), "...")
  ###   Loading fn
  load(fn)
  
  ###   Spatialize presence/absence data
  suppressWarnings(spat_echo <- spatialize(longitude = buoysDataEcho$longitude,
                          latitude = buoysDataEcho$latitude,
                          resolution = SPATIAL_SCALE,#, spatialRes ="D:/THESE_scripts/0-Resources/Maps/LonghurstArea")
                          field = "region"))
  buoysDataEcho <- cbind.data.frame(buoysDataEcho, spat_echo[, c("x", "y", "zone")])
  rm(spat_echo)
  # Apply Time scaling
  buoysDataEcho$timescale <- timescaling(timestamp = buoysDataEcho$sensor_date,
                                         resolution = TIME_SCALE)
  cat("Done.\n")
  buoysDataEcho
}
rm(buoysDataEcho)

### Bounding zone and casting zone type 
baseFadData <- baseFadData%>%
  dplyr::mutate(zone = forcats::fct_explicit_na(as.factor(zone), "OFF_ZONE"))%>%
  dplyr::filter(zone_delimitation(lon = x, lat = y, zones = as.numeric(as.character(zone)), 
                                  boundingBox = BOUNDING_BOX))
write.csv2(baseFadData, file =  file.path(f1_OUTPUTS, "presAbsData.csv"), row.names = F)
cat("Done.\n")


### 3.2. Truncation type I: Identification chronology (sections) of CART succession on trajectories 
if(TRUNCATE_START_SECTIONS_FOR_F_COMPUTATIONS)
{
  cat(crayon::green("\t    -  Truncate start sections on trajectories (regardless of space-time units)..."))
  baseFadData <- baseFadData%>%
        dplyr::mutate(year = lubridate::year(sensor_date))%>%
        dplyr::filter(zone != "OFF_ZONE")%>%
        dplyr::group_by(buoy_id, trajId)%>%
        dplyr::arrange(sensor_date, .by_group = T)%>%
        dplyr::mutate(tunaPresence = ifelse(tunaPresence.chk =="Tunas", 1, 0),
                      soakTime = n())%>%
        dplyr::mutate(chrono = cumsum(ifelse(c(0,diff(tunaPresence))==0, 0, 1)))%>%
        dplyr::filter(chrono != 0)
  write.csv2(baseFadData, 
             file =  file.path(f1_OUTPUTS, "presAbsData_truncated.csv"), row.names = F)
  cat("Done.\n")
}


### 3.3. Aggregate f1 by zone and timescale 
cat(crayon::green("\t    -  Aggregating <f> over the spatio-temporal unit..."))
f1Data  <- baseFadData%>%
  # Compute daily proportion of inhabited FADs (prop_inh) in the area
  dplyr::group_by(year=lubridate::year(sensor_date), timescale, zone,  x, y, sensor_date)%>%
  dplyr::summarise(buoy_count = length(unique(buoy_id)),
                   prop_inh   = sum(tunaPresence.chk=="Tunas")/length(tunaPresence.chk),
                   .groups="keep")%>%
  # Filtering days with less than a threshold number of buoys in the time area
  dplyr::filter(buoy_count >= BUOYS_COUNT_THRESHOLD)%>% 
  # Computing average proportion of inhabited FADS over the time-area
  dplyr::group_by(year, timescale, zone, x, y)%>%
  dplyr::summarise(avg_f1 = mean(prop_inh, na.rm=T),
                   sd_f1  = sd(prop_inh, na.rm=T),
                   se_f1  = sd(prop_inh, na.rm=T)/sqrt(n()),
                   buoyCount = mean(buoy_count),
                   #'@upd: 2021-04-29
                   day_count = n(),
                   .groups="keep")%>%
  # Filtering average values computed with less than a threshold number of days 
  dplyr::filter(day_count >= MIN_NUMBER_DAY_FOR_COMPUTE_F_AVERAGE)
cat("Done.\n")



######## 3.4. Correcting <f>
#'@note:    f is actually the proportion of FOBs inhabited
#'       by tunas estimated from buoys, with unfortunately
#'       the impossibility to provide species discrimina-
#'       tion. As we need to estimated rather the propor-
#'       tion of FOBs inhabited by the species of interest
#'       , a correction is here performed on f using the 
#'       species occurence under FAD, calcultaed from T3-
#'       Sampling data.

### Loading T3-L2 Sampling data (same procedure as 1.1-Build_m.R)
cat(crayon::green("\t    -  Loading T3 sampling data...."))
samplingData   <- read.csv2(file = T3_SAMPLING_DATAFILE, stringsAsFactors = F)%>%
  # Uniformize colnames for the script
  dplyr::rename(latitude      = lat,
                longitude     = lon,
                activity_date = date_act, 
                set_id        = id_act, 
                catches       = sample_catch_ST,
                species       = sp,
                null_set      = code_act_type)%>%
  # Casting and formatting columns
  dplyr::mutate(year = lubridate::year(activity_date),
                timescale =  timescaling(timestamp = as.Date(activity_date), resolution = TIME_SCALE),
                fishing_mode = case_when(fishing_mode ==1 ~ "fob",
                                         fishing_mode ==2 ~ "fsc",
                                         fishing_mode ==3 ~ "unk"),
                null_set     = ifelse(null_set==0, TRUE, FALSE),
                weight_category = ifelse(species=="SKJ", NA, 
                                         case_when(weight_category=="m10" ~ "-10kg",
                                                   weight_category=="p10" ~ "+10kg")))%>%
  # Discarding null sets and other species (minor tunas)
  dplyr::filter(null_set == F,  
                species %in% c("YFT", "BET", "SKJ"))%>%
  # Compute total catch and species composition in each sampled set
  dplyr::group_by(set_id)%>%
  dplyr::mutate(total_catches = sum(catches))%>%
  dplyr::mutate(sp_prop       = catches/total_catches)
cat("Done.\n")

#### Apply the spatio-temporal stratification over the sampling data:
cat(crayon::green("\t    -  Apply spatio-temporal stratification on sampling data...."))
suppressWarnings(spat <- spatialize(longitude  = samplingData$longitude,
                                    latitude   = samplingData$latitude,
                                    resolution = SPATIAL_SCALE))
samplingData <- cbind.data.frame(samplingData, spat[, c("x", "y", "zone")])
rm(spat)

### Bounding zone and casting zone type 
samplingData <- samplingData %>%
  dplyr::mutate(zone = forcats::fct_explicit_na(as.factor(zone), "OFF_ZONE"))%>%
  dplyr::filter(zone_delimitation(lon = x, lat = y, zones = as.numeric(as.character(zone)), 
                                  boundingBox = BOUNDING_BOX))
# Saving data
T3_sampling_fn <- file.path(RESOURCES_DIR, "T3_Sampling_data.csv")
write.csv2(samplingData, file = T3_sampling_fn, row.names = F)
cat("Done.\n")


#### Calculating species occurence in each space-time stratum
cat(crayon::green("\t    -  Computing species occurence in FAD fishing sets from T3 sampling data...."))
species_occurence <- as.data.frame(samplingData %>%
  # Compute total number of sets in each time area units
  dplyr::group_by(year, zone, x, y, timescale, fishing_mode)%>%
  dplyr::mutate(n_set  = length(unique(set_id)))%>%
  # Discarding time-areas with catches under occurence threshold
  dplyr::filter(zone != "OFF_ZONE",
                fishing_mode == "fob",
                catches >= BIOMASS_OCCURENCE_THRESHOLD)%>%
  # Retrieve time-area units with insufficient number of sets
  dplyr::filter(n_set >= SET_COUNT_THRESHOLD_FOR_M_and_F_CORRECTION)%>%
  # Compute occurence for each species and size class
  dplyr::group_by(year, zone, x, y, timescale, n_set, species, weight_category)%>%
  dplyr::summarise(occurence = n()/unique(n_set),
                   .groups   = "keep"))
write.csv2(x = species_occurence, file = file.path(f1_OUTPUTS, "species_occurence.csv"), row.names = F)

 
#### Correcting proportion of inhabited FOBs <f>
cat(crayon::green("\t    -  Correcting <f> with the species occurence from T3 sampling data...."))
species_occurence  <- species_occurence %>%
        dplyr::filter(species == SPECIES ,
                     (is.na(weight_category) | weight_category == SIZE_CLASS))%>%
        dplyr::select(year, zone, x, y, timescale, occurence, n_set)
f1Data <- merge.data.frame(x = f1Data, 
                           y = species_occurence, 
                           all.x =FIll_IN_MISSING_DATA)


### Missing values processing ---------------------------------------------------------------------------
#'@Note
#'--------------------------------------------
#'    Replacing  missing specie occurence  for
#'    a given space-time units with the  quar-
#'    terly average value of species occurence 
#'    calculated over the whole study area.
#'--------------------------------------------

if(FIll_IN_MISSING_DATA)
{
  #### Computing quarterly average of species composition over the study area
  cat(crayon::green("\t\t    +  Averaging species occurence over the study area for missing  values...."))
  
  timescale_aggr_occurence <- as.data.frame(samplingData %>%
                          # Compute the total number of sets per strata and fishing mode
                          dplyr::group_by(x, y, timescale, fishing_mode)%>%
                          dplyr::mutate(n_set  = length(unique(set_id)))%>%
                          # Discarding data out of study area and sets with catches under occurence threshold
                          dplyr::filter(zone != "OFF_ZONE",
                                        fishing_mode == "fob",
                                        catches >= BIOMASS_OCCURENCE_THRESHOLD)%>%
                          # Retrieve stratum with insufficient number of sets for deriving occurence
                          dplyr::filter(n_set >= SET_COUNT_THRESHOLD_FOR_M_and_F_CORRECTION)%>%
                          # Compute the occurence for each species and size class
                          dplyr::group_by(x, y, timescale, n_set, species, weight_category)%>%
                          dplyr::summarise(occurence = n()/unique(n_set),
                                           .groups   = "keep")%>%
                          # keep only occurence for the selected species and size class
                          dplyr::filter(species == SPECIES ,
                                        (is.na(weight_category) | weight_category == SIZE_CLASS)))
  write.csv2(x = timescale_aggr_occurence, file = file.path(f1_OUTPUTS, "time_aggregated_species_occurence.csv"), row.names = F)
  cat("Done.\n")  
  
  
  #### Substituting missing values by the quarterly average of species compotion over the study area
  cat(crayon::green("\t\t    +  Substituting missing occurence values by their quarterly average ...."))
  f1Data <- merge.data.frame(x=f1Data, y = timescale_aggr_occurence, 
                             by =c("x", "y", "timescale"),
                             all.x = T, suffixes = c("","_tsAggr"))%>%
    dplyr::mutate(occurence  = ifelse(is.na(occurence), occurence_tsAggr, occurence),
                  n_set      = ifelse(is.na(n_set), n_set_tsAggr, n_set))
  cat("Done.\n")
}

# Correcting f data
f1Data <- f1Data%>%
  dplyr::mutate(avg_f1_uncorrected = avg_f1, 
                avg_f1 = avg_f1_uncorrected * occurence,
                # Corrected sd (NB. occurence over time area units is a cste value)  
                sd_f1_uncorrected = sd_f1,
                sd_f1 = sd_f1_uncorrected * occurence)

### Saving fad data
aggrFadData_fn <- file.path(f1_OUTPUTS, "aggregated_fadData.csv")
f1Data <- subset(f1Data, year %in%YEARS)
write.csv2(f1Data, file =  aggrFadData_fn , row.names = F)