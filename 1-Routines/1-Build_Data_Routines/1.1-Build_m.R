#'#*******************************************************************************************************************
#'@title : Associative Behaviour Based abundance Index for Tropical tuna 
#'@author : Yannick BAIDAI
#'@update : 2021-06-18
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  This script format correct and aggregate catches data 
#'#*******************************************************************************************************************


######## 1. BUILDING CATCHES: m ------------------------------------------------------------------------------------------------
######## 1.1. Formatting and spatializing logbook data
#'@notes:
#'@-----------------------
#'Group catches data :
#' the inputs data is the csv file from 
#' - per zone (<spatialRes> if integer data are mapped over a grid of resolution equals to the value of integer
#'                          if valid shapefile folder  data are mapped over the specified zones on the shapefile map
#' - and timeScale (<timeRes> character value among "Quarter" "Month" or "Year")
#' #'@-----------------------
cat(crayon::italic("\n\t+ 1.1. Building <m> from catches\n"))

# Applying defined spatio-temporal scale: 
if(!file.exists(m_DATA_DIR)){
  stop("Check parameter: <", crayon::bold$red("m_DATA_DIR"), ">\n",  
       "\t\t- Invalid path: ", crayon::underline$white(m_DATA_DIR))
}

### Loading data
cat(crayon::green("\t    -  Loading catches data from:", crayon::blue$underline(m_DATA_DIR), "..."))
catchData  <- read.csv2(m_DATA_DIR, stringsAsFactors = F)%>%
  # Uniformize colnames for the script
  dplyr::rename(latitude  = lat,
                longitude = lon,
                activity_date=date_act, 
                set_id  = id_act, 
                catches = lb_catch_ST,
                species = sp,
                null_set = code_act_type)%>%
  # Discard other species (minor tunas)
  dplyr::filter(species %in% c("YFT", "BET", "SKJ"))%>%
  # Cast and format columns
  dplyr::mutate(year = lubridate::year(activity_date),
                # Applying time stratifiaction according to parametr <TIME_SCALE>
                timescale = timescaling(timestamp = as.Date(activity_date),resolution = TIME_SCALE),
                fishing_mode = case_when(fishing_mode ==1 ~ "fob",
                                         fishing_mode ==2 ~ "fsc",
                                         fishing_mode ==3 ~ "unk"),
                null_set = ifelse(null_set==0, TRUE, FALSE),
                weight_category = ifelse(species=="SKJ", NA, 
                                         case_when(weight_category=="m10" ~ "-10kg",
                                                   weight_category=="p10" ~ "+10kg")))%>%
  # Compute sum of catch per sets
  dplyr::group_by(set_id)%>%
  dplyr::mutate(total_catches = sum(catches))
cat("Done.\n")

### Apply spatial stratification from args <TIME_SCALE> and <SPATIAL_SCALE>
cat(crayon::green("\t    -  Appplying spatio-temporal stratification...."))
suppressWarnings(spat_catch <- spatialize(longitude = catchData$longitude,
                                          latitude  = catchData$latitude,
                                          resolution = SPATIAL_SCALE,
                                          field = "region"))
catchData <- cbind.data.frame(catchData, spat_catch[, c("x", "y", "zone")])
rm(spat_catch)

### Bounding zone and casting zone type 
catchData <- catchData%>%
  dplyr::mutate(zone = forcats::fct_explicit_na(as.factor(zone), "OFF_ZONE"))%>%
  dplyr::filter(zone_delimitation(lon = x, lat = y, zones = as.numeric(as.character(zone)), 
                                  boundingBox = BOUNDING_BOX))
write.csv2(catchData, file = file.path(m_OUTPUTS, "catchData.csv"), row.names = F)
cat("Done.\n")



######## 1.2. Correcting <m>-------------------------------------------------
#'@note: 
#'---------------------------------------------------------------------------#
#'     Catches from logbook data has been standardized  (raised  using landing
#' notes), in order to correct data misreporting from skippers: T3-L2 process.
#' Since logbook data are also  affected by misestimation   of species catches
#' (SKJ generally  overestimated, while YFT  underestimated),  average species
#' composition calculated over the study period in the same area from sampling 
#' data are here used in order to correct the catch declarations.
#'---------------------------------------------------------------------------#     

### Loading T3-L2 Sampling data
cat(crayon::green("\t    -  Loading T3 sampling data (for <m> correction)...."))
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
  # Discarding null sets and other species than major tunas
  dplyr::filter(null_set == F,  
                species %in% c("YFT", "BET", "SKJ"))%>%
  # Compute total catch and species composition in each sampled set
  dplyr::group_by(set_id)%>%
  dplyr::mutate(total_catches = sum(catches))%>%
  dplyr::mutate(sp_prop       = catches/total_catches)
cat("Done.\n")

#### Apply the spatio-temporal stratification over the sampling data:
cat(crayon::green("\t    -  Applying spatio-temporal stratification on sampling data...."))
suppressWarnings(spat <- spatialize(longitude = samplingData$longitude,
                                    latitude  = samplingData$latitude,
                                    resolution = SPATIAL_SCALE))
samplingData <- cbind.data.frame(samplingData, spat[, c("x", "y", "zone")])
rm(spat)

### Bounding zone and casting zone type 
samplingData <- samplingData%>%
  dplyr::mutate(zone = forcats::fct_explicit_na(as.factor(zone), "OFF_ZONE"))%>%
  dplyr::filter(zone_delimitation(lon = x, lat = y, zones = as.numeric(as.character(zone)), 
                                  boundingBox = BOUNDING_BOX))

# Saving data
T3_sampling_fn <- file.path(RESOURCES_DIR, "T3_Sampling_data.csv")
write.csv2(samplingData, file = T3_sampling_fn, row.names = F)
cat("Done.\n")


#### Calculating species composition in each space-time stratum
cat(crayon::green("\t    -  Computing species composition from sampling data...."))
species_composition <- samplingData%>%
  # Discarding area out of selected strata
  dplyr::filter(zone != "OFF_ZONE")%>%  
  # Compute number of sampling sets per strata and fishing mode
  dplyr::group_by(year, timescale, zone, x, y,  fishing_mode)%>%
  dplyr::mutate(sample_count = length(unique(set_id)))%>%   
  # Discard strata with low number of samples
  dplyr::filter(sample_count >= SET_COUNT_THRESHOLD_FOR_M_and_F_CORRECTION)%>%
  # Compute average species composition per strataa and fishing mode
  dplyr::group_by(year, timescale, zone, x, y, fishing_mode, species, weight_category)%>%
  dplyr::summarise(sample_count  = unique(sample_count),
                   class_prop    = sum(sp_prop, na.rm=T)/sample_count, 
                   sp_prop_count = n(),
                   # Standard deviation computing: use proportion vector for each species while adding zero for missing values
                   class_sd      = sd(c(sp_prop,
                                        rep(0, (sample_count-sp_prop_count))),  
                                      na.rm = T),
                   .groups="keep")
write.csv2(x = species_composition, file = file.path(m_OUTPUTS, "species_composition.csv"), row.names = F)
cat("Done.\n")


#### Correcting catches <m> with species composition
cat(crayon::green("\t    -  Correcting <m> with the catch proportions by species and size classes from T3 sampling data....\n"))
catchData <- merge.data.frame(x = catchData, 
                              all.x= FIll_IN_MISSING_DATA,
                              y = species_composition)

### Missing values processing
#'@Note
#'------------------------------------------
#'    Replacing  missing species composition
#'    for  a  given   space-time  units  are
#'    replaced  with their quarterly average 
#'    value of species composition  computed 
#'    over the whole period in the same cell
#'------------------------------------------
if(FIll_IN_MISSING_DATA)
{
  #### Computing quarterly average of species composition over the study area
  cat(crayon::green("\t\t          +  Compute quarterly average of species composition over the study period in each area...."))
  species_composition_timeAggr <- samplingData%>%
    # Discarding area out of selected strata
    dplyr::filter(zone != "OFF_ZONE")%>%  
    # Compute number of sampling sets in each strata
    dplyr::group_by(x, y, timescale, fishing_mode)%>%
    dplyr::mutate(sample_count = length(unique(set_id)))%>%   
    # Discard strata with low number of samples
    dplyr::filter(sample_count >= SET_COUNT_THRESHOLD_FOR_M_and_F_CORRECTION) %>%
    # Compute species composition in each strata
    dplyr::group_by(x, y, timescale, fishing_mode, species, weight_category)%>%
    dplyr::summarise(sample_count  = unique(sample_count),
                     class_prop    = sum(sp_prop, na.rm=T)/sample_count, 
                     sp_prop_count = n(),
                     # Standard deviation computing: use proportion vector for each species while adding zero for missing values
                     class_sd      = sd(c(sp_prop,
                                          rep(0, (sample_count-sp_prop_count))),
                                        na.rm = T),
                     .groups="keep")
  write.csv2(x = species_composition_timeAggr, file = file.path(m_OUTPUTS, "time_aggregated_species_composition.csv"), row.names = F)
  cat("Done.\n")


  #### Substituting missing values by the quarterly average of species composition over the study area
  cat(crayon::green("\t\t          +  Substituting missing values of species composition by their quarterly average ...."))
  catchData <- merge.data.frame(x=catchData, y = species_composition_timeAggr, 
                                by =c("x", "y", "timescale", "fishing_mode", "species", "weight_category"),
                                all.x = T, suffixes = c("","_tsAggr"))%>%
    dplyr::mutate(class_prop    = ifelse(is.na(class_prop),    class_prop_tsAggr,    class_prop),
                  class_sd      = ifelse(is.na(class_sd),      class_sd_tsAggr,      class_sd),
                  sp_prop_count = ifelse(is.na(sp_prop_count), sp_prop_count_tsAggr, sp_prop_count),
                  sample_count  = ifelse(is.na(sample_count),  sample_count_tsAggr,  sample_count))
  cat("Done.\n")
}

# Correcting catch data
catchData <- catchData%>%
  dplyr::mutate(catches_uncorrected = catches,
                catches = total_catches * class_prop)
write.csv2(catchData, file = file.path(m_OUTPUTS, "corrected_catchData.csv"), row.names = F)


######## 1.3. Aggregating catches on spatio-temporal units and saving data
cat(crayon::green("\t    -  Aggregating catches on the spatio-temporal units...."))
catchData <- catchData%>%
  dplyr::filter(fishing_mode=="fob", 
                null_set == FALSE,  # Retrieve null sets
                zone != "OFF_ZONE", 
                catches >= BIOMASS_OCCURENCE_THRESHOLD)%>%
  dplyr::group_by(year = lubridate::year(activity_date),
                  timescale, x, y, zone, fishing_mode)%>%
  dplyr::mutate(set_count   = length(unique(set_id)))%>%
  # Discard strata with a low number of sets to compute <m>
  dplyr::filter(set_count >= SET_COUNT_THRESHOLD)%>% 
  dplyr::group_by(year,timescale, x, y, zone, fishing_mode, species, weight_category)%>%
  dplyr::summarise(avg_catches_uncorrected = mean(catches_uncorrected, na.rm=T),
                   sd_catches_uncorrected  = sd(catches_uncorrected, na.rm=T),
                   
                   avg_catches = mean(catches, na.rm = T),
                   sd_catches  = sd(catches, na.rm = T),

                   set_count   = unique(set_count),
                   se_catches  = sd_catches/sqrt(set_count),
                   med_catches = median(catches, na.rm = T),
                   min_catches = min(catches, na.rm = T),
                   max_catches = max(catches, na.rm = T),
                   .groups="keep")%>%
  dplyr::filter(species==SPECIES, 
                year %in% YEARS,
                (is.na(weight_category) || weight_category== SIZE_CLASS))

catchdata_fn <- file.path(m_OUTPUTS, "aggregated_catchdata.csv")
write.csv2(catchData, file =  catchdata_fn, row.names = F)
cat("Done.\n")
