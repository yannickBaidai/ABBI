##################################################################################################################################
#'@author Y. BAIDAI 
#'@title tropical tunas abundance Index based on their associative behaviour
#'@contact yannick.baidai@gmail.com
#'@date 2020-06-24
#'---------------------
#'@update:
#'2021-04-13 : (Major update)
#'2021-06-24
#' - Scripts for YFT and SKJ including m and f correction
##################################################################################################################################

rm(list=ls())
invisible(gc())

#'@param: working directory
WORKING_DIR <- "D:/ABBI/ABBI/"

#'@outputs_directories:
OUTPUT_DIR <-  "D:/Updated_Tuna_Abundance_Assessment_from_T3"


#'@INPUTS---------------------------------------------------------------------------------------------------------------
#'@DIRECTORIES
#-----------------------------------------------------------------------------------------------------------------------

#'@CATCHES_DATA: from BALBAYA DATA
m_DATA_DIR     <- "D:/ABBI/ABBI/0-Data/m-dataSource/T3-L2_catchesData.csv" # T3 Level 2 data

#'@species_occurence_raw_data: from T3-L2 sampling data
T3_SAMPLING_DATAFILE <- "D:/ABBI/ABBI/0-Data/m-dataSource/T3-samplingData.csv"

#'@Buoy_density_data: from M3I buoys
Nfob_DATA_DIR   <- "D:/Data/EchoFad_Outputs_per_year/"

#'@Tuna_aggregation_presence_absence: from echosounder data
f1_DATA_DIR     <- "D:/ABBI/ABBI/0-Data/f-dataSource/"

#'@param NOMINAL_CATCHES_FILE : Nominal catches data
NOMINAL_CATCHES_FILE <- "D:/Data/0-Nominal catches from IOTC/IOTC-2020-DATASETS-CESurface_per_strata/IOTC-2020-WPTT22-DATA05-CESurface.csv"


#---------------------------------------------------------------------------------------------------------------------------------------#
####### PARAMETERS
#'@param OCEAN:
OCEAN <- "Indian"

#'@param SPECIES: <character vector> Tuna species to be considered
SPECIES_   <- c("SKJ", "YFT", "BET")#"SKJ"
SIZECLASS  <- "-10kg" # Only useful for YFT  or BET

#'@param YEARS: <numeric vector> Years to be processed
YEARS <- 2013:2019

#'@param REF_TIME: <character> Reference time (t0) for the relative abundance Index : I = N(t)/N(t0)
#'format : YYYY-MM_DD
REF_TIME <- "2013-01-01"

#'@param TIME_SCALE: <character list> Time unit of aggregation ("Day", "Month", "Quarter" or "Week")
#TIME_SCALES <- c("Month", "Quarter")
TIME_SCALES <-  "Quarter"

#'@param SPATIAL_SCALE: List or vector containing : Spatial unit of aggregation: 
#' the Integer value (defining cel SPATIAL_SCALE) or folder path (valid folder containing a shapedfile of zonee) can be provided
#SPATIAL_SCALES <- list(WIO = file.path(WORKING_DIR, "0-Resources/GZ_CROPPED_INDIAN_OCEAN"),
#                      GRID = 10)
SPATIAL_SCALES <- list(GRID = 10)

#'@param BOUNDING_BOX: list or NULL optional limits of the area and zone to exlcude from the analyses
BOUNDING_BOX   <- list(xmin = 40,  xmax = 70,
                       ymin = -10, ymax = 10,
                       excludedZones = c(347))

#---------------------------------------------------------------------------------------------------------------------------------------#
###### Routines triggers
#'@Analyses_to_be_run
BUILD_DATA          <- T   # Build base data for the analysis from raw inputs of previous lines (L25, 28, etc)
COMPUTE_BUOYDENSITY <- T   # Compute buoy density (time consuming), can be skipped from the data building if already generated
BUILD_TIME_SERIES   <- T   # Build time series of inputs parameters and abundance
RUN_ANALYSIS        <- T   # Graphical outputs

#'@Options_and_parameters
#'Species composition and species occurence
FIll_IN_MISSING_DATA                       <- T  # Fill missing data of species composition or occurence with an average value computed over the study area for the same timescale 
SET_COUNT_THRESHOLD_FOR_M_and_F_CORRECTION <- 20 # Minimum number of fishing sets used from sampling data to compute class proportion / species occurence
BIOMASS_OCCURENCE_THRESHOLD                <- 1  # Minimum biomass (in tons) to consider a species present under a FAD

#'Computing of m average
SET_COUNT_THRESHOLD   <- SET_COUNT_THRESHOLD_FOR_M_and_F_CORRECTION # Minimum number of fishing sets to compute the average f over the timescalece

#'Computing of f average composition and species occurence
BUOYS_COUNT_THRESHOLD                      <- 30 # Minimum number of buoys to calculate daily F values in the area.
MIN_NUMBER_DAY_FOR_COMPUTE_F_AVERAGE       <- 30 # Minimum number of daily F values  to compute average f over the timescale
TRUNCATE_START_SECTIONS_FOR_F_COMPUTATIONS <- T  # Truncate start sections of FAD trajectories before computing f

#' Sensitivity index with regard to CRT vatriability
RUN_SENSITIVITY_ON_CRT   <- T  # Run sensitivity analysis on abundance estimates with regard to CRT


#---------------------------------------------------------------------------------------------------------------------------------------#
###### Computations loop
for(SPECIES in SPECIES_)
{
  # Check <SIZE_CLASS> for SKJ
  SIZE_CLASS <- ifelse(SPECIES == "SKJ", NA, SIZECLASS)
  
  for(TIME_SCALE in TIME_SCALES)
  {
    for(spatial_ptr in 1:length(SPATIAL_SCALES))
    {
      # Current spatial strata
      SPATIAL_SCALE <- SPATIAL_SCALES[[spatial_ptr]]
      
      # verbosity3
      cat("\014")
      cat("#################################################################################\n")
      cat("###########       ASSOCIATIVE BEHAVIOUR-BASED ABUNDANCE INDEX     ###############\n")
      cat("#################################################################################\n")
      cat(crayon::bold("+ 0. Reading parameters and generating outputs folder....\n"))
      cat(crayon::bold$yellow("     ==================================\n"))
      cat(crayon::italic$yellow("       - Processing   :", crayon::white(SPECIES)), "\n")
      cat(crayon::italic$yellow("       - Time scale   :", crayon::white(TIME_SCALE)), "\n")
      cat(crayon::italic$yellow("       - Spatial scale:", crayon::white(SPATIAL_SCALE)), "\n")
      cat(crayon::bold$yellow("     ==================================\n"))
      
      #START----------------------------------------------------------------------------------------------------------------------#
      #'@folder_check
      OUTPUTS_DIR <- file.path(OUTPUT_DIR, 
                               SPECIES,
                               ifelse(class(SPATIAL_SCALE)=="numeric", paste0(SPATIAL_SCALE, " Deg. Square"), basename(SPATIAL_SCALE)),
                               toupper(TIME_SCALE))
      
      try(dir.create(OUTPUTS_DIR, recursive = T, showWarnings =F))
      
      m_OUTPUTS <- file.path(OUTPUTS_DIR,"1-m_data")
      try(dir.create(m_OUTPUTS, recursive = T, showWarnings =F))
      
      f1_OUTPUTS <- file.path(OUTPUTS_DIR,"3-f1_data")
      try(dir.create(f1_OUTPUTS, recursive = T, showWarnings =F))
      
      Nfob_OUTPUTS <- file.path(OUTPUTS_DIR,"2-Nfob_data")
      try(dir.create(Nfob_OUTPUTS, recursive = T, showWarnings =F))
      
      RESOURCES_DIR   <- file.path(WORKING_DIR, "0-Resources")
      if(!dir.exists(RESOURCES_DIR))
        stop(crayon::red$bold("Resources folder not found :", RESOURCES_DIR))
      
      FUNCTIONS_DIR   <- file.path(WORKING_DIR, "0-Functions")
      if(!dir.exists(FUNCTIONS_DIR))
        stop(crayon::red$bold("Functions folder not found :", FUNCTIONS_DIR))
      
      SUB_ROUTINES    <- file.path(WORKING_DIR, "1-Routines")
      if(!dir.exists(SUB_ROUTINES))
        stop(crayon::red$bold("Routines folder not found :", SUB_ROUTINES))
      
            BUILDDATA_ROUTINES    <- file.path(SUB_ROUTINES, "1-Build_Data_Routines")
            if(!dir.exists(BUILDDATA_ROUTINES))
              stop(crayon::red$bold("Routines folder not found :", BUILDDATA_ROUTINES))
            
            TIMESERIES_ROUTINES <- file.path(SUB_ROUTINES, "2-Time_Series_Routines")
            if(!dir.exists(TIMESERIES_ROUTINES))
              stop(crayon::red$bold("Routines folder not found :", TIMESERIES_ROUTINES))
            
            ANALYSIS_ROUTINES <- file.path(SUB_ROUTINES, "3-Analysis_Routines")
            if(!dir.exists(ANALYSIS_ROUTINES))
              stop(crayon::red$bold("Routines folder not found :", ANALYSIS_ROUTINES))
            
            
      

      # save analysis parameters---------------------------------------------------------------------------------------------#
      new_param <- data.frame(log  = m_DATA_DIR,
                              buoyDens = Nfob_DATA_DIR,
                              echo = f1_DATA_DIR,
                              out  = OUTPUTS_DIR,
                              time = TIME_SCALE,
                              spat = SPATIAL_SCALE,
                              spec = SPECIES,
                              miss = FIll_IN_MISSING_DATA,
                              sp_occ_minPoints  = SET_COUNT_THRESHOLD_FOR_M_and_F_CORRECTION,
                              biomass_occMin    = BIOMASS_OCCURENCE_THRESHOLD,
                              m_minPoints       = SET_COUNT_THRESHOLD,
                              daily_f_minPoints = BUOYS_COUNT_THRESHOLD,
                              avg_f_minPoints   = MIN_NUMBER_DAY_FOR_COMPUTE_F_AVERAGE,
                              sections_trunc    = TRUNCATE_START_SECTIONS_FOR_F_COMPUTATIONS)


      param_backup <- file.path(OUTPUTS_DIR, "param_backup.csv")
      if(file.exists(param_backup)){
        old_param <- read.csv2(param_backup)
        RES <- tryCatch(all(old_param==new_param), error=function(e){return(FALSE)})
        if(RES){
          # same building data parameters : SKIP dataset construction step
          warning(crayon::yellow$bold("\t Previous analyses parameters match new parameters: Skipping dataset construction.\n"))
          BUILD_DATA <- FALSE
        }
      }
      
      #'@Variables_preserve
      CRT_THEO <- NULL
      DO_NOT_DELETE <- c("DO_NOT_DELETE", "new_param", "OCEAN", "YEARS", "REF_TIME", 
                         "WORKING_DIR", "m_DATA_DIR", "Nfob_DATA_DIR", "f1_DATA_DIR", "RESOURCES_DIR", "FUNCTIONS_DIR", "NOMINAL_CATCHES_FILE", "T3_SAMPLING_DATAFILE", 
                         "SUB_ROUTINES", "ANALYSIS_ROUTINES", "TIMESERIES_ROUTINES", "BUILDDATA_ROUTINES",
                         "OUTPUTS_DIR", "OUTPUT_DIR", "m_OUTPUTS", "f1_OUTPUTS","Nfob_OUTPUTS", 
                         "COMPUTE_BUOYDENSITY", "BUILD_DATA", "BUILD_TIME_SERIES", "RUN_ANALYSIS",
                         "TIME_SCALES", "TIME_SCALE", "SPATIAL_SCALES", "SPATIAL_SCALE", "spatial_ptr", "SPECIES", "SPECIES_", "SIZECLASS", "SIZE_CLASS",
                         "BUOYS_COUNT_THRESHOLD","BIOMASS_OCCURENCE_THRESHOLD", "SET_COUNT_THRESHOLD", "SET_COUNT_THRESHOLD_FOR_M_and_F_CORRECTION",
                         "RUN_SENSITIVITY_ON_CRT", "TRUNCATE_START_SECTIONS_FOR_F_COMPUTATIONS", "MIN_NUMBER_DAY_FOR_COMPUTE_F_AVERAGE", "FIll_IN_MISSING_DATA",
                         "BOUNDING_BOX", "CRT_THEO")

      #---------------------------------------------------------------------------------------------------------------------------------------#
      #'@Run analysis
      if(BUILD_DATA){
        cat(crayon::bold("+ 1. Building inputs data from file.\n"))
        source(file.path(BUILDDATA_ROUTINES, "1-Build_data.R"))
        write.csv2(new_param, file = file.path(OUTPUTS_DIR, "param_backup.csv"), row.names = F)
      }

      #---------------------------------------------------------------------------------------------------------------------------------------#
      if(BUILD_TIME_SERIES)
      {
        cat(crayon::bold("+ 2. Building abundance time series.\n"))
        source(file.path(TIMESERIES_ROUTINES,  "2-Build_time_series.R"))
      }

      #---------------------------------------------------------------------------------------------------------------------------------------#
      if(RUN_ANALYSIS)
      {
        cat(crayon::bold("+ 3. Run analyses.\n"))
        source(file.path(ANALYSIS_ROUTINES,  "3-run_analyses.R"))
      }
    }
  }
}








