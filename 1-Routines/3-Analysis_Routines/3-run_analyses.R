#'#*******************************************************************************************************************
#'@title : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2020-06-25
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  Analysis results and provide graphical outputs
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

rm(list = ls()[!ls() %in% get("DO_NOT_DELETE")])
invisible(gc())


### Functions and libraries loading
cat(crayon::italic("\n\t+ 3.0. Initialize resources for Analysis:\n"))

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

#### Label for graphs
time_lbl  <- tolower(TIME_SCALE)
timeScaleLbl <- ifelse(toupper(TIME_SCALE) =="QUARTER", "Quarter", "")
space_lbl <- ifelse(is.numeric(SPATIAL_SCALE), 
                    paste0("per ", SPATIAL_SCALE, "° square"),
                    paste("in", basename(SPATIAL_SCALE)))
yearLbl <- paste(range(YEARS), collapse = '-')
specieLbl <- ifelse(SPECIES =="SKJ", "SKJ", paste0(SPECIES, " (", SIZE_CLASS, ")"))


# Loading world map
world <- ggplot2::map_data("world")


# Launch analysis scripts
source(file.path(ANALYSIS_ROUTINES, "3.1-Check_data_availability.R"))
source(file.path(ANALYSIS_ROUTINES, "3.2-m_related_analyses.R"))
source(file.path(ANALYSIS_ROUTINES, "3.3-f_related_analyses.R"))
source(file.path(ANALYSIS_ROUTINES, "3.4-p_related_analyses.R"))
source(file.path(ANALYSIS_ROUTINES, "/3.5-abundance_related_analyses.R"))
