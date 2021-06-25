#'@FUNCTION: IOTC_dataFormatter
#'--------------------------------------------------------------------------------------------------------------------------------
#'#' @param inputFilepath  <character> filepath of the IOTC data files
#' @param outputFilePath <character> filepath of the output fil, if NULL a file is saved in the same folder of the input path
#' @param return <boolean> return or not the dataframe
#'#'
#' @examples
#' df <- IOTC_dataFormatter(inputFilepath = "D:/IOTC-2020-DATASETS-CESurface/IOTC-2020-WPTT22-DATA05-CESurface.csv")
#' library(ggplot2)
#' world <- map_data("world")
#' ggplot(data=subset(df, Year==2016 & Gear=="PS"))+
#'   geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory")+
#'   geom_tile(aes(x=x, y=y, height = yOffset, width=xOffset, fill = SKJ.LS), color="white")+
#'   scale_fill_distiller(palette = "Spectral")+
#'   facet_wrap(~MonthStart)
#'--------------------------------------------------------------------------------------------------------------------------------
IOTC_dataFormatter <- function(inputFilepath, outputFilePath= NULL)
{

  # IOTC CE DATASETS file format: csv files with sep = "," and dec ="."
  df <- read.csv(file = inputFilepath, stringsAsFactors = F)
  
  # Get lat and lon coordinates:
  IOTC_REF <- data.frame(size = 1:6,
                         latRes     = c(5,  10, 10, 20, 1, 5),
                         lonRes     = c(10, 20, 10, 20, 1, 5))
  
  coords <- data.frame(grid=as.character(df$Grid))
  
  coords$size     <- as.numeric(substr(coords$grid, 1,1))
  coords$latRes   <- IOTC_REF$lonRes[match(coords$size, IOTC_REF$size)]
  coords$lonRes   <- IOTC_REF$lonRes[match(coords$size, IOTC_REF$size)]
  coords$quadrant <- as.numeric(substr(coords$grid, 2,2))
  
  coords$lat <- ifelse(coords$quadrant %in% c(1,4), 1, -1) * as.numeric(substr(coords$grid, 3,4))
  coords$lon <- ifelse(coords$quadrant %in% c(1,2), 1, -1) * as.numeric(substr(coords$grid, 5,7))
  
  # Cell center
  df$y    <- coords$lat + (coords$latRes/2)
  df$yOffset <- coords$latRes
  
  df$x    <- coords$lon + (coords$lonRes/2)
  df$xOffset <- coords$lonRes 
 
  if(!is.null(outputFilePath))
  {
    try(write.csv2(x = df, file = outputFilePath, row.names = F))
  }
  
  return(df)
}



#----------------------------------
RUN <- F
if(RUN)
{
  RES <- ifelse(exists("RESOLUTION"), RESOLUTION, 5)
  source(file = "D:/These Baidai/Redaction/4-Assessing tuna abundance from their associative dynamics with drifting FADs/Scripts/0-Functions/custom_functions.R")
  source(file = "D:/These Baidai/Redaction/4-Assessing tuna abundance from their associative dynamics with drifting FADs/Scripts/0-Functions/scattergraph.R")
  
  #-------SCRIPT: Agregate nominal catches of SKJ and YFT from IOTC DATA
  df <- IOTC_dataFormatter(inputFilepath = "D:/Data/0-Nominal catches from IOTC/IOTC-2020-DATASETS-CESurface_per_strata/IOTC-2020-WPTT22-DATA05-CESurface.csv")
  spat <- spatialize(longitude = df$x, latitude = df$y, resolution = RES)
  df <- cbind(df[,-c(55,57)], spat)
  
  # Aggregating data
  require(dplyr)
  df_ <- df%>%
    dplyr::filter(Year %in% 2010:2019)%>%
    dplyr::group_by(zone, x, y, year=Year, month=MonthStart, 
                    timestamp = as.Date(as.POSIXct(zoo::as.yearmon(paste0(Year, "-", MonthStart)))))%>%
    dplyr::summarise(YFT.FS   =  sum(YFT.FS, na.rm=T),
                     YFT.LS   =  sum(YFT.LS, na.rm=T),
                     YFT.UNCL =  sum(YFT.UNCL, na.rm=T),
                     SKJ.FS   =  sum(SKJ.FS, na.rm=T),
                     SKJ.LS   =  sum(SKJ.LS, na.rm=T),
                     SKJ.UNCL =  sum(SKJ.UNCL, na.rm=T),
                     .groups   =  "keep")%>%
    dplyr::mutate(YFT = YFT.FS + YFT.LS + YFT.UNCL,
                  SKJ = SKJ.FS + SKJ.LS + SKJ.UNCL)
  write.csv2(x = df_, 
             file = paste0("D:/Data/0-Nominal catches from IOTC/IOTC-2020-DATASETS-CESurface_per_strata",
                           "/IOTC-2020-WPTT22-YFT-SKJ_", RES,"Deg-aggregated_catches.csv"), row.names = F)                      
                       
                     
  library(ggplot2)
  world <- map_data("world")
  map <-  ggplot()+
    geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory")+
    labs(x="Longitude", y="Latitude", fill="Average CAT (days)")+
    scale_fill_distiller(palette="Spectral", trans="log10")+
    theme_linedraw(base_size = 9)+
    theme(panel.background = element_rect(color="black", fill="white"),
          panel.grid.major  = element_line(colour = "dimgray", linetype = "dashed", size=0.4),
          panel.grid.minor  = element_blank(),
          axis.ticks = element_line(size=1, colour="black"),
          legend.position = "bottom")
  
    
  
  # Scattergraphs
  geom_scattergraph(p = map, 
                    lon = df_$x, 
                    lat = df_$y, 
                    x   = df_$timestamp,
                    y   = df_$SKJ,
                    size=0.3,
                    graph_size = RES,
                    color ="black", linetype="solid")

  

  
}



