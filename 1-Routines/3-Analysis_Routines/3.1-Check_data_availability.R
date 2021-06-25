#'#*******************************************************************************************************************
#'@title : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2020-06-25
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  Analysis script: checking the availablity of data over the space-time units defined
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************


#####################
## Check data availability per square
cat(crayon::italic("\n\t+ 3.1. Check data availability over the spatio-temporal stratification.\n"))
if(is.numeric(SPATIAL_SCALE))
{
  ### Loading catch data
  catch <- unique(read.csv2(file = file.path(m_OUTPUTS, "aggregated_catchdata.csv"))%>%
                    dplyr::filter(species==SPECIES, year %in% YEARS)%>%
                    dplyr::select(year, timescale, zone, x,y))
  catch$catch <- T
  
  ### Loading FAD data
  fads <- unique(read.csv2(file = file.path(f1_OUTPUTS, "aggregated_fadData.csv"))%>%
                   dplyr::filter(year %in% YEARS)%>%
                   dplyr::select(year, timescale, zone, x,y))
  fads$fads <- T
  
  ### Loading buoy data  
  buoys <- unique(read.csv2(file=file.path(Nfob_OUTPUTS, "aggregated_dailyBuoyDensityData.csv"))%>%
                    dplyr::filter(year %in% YEARS, x >20)%>%
                    dplyr::select(year, timescale, zone, x,y))
  buoys$buoys <- T
  
  ### Merging data source (m, f and p)
  df <- merge.data.frame(x=fads, y = catch, all =T)
  df <- merge.data.frame(x=df, y=buoys, all=T)
  try(df <- dplyr::select(df, -fishing_mode, -species), silent=T)
  df$avail <- apply(X = df[,6:8], MARGIN = 1, FUN = sum, na.rm=T) 
  
  
  df$avail2 <- ifelse(df$avail==3, "Full data", "Missing data")
  write.csv2(x = df, file = file.path(OUTPUTS_DIR, "data_availability.csv"), row.names = F)
  
  
  # No need to plot this per day !
  if(!toupper(TIME_SCALE)  %in% c("DAY", "WEEK"))
  {
    library(ggplot2)
    world <- map_data("world")
    ggplot()+
      geom_tile(data=df, aes(x, y, fill=avail2), color="transparent", size=0.1)+
      geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory", size=0.3)+
      labs(x="Longitude", y="Latitude", fill="Data availability")+
      scale_fill_manual(values=c( "darkgreen", "white"))+
      coord_quickmap(xlim = c(20,80), ylim=c(-50,50))+
      theme_linedraw(base_size = 9)+
      theme(panel.background = element_rect(color="black", fill="lightblue"),
            panel.grid.major  = element_line(colour = "dimgray", linetype = "dashed", size=0.4),
            panel.grid.minor  = element_blank(),
            axis.ticks = element_line(size=1, colour="black"),
            legend.position = "bottom")+
      facet_grid(timescale~year)
    ggsave(filename = file.path(OUTPUTS_DIR, paste0("0.2-data_availability_at_buoysCountThreshold_of", BUOYS_COUNT_THRESHOLD, ".png")), 
           width = 16, height = 9, dpi = 600)
    
    
    # Scatterpie
    df[is.na(df)] <- 0
    ggplot()+
      geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory", size=0.3)+
      scatterpie::geom_scatterpie(data=df, aes(x, y), cols=c("fads", "catch", "buoys"), color=NA)+
      labs(x="Longitude", y="Latitude", fill="Data availability")+
      coord_fixed(xlim = c(20,80), ylim=c(-50,50))+
      theme_linedraw(base_size = 12)+
      theme(panel.background = element_rect(color="black", fill="white"),
            panel.grid.major  = element_line(colour = "dimgray", linetype = "dashed", size=0.4),
            panel.grid.minor  = element_blank(),
            axis.ticks = element_line(size=1, colour="black"),
            legend.position = "bottom")+
      facet_grid(timescale~year)
    ggsave(filename = file.path(OUTPUTS_DIR, paste0("0.1-data_availability_at_buoysCountThreshold_of", BUOYS_COUNT_THRESHOLD, ".png")),
           width = 32, height = 18, dpi = 400)
  }
}
