#'#*******************************************************************************************************************
#'@title : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2021-06-24
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  Analysis on catches and species composition
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

cat(crayon::italic("\n\t+ 3.2. Analysing catches and related data <m>.\n"))

##### 1. Species compositions from Sampling data and  Logbook reporting----------------
cat(crayon::green("\t\t    3.2.1. Comparison of species compositions from Sampling data and  Logbook reporting....."))
catchData    <- read.csv2(file.path(m_OUTPUTS, "catchData.csv"), stringsAsFactors = F)
samplingData <- read.csv2(file.path(RESOURCES_DIR, "T3_Sampling_data.csv"), stringsAsFactors = F)
      
log_prop <- catchData%>%
    dplyr::mutate(sp_prop = catches/total_catches)%>%
    group_by(year,  fishing_mode)%>%
    dplyr::mutate(set_count = length(unique(set_id)))%>%
    dplyr::group_by(year,  fishing_mode, species)%>%
    dplyr::summarise(avg = sum(sp_prop)/set_count,
                     type ="Logbook reporting (standardized)",
                    .groups = "keep")
  
smp_prop <- samplingData%>%
  dplyr::mutate(sp_prop = catches/total_catches)%>%
  group_by(year,  fishing_mode)%>%
  dplyr::mutate(set_count = length(unique(set_id)))%>%
  dplyr::group_by(year,  fishing_mode, species)%>%
  dplyr::summarise(avg = sum(sp_prop)/set_count,
                   type ="Sampling data",
                   .groups = "keep")

ggplot(rbind.data.frame(log_prop, smp_prop),
       aes(x=year, y=avg))+
  geom_bar(aes(fill=species), position="dodge", stat="identity")+
  labs(y="Catch proportion")+
  ylim(c(0,1))+
  facet_grid(type~fishing_mode)+
  theme_gray(base_size = 12)
ggsave(filename = file.path(m_OUTPUTS, "1-species_composition_logbook_vs_Sampling.png"), width = 20, height =18, units = "cm", dpi = 600)
cat("Done.\n")
   
##### 2. FOB Species composition over time per strata--------------------------------------------------
cat(crayon::green("\t\t    3.2.2. Time evolution of FOB species compositions per strata....."))
species_composition <- read.csv2(file.path(m_OUTPUTS, "species_composition.csv"), stringsAsFactors =F)%>%
	dplyr::filter(fishing_mode=="fob")%>%
	dplyr::mutate(timestamp = dplyr::case_when(
			toupper(TIME_SCALE) == "QUARTER" ~ as.Date(as.POSIXct(zoo::as.yearqtr(paste0(year, "-Q", timescale),format = "%Y-Q%q"))),
			toupper(TIME_SCALE) == "MONTH"   ~ as.Date(as.POSIXct(zoo::as.yearmon(paste0("01", timescale, year),format = "%d%B%Y")))),
			y = factor(y, levels = sort(unique(y), decreasing = T), ordered = T))



### All species (stacked barplot)
ggplot(subset(species_composition, fishing_mode=="fob" ), 
       aes(x= timestamp,  y = class_prop, fill=paste(species, weight_category)))+
    geom_bar(position = "stack", stat="identity")+
    scale_fill_manual(values = c(RColorBrewer::brewer.pal(6, "Blues")[c(3,5)],
                                 RColorBrewer::brewer.pal(6, "Greens")[5],
                                 RColorBrewer::brewer.pal(6, "Reds")[c(4,6)]))+
    labs(y="Species proportion", x="Time", fill="Species")+
    theme_classic(base_size = 8)+
    theme(legend.position = "bottom",
          panel.background = element_rect(colour="black"),
          legend.key.size = unit(0.25, "cm"))+
    facet_grid(as.factor(y)~x)
ggsave(filename = file.path(m_OUTPUTS, "2.1-FOB-species_composition_per_strata-allspecies.jpg"),
       width = 20, height = 12, dpi = 600, units = "cm")


### Only selected  species (time series)
ggplot(subset(species_composition, species ==SPECIES), 
       aes(x= timestamp, y = class_prop,
           group = ifelse(is.na(weight_category), "-10kg", weight_category),
           color = ifelse(is.na(weight_category), "-10kg", weight_category)))+
  geom_errorbar(aes(ymin=class_prop-class_sd, ymax=class_prop+class_sd))+
  geom_line(size= 0.5)+
  geom_point(shape=21, size=0.75, fill="white")+
  labs(y="Species proportion", x="Time", color="Size class",
       title = paste0(SPECIES, "proportion in FOB-sampled sets per strata"))+
  theme_classic(base_size = 8)+
  theme(legend.position = "bottom",
        panel.background = element_rect(colour="black"),
        legend.key.size = unit(0.25, "cm"))+
  facet_grid(y~x)
ggsave(filename = file.path(m_OUTPUTS, paste0("2.2-FOB-species_composition_per_strata-only", SPECIES,".jpg")),
                            width = 20, height = 12, dpi = 600, units = "cm")		 
cat("Done.\n")		
	 
##### 3. Overall  species composition per time----------------
# Lookup quarterly/monthly average of species composition
if(FIll_IN_MISSING_DATA)
{
	cat(crayon::green("\t\t    3.2.3. Time evolution of overall FOB species composition.\n"))
	species_composition_timeAggr <- read.csv2(file.path(m_OUTPUTS, "time_aggregated_species_composition.csv"), stringsAsFactors =F)%>%
	  dplyr::mutate(y = factor(y, levels = sort(unique(y), decreasing = T), ordered = T),
	                timestamp = dplyr::case_when(
                  	    toupper(TIME_SCALE) == "QUARTER" ~ as.Date(as.POSIXct(zoo::as.yearqtr(paste0("2000-Q", timescale),format = "%Y-Q%q"))),
                  	    toupper(TIME_SCALE) == "MONTH"   ~ as.Date(as.POSIXct(zoo::as.yearmon(paste0("01", timescale, "2000"),format = "%d%B%Y")))))
	
	#'Time evolution of species composition over the study area
  ggplot(subset(species_composition_timeAggr, fishing_mode=="fob" ), 
         aes(x= timestamp+31, y = class_prop, fill=paste(species, weight_category)))+
    geom_col(position = "stack")+
    scale_x_date(date_breaks ="3 months", date_minor_breaks = "1 month", date_labels = "%b")+
    scale_fill_manual(values = c(RColorBrewer::brewer.pal(6, "Blues")[c(3,5)],
                                 RColorBrewer::brewer.pal(6, "Greens")[5],
                                 RColorBrewer::brewer.pal(6, "Reds")[c(4,6)]))+
    labs(y="Species proportion", x="Time", fill="species")+
    theme_classic(base_size = 10)+
    theme(panel.background = element_rect(colour="black"))+
    facet_grid(y~x)
  ggsave(filename = file.path(m_OUTPUTS, "3.1-Overall_species_composition_per_timescale.jpg"),
         width = 16, height = 9, dpi = 300, units = "cm")
  
  
  # Species proportion with reconstructed values (approxiamted from quartely average of porportion in the same area) -----------------------
  #### Dataframe with corrected values : Approximated and actual proportions values
  sp_corrected <- species_composition%>%
    dplyr::filter(fishing_mode=="fob", species==SPECIES,
                  (is.na(weight_category) | weight_category == SIZE_CLASS))%>%
    dplyr::group_by(zone, x, y)%>%
    dplyr::group_modify(~ merge.data.frame(x = .x,
                                           y = data.frame(timestamp = seq.Date(from = as.Date("2013-01-01"), to =  as.Date("2019-10-01"), by= tolower(TIME_SCALE)),
                                                          timescale = rep(1:4, 7)),
                                           all.y = T))%>%
    dplyr::select(zone, x,y, timestamp, timescale, class_prop, class_sd)%>%
    dplyr::mutate(approx = ifelse(is.na(class_prop), T, F))
  
  aggr_species_composition <- read.csv2(file.path(m_OUTPUTS, "time_aggregated_species_composition.csv"), stringsAsFactors =F)%>%
    dplyr::filter(fishing_mode=="fob", species==SPECIES,
                  (is.na(weight_category) | weight_category == SIZE_CLASS))%>%
    dplyr::select(x , y, timescale, class_prop, class_sd)
  
  sp_corrected <- merge.data.frame(x= sp_corrected, y= aggr_species_composition, by=c("x", "y", "timescale"),
                                   all.x = T, suffixes = c("","_aggr"))%>%
    dplyr::mutate(class_prop  = ifelse(is.na(class_prop), class_prop_aggr, class_prop),
                  class_sd    = ifelse(is.na(class_sd),  class_sd_aggr, class_sd))
  
  #### plot reconstructed time series
  ggplot(sp_corrected, aes(x= timestamp, y = class_prop))+
    geom_errorbar(aes(ymin=class_prop-class_sd, ymax=class_prop+class_sd), width=30, alpha=0.5)+
    geom_line(size= 0.6)+
    geom_point(aes(fill = approx), shape=21, size=1.5)+
    scale_fill_manual(values=c("white", "red"))+
    labs(y="Proportion", x="Time", color="Size class",
         title = paste0(SPECIES, " proportion in FOB-sampled sets per strata"),
         caption="Red points represents proportion values approximated from\nthe quartely average proportion over the study period in the spatial strata")+
    theme_classic(base_size = 8)+
    theme(legend.position = "bottom",
          panel.background = element_rect(colour="black"),
          legend.key.size = unit(0.25, "cm"))+
    facet_grid(y~x)
  ggsave(filename = file.path(m_OUTPUTS, paste0("3.2-reconstructed_species_proportion_with_approximated_values.jpg")),
         width = 20, height = 12, dpi = 600, units = "cm")		 
  cat("Done.\n")
}

##### 4. Comparison of  corrected vs uncorrected catches----------------
cat(crayon::green("\t\t   3.2.4. Comparison of corrected and uncorrected catches.\n"))
catchData <- read.csv2(file.path(m_OUTPUTS, "corrected_catchData.csv"), stringsAsFactors =F)

p1 <- ggplot(subset(catchData, fishing_mode=="fob"),
         aes(x= catches_uncorrected, y = catches))+
  geom_point(shape=21, fill="darkblue", alpha=0.3)+
  xlim(c(0,300))+
  ylim(c(0,300))+
  geom_abline(slope=1, linetype="dashed", color="red", size=0.5)+
  labs(title = "FOB associated catches", 
       x="Logbook standardized", y="Corrected catches")+
  theme_classic(base_size = 8)+
  theme(panel.background = element_rect(colour="black"))+
  facet_grid(weight_category ~ species)

p2 <- ggplot(subset(catchData, fishing_mode=="fsc"),
             aes(x= catches_uncorrected, y = catches))+
  geom_point(shape=21, fill="darkblue", alpha=0.3)+
  xlim(c(0,300))+
  ylim(c(0,300))+
  geom_abline(slope=1, linetype="dashed", color="red", size=0.5)+
  labs(title = "FSC associated catches", 
       x="Logbook standardized", y="Corrected catches")+
  theme_classic(base_size = 8)+
  theme(panel.background = element_rect(colour="black"))+
  facet_grid(weight_category ~ species)

ggpubr::ggarrange(p1, p2, nrow=2)
ggsave(filename = file.path(m_OUTPUTS, "4-Comparison_st_Catches_vs_correctedCatches.jpg"),
       width = 16, height = 12, dpi = 300, units = "cm")
cat("Done.\n")
  
##### 5. Distribution of average catches and set number in the different strata----------------
cat(crayon::green("\t\t   3.2.5. Distribution of average catches and fishing set number in the different stratum.\n"))
catchData <- read.csv2(file.path(m_OUTPUTS, "aggregated_catchdata.csv"), stringsAsFactors=F)
catch_df <- reshape::melt(data = as.data.frame(subset(catchData, species==SPECIES & year %in% YEARS, 
                                                      select = c("year", "zone", "timescale", "avg_catches", "set_count"))),
                          measure.vars = c("avg_catches", "set_count"))
catch_df$variable <- plyr::mapvalues(x = catch_df$variable , 
                                     from = c("avg_catches", "set_count"),  
                                     to = c("Average catches (tons)", "Number of fishing sets"))
          
infoDf <-  catch_df%>%
    dplyr::group_by(variable)%>%
    dplyr::summarise(avg = mean(value, na.rm = T),
                     max = max(value, na.rm = T), 
                     label = round(avg),
                     .groups="keep")
          
ggplot(data = catch_df, aes(x=value))+
  geom_histogram(fill="lightblue", color="black", size= 0.2)+
  geom_vline(data = infoDf, aes(xintercept=avg, group=variable), color="red", linetype="dashed", size=1)+
  geom_text(data = infoDf,
            aes(x = avg,
                y = 0,
                label = label,
                group=variable), size=3, fontface="bold")+
  facet_wrap(~variable, scales = "free")+
  theme_linedraw(base_size = 8)+
  labs(title=paste0("Distribution of ", SPECIES, 
                    ifelse(SPECIES=="SKJ", "", paste0(" (", SIZE_CLASS, ") ")),
                    " corrected catches and fishing sets per ", time_lbl," and ", space_lbl), x="")+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour="gray", size = 0.25, linetype = "dashed"),
        panel.grid.minor.y = element_line(colour="gray", size = 0.25, linetype = "dashed"))
ggsave(filename = file.path(m_OUTPUTS, "5-setNumber_and_catchDistribution.jpg"),
       width = 16, height = 8, dpi = 600, units = "cm")
cat("Done.\n")


##### 6. Time series of corrected catches and standardized logbook catches----------------
cat(crayon::green("\t\t    3.2.6. Time series of corrected catches and standardized logbook catches in the different stratum.\n"))
catchData <- read.csv2(file.path(m_OUTPUTS, "aggregated_catchdata.csv"), stringsAsFactors=F)%>%
      dplyr::mutate(timestamp = case_when(
      toupper(TIME_SCALE) == "QUARTER" ~ as.Date(as.POSIXct(zoo::as.yearqtr(paste0(year, "-Q", timescale),format = "%Y-Q%q"))),
      toupper(TIME_SCALE) == "MONTH"   ~ as.Date(as.POSIXct(zoo::as.yearmon(paste0("01", timescale, year),format = "%d%B%Y")))))

ggdf <-  melt_nd(data=catchData[, c("timestamp", "x", "y", "avg_catches", "avg_catches_uncorrected", "sd_catches", "sd_catches_uncorrected")], 
                 id.vars = c("timestamp", "x", "y"), 
                 measure.vars = list(avg = c("avg_catches", "avg_catches_uncorrected"),
                                     se  = c( "sd_catches", "sd_catches_uncorrected")))

ggdf <- ggdf %>%
  dplyr::mutate(variable.avg = plyr::mapvalues(x = variable.avg, 
                                               from=c("avg_catches", "avg_catches_uncorrected"),
                                               to = c(paste("Standardized and corrected data"), 
                                                      paste0("Standardized logbook data"))),
                y = factor(y, levels = sort(unique(y), decreasing = T), ordered = T),
                x = as.factor(x))

ggplot(data=ggdf, aes(x=timestamp, y=avg, color=variable.avg))+
  geom_errorbar(aes(x=timestamp, ymin=avg-se, ymax=avg+se), width=30)+
  geom_line (size=0.6)+
  geom_point(shape=21,aes(fill=variable.avg), size=0.9)+
  scale_fill_manual(values=c("white", "white"))+
  scale_color_manual(values=c("red", "blue"))+
  labs(x="Time", y="Catches (tons)", 
       fill="",linetype="", color="", 
       title =  paste0(SPECIES, " (", SIZE_CLASS, ") catches"))+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", date_minor_breaks = "1 year")+
  theme_classic(base_size = 9)+
  theme(panel.background = element_rect(colour="black"),
        legend.position  = "bottom")+
  facet_grid(y~x, scales = "free")
ggsave(filename = file.path(m_OUTPUTS, "6-corrected_and_uncorrected_catch_time_series.png"),
       width = 30, height = 18, dpi = 300, units="cm")


##### 7. scattergraph of time series of catches---------------------------------------------------------------------------------
fob_DATA <- read.csv2(file.path(OUTPUTS_DIR, "modelInputs_timeSeries.csv"), stringsAsFactors = F)%>%
  dplyr::mutate(timestamp = as.Date(timestamp))

if(is.numeric(SPATIAL_SCALE))
{
  # zone summary      
  catches_aggr <- fob_DATA%>%
    dplyr::group_by(x, y, zone)%>%
    dplyr::summarise(avgCatches = mean(catches, na.rm=T), 
                     .groups="keep")
  
  (worldmap <- ggplot()+
      geom_tile(data=catches_aggr, aes(x, y, fill=avgCatches))+
      geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory")+
      labs(x="Longitude", y="Latitude", fill=paste("Average catches (in tons) over", paste(range(YEARS), collapse=" - ")))+
      scale_fill_distiller(palette="Spectral", 
                           guide = guide_colorbar(title.position = "top", title.hjust=0.5, 
                                                  barwidth=50, ticks.linewidth = 1.5, ticks.colour = "black", frame.colour="black"))+
      theme_linedraw(base_size = 12)+
      theme(panel.background = element_rect(color="black", fill="white"),
            panel.grid.major  = element_line(colour = "dimgray", linetype = "dashed", size=0.4),
            panel.grid.minor  = element_blank(),
            axis.ticks = element_line(size=1, colour="black"),
            legend.position = "bottom"))
  
  # Scattergraphs
  geom_scattergraph_v3(map = worldmap,
                       lon = fob_DATA$x,
                       lat = fob_DATA$y,
                       x   = fob_DATA$timestamp,
                       y   = fob_DATA$catches,
                       group = NULL,
                       se   = fob_DATA$sd_catches,
                       size=0.7,
                       cellSize = SPATIAL_SCALE,
                       linetype="solid")
  ggsave(filename = file.path(m_OUTPUTS, "7-catches_over_year.png"),
         width = 30, height = 20, dpi = 1200, units="cm")
}
cat("Done.\n")

##### 8. Kernel density of fishing set operations for global maps---------------------------------------------------
if(!is.numeric(SPATIAL_SCALE))
{
  catches_pos <- read.csv2(file = file.path(m_OUTPUTS, "catchData.csv"), stringsAsFactors = F)%>%
    dplyr::mutate(year = lubridate::year(activity_date))%>%
    dplyr::filter(species==SPECIES, fishing_mode=="fob", year %in% YEARS, ocean==OCEAN, catches >0, zone != "OFF_ZONE")%>%
    dplyr::select(year, zone, timescale, longitude, latitude, catches)


  #'@graph: Kernel density of catches in area
  #'---------------------------------------------
  # Loading area
  cat(crayon::green("\t\t    3.2.7. Kernel density of catches in area over the study period...."))
  area      <- get_area(map_df  = SPATIAL_SCALE)
  zoning.df <- load_zone(map_df = SPATIAL_SCALE)
  
  ggplot(data=catches_pos)+
      labs(x="Longitude", y="Latitude", fill="Level")+
      stat_density2d(aes(x=longitude, y=latitude, fill=..level..),
                     geom="polygon", alpha= 1, color="dimgray", size = 0.1)+
      geom_map(data=world, map=world,aes(map_id =region),fill = "gray", color="ivory")+
      geom_polygon(data=zoning.df, 
                   mapping = aes(x=long, y=lat, group = group), 
                   alpha = 0, fill = "white",
                   color ="red", linetype ="solid", size = 0.4, show.legend =F)+
      scale_x_continuous(breaks = seq(-180,180,20))+
      scale_y_continuous(breaks = seq(-90,90,20))+
      scale_fill_distiller(palette="Greens", direction = 1)+
      coord_quickmap(xlim = c(25, 100), ylim=c(-40,30))+
      ggsn::scalebar(dist = 1000, dist_unit = "km", model = 'WGS84', transform = T, location = "bottomleft",
                     height = 0.03, st.dist = 0.02, st.size = 4,
                     x.min = 25, x.max = 100,
                     y.min = -40, y.max = 30)+
      ggsn::north(location = "topright", x.min = 25, x.max = 100,
                  y.min = -40, y.max = 30, scale=0.15)+
      theme_linedraw(base_size = 12)+
      theme(panel.background = element_rect(color="black", fill="white"),
            plot.background = element_rect(fill=NA, color="transparent"),
            panel.grid.major = element_line(colour = "dimgray", linetype = "dashed", size=0.25),
            panel.grid.minor = element_blank())+
      facet_wrap(~zone)
  ggsave(filename = file.path(m_OUTPUTS, "8-Cacthes_kernel_density.png"), width = 9, height = 6, dpi = 1200)
  cat("Done.\n")
}


#### Verbosity
cat(crayon::green("\t\t    - Graphical outputs available in folder:\n"), crayon::blue$underline$italic(m_OUTPUTS))

