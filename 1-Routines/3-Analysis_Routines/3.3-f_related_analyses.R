#'#*******************************************************************************************************************
#'@title : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2021-06-24
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  Analysis on proportion of inhabited FOBs and related data
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

cat(crayon::italic("\n\t+ 3.3. Analysing proportion of inhabited FOBs and related data <f>.\n"))

##### 1. Species occurence from Sampling data----------------
cat(crayon::green("\t\t     3.3.1. Comparison of species compositions from Sampling data and  Logbook reporting....."))
species_occurence <- read.csv2(file.path(f1_OUTPUTS, "species_occurence.csv"), stringsAsFactors = F)%>%
  dplyr::mutate(y = factor(y, levels = sort(unique(y), decreasing = T), ordered = T),
                weight_category = ifelse(is.na(weight_category), "-10kg", weight_category),
                timestamp = dplyr::case_when(
                    toupper(TIME_SCALE) == "QUARTER" ~ as.Date(as.POSIXct(zoo::as.yearqtr(paste0(year, "-Q", timescale),format = "%Y-Q%q"))),
                    toupper(TIME_SCALE) == "MONTH"   ~ as.Date(as.POSIXct(zoo::as.yearmon(paste0("01", timescale, year),format = "%d%B%Y")))))


ggplot(data  = subset(species_occurence, species==SPECIES), 
       aes(x = timestamp+31, y=occurence))+
 geom_col(aes(fill = weight_category), 
          position = "dodge", color="black", size= 0.05)+
 geom_text(aes(label=n_set), size=3, nudge_y= 0.035, check_overlap = T)+
 scale_x_date(date_breaks ="1 year", date_labels = "%Y")+
 scale_y_continuous(name = "Species occurence in sets (%)")+
 scale_fill_manual(values=c("darkorange", "white"))+
 scale_color_manual(values=c("red", "blue"))+
 labs(y="Species occurence", x="Time", color="Size class",
      caption = "Values represent number of fishing sets used to derived occurence",
      fill = "Size class",
      title = paste0(SPECIES,
                    ifelse(SPECIES=="SKJ", "", paste0("(", SIZE_CLASS, ") ")),
                    " occurence in sampled sets"))+
 theme_classic(base_size = 12)+
 theme(legend.position = "bottom",
       panel.background = element_rect(colour="black"),
       legend.key.size = unit(0.2, "cm"),
       panel.grid = element_line(color="gray", size=0.25, linetype = "dashed"))+
 facet_grid(y~x)
ggsave(filename = file.path(f1_OUTPUTS, "1-time_series_of_species_occurence.jpg"),
      width = 30, height = 15, dpi = 600, units = "cm")
cat("Done.\n") 
 
 
##### 3. Overall  species composition per time----------------
# Lookup quarterly/monthly average of species composition
if(FIll_IN_MISSING_DATA)
{
  cat(crayon::green("\t\t     3.3.2. Time evolution of overall FOB species occurence....."))
  timescale_aggr_occurence <- read.csv2(file.path(f1_OUTPUTS, "time_aggregated_species_occurence.csv"), stringsAsFactors = F)%>%
    dplyr::mutate(y = factor(y, levels = sort(unique(y), decreasing = T), ordered = T),
                  timestamp = dplyr::case_when(
                      toupper(TIME_SCALE) == "QUARTER" ~ as.Date(as.POSIXct(zoo::as.yearqtr(paste0("2000-Q", timescale),format = "%Y-Q%q"))),
                      toupper(TIME_SCALE) == "MONTH"   ~ as.Date(as.POSIXct(zoo::as.yearmon(paste0("01", timescale, "2000"),format = "%d%B%Y")))))
  

  ggplot(data =timescale_aggr_occurence, 
         aes(x= timestamp+31,
             y = occurence))+
    geom_col(size = 0.5, fill="ivory", color="black")+
    geom_text(aes(label=n_set), nudge_y = 0.05, size=3, check_overlap = T)+
    scale_x_date(date_breaks ="1 months", date_labels = "%b")+
    labs(y="Species occurence", x="Year", 
         caption = "Values represent the number of fishing sets used to derive the occurence.",
         title = paste0("Quarterly ", SPECIES,
                        ifelse(SPECIES=="SKJ", "", paste0("(", SIZE_CLASS, ") ")),
                        " occurence in sampled sets"))+
    theme_classic(base_size = 12)+
    theme(legend.position = "bottom",
          legend.key.size = unit(0.2, "cm"),
          panel.background =  element_rect(color="black"))+
    facet_grid(y~x)
  ggsave(filename = file.path(f1_OUTPUTS, "2-species_occurence_over_timescale.jpg"),
         width = 30, height = 15, dpi = 600, units = "cm")
  cat("Done.\n")
}
 

#### 3. Distribution of average f and buoy number in the different strata----------------
cat(crayon::green("\t\t     3.3.3. Distribution of average proportion of inhabited FOBs and M3I buoy number in the different stratum...."))
f1Data <- read.csv2(file.path(f1_OUTPUTS, "aggregated_fadData.csv"), stringsAsFactors = F)

f1_df <- reshape::melt(data = as.data.frame(subset(f1Data, year %in% YEARS, 
                                                      select = c("year", "zone", "timescale", "avg_f1", "buoyCount"))),
                          measure.vars = c("avg_f1", "buoyCount"))
f1_df$variable <- plyr::mapvalues(x = f1_df$variable , 
                                     from = c("avg_f1", "buoyCount"),  
                                     to = c("Daily average proportion of inhabited FOBs (tons)", "Daily average number of M3I buoys"))
infoDf <-  f1_df%>%
  dplyr::group_by(variable)%>%
  dplyr::summarise(avg=mean(value, na.rm = T),
                   max=max(value, na.rm = T), 
                   label = round(avg, 2),
                   .groups="keep")

ggplot(data = f1_df, aes(x=value))+
  geom_histogram(fill="lightblue", color="black", size= 0.2)+
  geom_vline(data = infoDf, aes(xintercept=avg, group=variable), color="red", linetype="dashed", size=1)+
  geom_text(data = infoDf,
            aes(x = avg,
                y = 0,
                label = label,
                group=variable), size=3, fontface="bold")+
  facet_wrap(~variable, scales = "free")+
  theme_linedraw(base_size = 8)+
  labs(title="Distribution of f and number of M3I buoys sets", x="")+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour="gray", size = 0.25, linetype = "dashed"),
        panel.grid.minor.y = element_line(colour="gray", size = 0.25, linetype = "dashed"))
ggsave(filename = file.path(f1_OUTPUTS, "3-dailyBuoyNumber_and_f_Distribution.jpg"),
       width = 16, height = 8, dpi = 600, units = "cm")
cat("Done.\n")

#### 4. Time series of corrected and uncorrected f----------------
cat(crayon::green("\t\t     3.3.4. Time series of corrected and uncorrected f in the different stratum....."))
f1Data <- read.csv2(file.path(f1_OUTPUTS, "aggregated_fadData.csv"), stringsAsFactors = F)%>%
  dplyr::mutate(timestamp = case_when(
    toupper(TIME_SCALE) == "QUARTER" ~ as.Date(as.POSIXct(zoo::as.yearqtr(paste0(year, "-Q", timescale),format = "%Y-Q%q"))),
    toupper(TIME_SCALE) == "MONTH"   ~ as.Date(as.POSIXct(zoo::as.yearmon(paste0("01", timescale, year),format = "%d%B%Y")))))
   
ggdf <-  melt_nd(data=f1Data[, c("timestamp", "x", "y", "avg_f1", "avg_f1_uncorrected", "sd_f1", "sd_f1_uncorrected")], 
                 id.vars = c("timestamp", "x", "y"), 
                 measure.vars = list(avg = c("avg_f1", "avg_f1_uncorrected"),
                                     se  = c("sd_f1", "sd_f1_uncorrected")))
ggdf <- ggdf %>%
  dplyr::mutate(variable.avg = plyr::mapvalues(x = ggdf$variable.avg, 
                                               from= c("avg_f1", "avg_f1_uncorrected"),
                                               to = c(paste0("Tuna aggregation with more than ", BIOMASS_OCCURENCE_THRESHOLD, " kg of ", SPECIES, 
                                                             ifelse(SPECIES=="SKJ", "", paste0("(", SIZE_CLASS, ") "))), 
                                                      "Tuna aggregation")),
                y = factor(ggdf$y, levels = sort(unique(y), decreasing = T), ordered = T),
                x = as.factor(x))

ggplot(data=ggdf, aes(x=timestamp, y=avg, color=variable.avg))+
  geom_errorbar(aes(x=timestamp, ymin=avg-se, ymax=avg+se), width=30)+
  geom_line (size=0.6, na.rm=T)+
  geom_point(shape=21, fill="white", size=0.9)+
  scale_color_manual(values=c("orange", "blue"))+
  labs(x="Time", y="Catches (tons)", 
       fill="",linetype="", color="", 
       title =  "Proportion of FOBs inhabited by tunas (f)")+
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", date_minor_breaks = "1 year")+
  theme_classic(base_size = 9)+
  theme(panel.background = element_rect(colour="black"),
        legend.position = "bottom")+
  facet_grid(y~x, scales = "free")
ggsave(filename = file.path(f1_OUTPUTS, "4-corrected_and_uncorrected_f_time_series.png"),
       width = 30, height = 18, dpi = 300, units="cm")

#### 5. scattergraph of time series of catches---------------------------------------------------------------------------------
fob_DATA <- read.csv2(file.path(OUTPUTS_DIR, "modelInputs_timeSeries.csv"), stringsAsFactors = F)%>%
  dplyr::mutate(timestamp = as.Date(timestamp))
if(is.numeric(SPATIAL_SCALE))
{
  # summary
  fobAvg <- fob_DATA%>%
    dplyr::group_by(zone, x, y)%>%
    dplyr::summarise(avg=mean(f1, na.rm=T), .groups="keep")
  
  worldmap <- ggplot()+
    geom_tile(data=fobAvg, aes(x, y, fill=avg))+
    geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory")+
    labs(x="Longitude", y="Latitude", fill="Average proportion of inhabited FOBs")+
    scale_fill_distiller(palette="Spectral", trans="log10")+
    theme_linedraw(base_size = 9)+
    theme(panel.background = element_rect(color="black", fill="white"),
          panel.grid.major  = element_line(colour = "dimgray", linetype = "dashed", size=0.2),
          panel.grid.minor  = element_blank(),
          axis.ticks = element_line(size=1, colour="black"),
          legend.position = "bottom")
  
  geom_scattergraph(p = worldmap,
                    lon = fob_DATA$x,
                    lat = fob_DATA$y,
                    x  = fob_DATA$timestamp,
                    y  = fob_DATA$f1,
                    se = fob_DATA$se_f1,
                    geom = geom_point(shape=21, color="black", fill="white", size=0.5, stroke=0.2),
                    size = 0.5,
                    graph_size = SPATIAL_SCALE,
                    color = "black", linetype="solid")
  
  # ggdf <- na.omit(ggdf)
  # geom_scattergraph_v3(map = worldmap,
  #                       lon = as.numeric(as.character.factor(ggdf$x)),
  #                       lat = as.numeric(as.character.factor(ggdf$y)),
  #                       x   = ggdf$timestamp,
  #                       y   = ggdf$avg,
  #                       se  = ggdf$se,
  #                       group = ggdf$variable.avg,
  #                       size = 0.5,
  #                       cellSize = SPATIAL_SCALE,
  #                       linetype="solid")
  ggsave(filename = file.path(f1_OUTPUTS, paste0("5-f1_over_year.jpg")),
         width = 30, height = 20, dpi = 1200, units="cm")
}
cat("Done.\n")

#### Verbosity
cat(crayon::green("\t    - Graphical outputs available in folder:\n"), crayon::blue$underline$italic(f1_OUTPUTS))