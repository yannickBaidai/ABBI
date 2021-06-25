#'#*******************************************************************************************************************
#'@title : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2021-06-24
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  Analysis on fob density  and related data
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

cat(crayon::italic("\n\t+ 3.4. Analysing FOB density and related data <m>.\n"))

#### 1. Distribution of buoy density for the defined space-time strata-----------------------------------------
cat(crayon::green("\t\t     3.4.1. Distribution of buoy density ad M3I buoy number....."))

buoyDensData <- read.csv2(file.path(Nfob_OUTPUTS, "aggregated_dailyBuoyDensityData.csv"), stringsAsFactors = F)
buoyDens_df <- reshape::melt(data = as.data.frame(subset(buoyDensData[, c(1:3,6,10)],  year %in% YEARS)),
                             id.vars = c("year", "timescale", "zone"))

buoyDens_df$variable <- plyr::mapvalues(x = buoyDens_df$variable , 
                                        from = c("dailyAvgDens", "avgM3I"),  
                                        to = c("daily average buoy number", "daily average M3I number"))
buoyDens_df$value <- ifelse(is.nan(buoyDens_df$value ), 0, buoyDens_df$value)
infoDf <-  buoyDens_df%>%
  dplyr::group_by(variable)%>%
  dplyr::summarise(avg=mean(value), 
                   max=max(value), 
                   label = round(mean(value)),
                   .groups="keep")

require(ggplot2)                  
ggplot(data = buoyDens_df, aes(x=value))+
  geom_histogram(fill="lightblue", color="black", position = position_nudge(x = 5), size=0.1, bins = 50)+
  geom_vline(data = infoDf, aes(xintercept=avg, group=variable), color="red", linetype="dashed", size=0.5)+
  geom_text(data = infoDf,
            aes(x = avg, y = 0, label = label, group=variable), fontface="bold", size = 3)+
  facet_wrap(~variable, scales = "free")+
  theme_linedraw(base_size = 8)+
  labs(title=paste("Distribution of buoy number per", time_lbl, space_lbl), x="")+
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour="gray", size = 0.25, linetype = "dashed"),
        panel.grid.minor.y = element_line(colour="gray", size = 0.25, linetype = "dashed"))
ggsave(filename = file.path(Nfob_OUTPUTS, "1-buoyNumber_Distribution.jpg"),
       width = 16, height = 8, dpi = 600, units = "cm")
cat("Done.\n")


#### 2. Time series of French proportion of buoys (relative to UE buoys) from katara and al. 2018 -----------------
cat(crayon::green("\t\t     3.4.2. Time series of French proportion of buoys (relative to UE buoys)......"))
katara_data <- read.csv2(file.path(Nfob_OUTPUTS, "katara_data.csv"), stringsAsFactors = F)    

ggplot(katara_data, aes(x=as.Date(date), y=100*french_prop))+
  labs(x="Time", y ="Percentage of French DFADs relative to UE buoys (%)", caption = "*from katara and al. 2018")+
  geom_line(size=1)+
  geom_point(shape=21, fill="white", size=2)+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "3 months")+
  theme_classic(base_size = 16)+
  theme(panel.background = element_rect(colour="black"))
ggsave(filename = file.path(Nfob_OUTPUTS, "2-french_fad_prop_over_year(from_digitized_katara2018).png"),
       width = 12, height = 8, dpi=1200)
cat("Done.\n")


#### 3. Time series of log and DFADs proportion  ------------------------------------------------------------------
cat(crayon::green("\t\t     3.4.3. Time series of log/DFADs proportion....."))

logFad_prop <- read.csv2(file.path(Nfob_OUTPUTS, "logFad_prop"), stringsAsFactors = F)
ggdf <- reshape::melt.data.frame(data = logFad_prop, id.vars = c("year", "raisingLog"))
ggdf$variable <- plyr::mapvalues(x = ggdf$variable, from=c("Logs"), to = c("Other"))

ggplot(data=ggdf, aes(x=year, y=value, fill=variable))+
  labs(x="Time (years)", y="Percentage (%)", fill= "FOB type:")+
  geom_bar(stat="identity", color="dimgray", size= 0.2)+
  scale_x_continuous(breaks = YEARS)+
  scale_y_continuous(breaks=seq(0, 100, 10))+
  scale_fill_brewer(palette="Greys")+
  theme_classic(base_size = 12)+
  theme(panel.background = element_rect(colour="black"),
        legend.position = "right")
ggsave(filename = file.path(Nfob_OUTPUTS, "3-log_fad_proportion_over_year.jpg"),
       width = 12, height = 8, dpi = 1200, units ="cm")
cat("Done.\n")



#### 4. Temporal evolution of buoys/logs and FOBs in the study area ----------------------------------------------
cat(crayon::green("\t\t     3.4.4. Time series of FOBs....."))

df <- as.data.frame(read.csv2(file.path(OUTPUTS_DIR, "modelInputs_timeSeries.csv"), stringsAsFactors = F)%>%
      dplyr::select(x,y,timestamp, buoyDensity, NFad, NLog, NFob, sd_dens, se_NFad, se_NLog, se_NFob))%>%
      dplyr::mutate(timestamp = as.Date(timestamp))

smry <- df%>%
  dplyr::group_by(x,y)%>%
  dplyr::summarise(avg = mean(NFob, na.rm=T),
                   .groups="keep")

df <- melt_nd(data = df, id.vars = c("x", "y","timestamp"), 
              measure.vars = list(avg = c("buoyDensity", "NFad", "NLog", "NFob"),
                                  std = c("sd_dens", "se_NFad", "se_NLog", "se_NFob")))
df$y <- factor(x = df$y, levels = unique(sort(df$y, decreasing = T)), ordered = T)
df$variable.avg <- plyr::mapvalues(x=df$variable.avg,
                                   from = c("buoyDensity", "NFad", "NLog", "NFob"),
                                   to   = c("French active buoys", "DFADs", "Other", "FOBs"))
df$variable.avg <- factor(x = df$variable.avg, levels =c("French active buoys", "DFADs", "Other", "FOBs"), ordered = T)


if(is.numeric(SPATIAL_SCALE))
{
  # ScatterGraphs----------------
  (worldmap <- ggplot()+
     geom_tile(data=smry, aes(x, y, fill=avg))+
     geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory")+
     labs(x="Longitude", y="Latitude", fill=paste("Average estimated number of FOBs"))+
     scale_fill_distiller(palette="Spectral", 
                          guide = guide_colorbar(title.position = "top", title.hjust=0.5, 
                                                 barwidth=30, ticks.linewidth = 1.5, ticks.colour = "black", frame.colour="black"))+
     theme_linedraw(base_size = 12)+
     theme(panel.background = element_rect(color="black", fill="white"),
           panel.grid.major  = element_line(colour = "dimgray", linetype = "dashed", size=0.4),
           panel.grid.minor  = element_blank(),
           axis.ticks = element_line(size=1, colour="black"),
           legend.position = "bottom"))
  
  # Scattergraphs
  geom_scattergraph_v3(map = worldmap,
                       lon = df$x,
                       lat = as.numeric(as.character.factor(df$y)),
                       x   = df$timestamp,
                       y   = df$avg,
                       group = df$variable.avg,
                       se   = df$std,
                       size=0.7,
                       cellSize = 10,
                       linetype="solid")
  ggsave(filename = file.path(Nfob_OUTPUTS, "4-Quarterly_average_of_p.png"),
         width = 30, height = 20, dpi = 1200, units="cm")
}else
{
  ggplot(df, aes(x=timestamp, y=avg, color=variable.avg))+
    geom_errorbar(aes(ymin=avg-std, ymax=avg+std, color=variable.avg), width=40,na.rm = T)+
    geom_line(size=0.8,na.rm = T)+
    geom_point(shape=21, fill="white", size=2,na.rm = T)+
    scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
    scale_y_continuous(position = "right")+
    scale_color_brewer(palette = "Set1")+
    facet_grid(y~x, switch = "y", scales = "free_y")+
    labs(x="Time", y="Estimated number of FOBs", color="")+
    theme_minimal(base_size = 24)+
    theme(panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(fill=NA, color="transparent"),
          panel.grid = element_blank(),
          legend.position = "bottom",
          axis.ticks = element_line(colour="black", size=1),
          strip.background = element_rect(color = "black"),
          panel.spacing = unit(2, "lines"))
  ggsave(filename = file.path(Nfob_OUTPUTS, "4-Quarterly_average_of_p.png"), width = 18, height = 9.5, dpi=1200)
}
cat("Done.\n")

#### Verbosity
cat(crayon::green("\t    - Graphical outputs available in folder:\n"), crayon::blue$underline$italic(Nfob_OUTPUTS))