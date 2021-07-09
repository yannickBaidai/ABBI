#'#*******************************************************************************************************************
#'@title : SKJ abundance Index based on their associative behaviour
#'@author : Yannick BAIDAI
#'@update : 2021-06-24
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  Abundance estimates graphical outputs and analysis
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

cat(crayon::italic("\n\t+ 3.5. Abundance estimates graphical outputs and analysis.\n"))

#### 1. Time series of CAT ----------------------------------------------------------------------------------
cat(crayon::green("\t\t     3.5.1. Time series of CAT...."))
CAT <- read.csv2(file.path(OUTPUTS_DIR, "abundance_estimates.csv"), stringsAsFactors = F)%>%
  dplyr::filter(CRT==CRT_THEO)%>%
  dplyr::mutate(timestamp = as.Date(timestamp),
                y = factor(x = y, levels = unique(sort(y, decreasing = T)), ordered = T))%>%
  dplyr::select(zone, x, y,timestamp, CAT, phi)

ggplot(CAT, aes(x=timestamp, y=CAT, color=as.factor(phi)))+
  geom_line(size=0.8,na.rm = T)+
  geom_point(shape=21, fill="white", size=2,na.rm = T)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_y_continuous(position = "right")+
  scale_color_brewer(palette = "Paired")+
  facet_grid(y~x, switch = "y", scales = "free_y")+
  labs(x="Time (Quarters)", y="CAT (days)", color=expression(phi))+
  theme_minimal(base_size = 24)+
  theme(panel.background = element_rect(color="black", fill="white"),
        plot.background = element_rect(fill=NA, color="transparent"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_line(colour="black", size=1),
        strip.background = element_rect(color = "black"),
        panel.spacing = unit(2, "lines"))
ggsave(filename = file.path(OUTPUTS_DIR, "1.1-Quarterly_average_of_CAT.png"), width = 18, height = 9.5, dpi=1200)

### ScatterGraphs
if(is.numeric(SPATIAL_SCALE))
{
  smry <- CAT%>%
    dplyr::mutate(y=as.numeric(as.character.factor(y)))%>%
    dplyr::group_by(x,y,phi)%>%
    dplyr::summarise(avg = mean(CAT, na.rm = T),
                     sd_  = sd(CAT, na.rm=T),
                     .groups="keep")
  
  phi_value <- min(smry$phi)
  (worldmap <- ggplot()+
      geom_tile(data=subset(smry, phi==2e-5), aes(x, y, fill=avg))+
      geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory")+
      labs(x="Longitude", y="Latitude", fill=paste("Average CAT (", brr:::greek_utf8("phi"), "=", phi_value, ") over 2013-2019"))+
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
                       lon = CAT$x,
                       lat = as.numeric(as.character.factor(CAT$y)),
                       x   = CAT$timestamp,
                       y   = CAT$CAT,
                       group = as.factor(CAT$phi),
                       size=0.7,
                       cellSize = 10,
                       linetype="solid")
  
  ggsave(filename = file.path(OUTPUTS_DIR, paste0("1.2-CAT_timeseries.jpg")),
         width = 30, height = 20, dpi = 1200, units="cm")
}
cat("Done.\n")


#### 2. Quarterly average of <Nassoc>  ----------------------------------------------------------------------------------
cat(crayon::green("\t\t     3.5.2. Time series of Nassoc...."))

df <- as.data.frame(read.csv2(file = file.path(OUTPUTS_DIR, "abundance_estimates.csv"), stringsAsFactors = F)%>%
                      dplyr::mutate(timestamp = as.Date(timestamp))%>%
                      dplyr::select(x,y,timestamp, Nassoc, se_Nassoc))

smry <- df%>%
  dplyr::group_by(x,y)%>%
  dplyr::summarise(avg = mean(Nassoc, na.rm = T),
                   sd_  = sd(Nassoc, na.rm=T),
                   .groups="keep")
df$y <- factor(x = df$y, levels = unique(sort(df$y, decreasing = T)), ordered = T)

ggplot(df, aes(x=timestamp, y=Nassoc))+
  geom_errorbar(aes(ymin=Nassoc-se_Nassoc, ymax=Nassoc+se_Nassoc), width=40)+
  geom_line(size=0.8,na.rm = T)+
  geom_point(shape=21, fill="white", size=2,na.rm = T)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_y_continuous(position = "right")+
  scale_color_brewer(palette = "Paired")+
  facet_grid(y~x, switch = "y", scales = "free_y")+
  labs(x="Time (Quarters)", y="Biomass (tons)", color=expression(phi))+
  theme_minimal(base_size = 24)+
  theme(panel.background = element_rect(color="black", fill="white"),
        plot.background = element_rect(fill=NA, color="transparent"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_line(colour="black", size=1),
        strip.background = element_rect(color = "black"),
        panel.spacing = unit(2, "lines"))
ggsave(filename = file.path(OUTPUTS_DIR, "2.1-Absolute_N_assoc.png"), width = 18, height = 9.5, dpi=1200)


# ScatterGraphs
if(is.numeric(SPATIAL_SCALE))
{
  (worldmap <- ggplot()+
     geom_tile(data=smry, aes(x, y, fill=avg))+
     geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory")+
     labs(x="Longitude", y="Latitude", fill=paste("Average associated YFT(-10kg) biomass over 2013-2019"))+
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
                       y   = df$Nassoc,
                       group = NULL,
                       se   = df$se_Nassoc,
                       size=0.7,
                       cellSize = 10,
                       linetype="solid")
  ggsave(filename = file.path(OUTPUTS_DIR, "2.2-Absolute_Nassoc.png"),
         width = 30, height = 20, dpi = 1200, units="cm")  
}
cat("Done.\n")

#### 3. Quarterly average of <Nfree>  ----------------------------------------------------------------------------------
cat(crayon::green("\t\t     3.5.3. Time series of Nfree...."))

df <- as.data.frame(read.csv2(file = file.path(OUTPUTS_DIR, "abundance_estimates.csv"), stringsAsFactors = F)%>%
                      dplyr::mutate(timestamp = as.Date(timestamp))%>%
                      dplyr::select(x,y,timestamp, Nfree, se_Nfree, phi, CRT))
smry <- df%>%
  dplyr::group_by(x,y, phi)%>%
  dplyr::summarise(avg  = mean(Nfree, na.rm = T),
                   sd_  = sd(Nfree, na.rm=T),
                   .groups="keep")
df$y <- factor(x = df$y, levels = unique(sort(df$y, decreasing = T)), ordered = T)

ggplot(df, aes(x=timestamp, y=Nfree, color=as.factor(phi)))+
  geom_errorbar(aes(ymin=Nfree-se_Nfree, ymax=Nfree+se_Nfree), width=40)+
  geom_line(size=0.8,na.rm = T)+
  geom_point(shape=21, fill="white", size=2,na.rm = T)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_y_continuous(position = "right")+
  scale_color_brewer(palette = "Paired")+
  facet_grid(CRT+y~x, switch = "y", scales = "free_y")+
  labs(x="Time (Quarters)", y="Biomass (tons)", color=expression(phi))+
  theme_minimal(base_size = 12)+
  theme(panel.background = element_rect(color="black", fill="white"),
        plot.background = element_rect(fill="white", color="white"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_line(colour="black", size=1),
        strip.background = element_rect(color = "black"),
        panel.spacing = unit(2, "lines"))
ggsave(filename = file.path(OUTPUTS_DIR, "3.1-Absolute_Nfree(CRT_sens).png"), width = 30, height = 20, dpi=600)

df <- subset(df, CRT==CRT_THEO)
ggplot(df, aes(x=timestamp, y=Nfree, color=as.factor(phi)))+
  geom_errorbar(aes(ymin=Nfree-se_Nfree, ymax=Nfree+se_Nfree), width=40)+
  geom_line(size=0.8,na.rm = T)+
  geom_point(shape=21, fill="white", size=2,na.rm = T)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_y_continuous(position = "right")+
  scale_color_brewer(palette = "Paired")+
  facet_grid(y~x, switch = "y", scales = "free_y")+
  labs(x="Time (Quarters)", y="Biomass (tons)", color=expression(phi))+
  theme_minimal(base_size = 24)+
  theme(panel.background = element_rect(color="black", fill="white"),
        plot.background = element_rect(fill="white", color="white"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_line(colour="black", size=1),
        strip.background = element_rect(color = "black"),
        panel.spacing = unit(2, "lines"))
ggsave(filename = file.path(OUTPUTS_DIR, "3.2-Absolute_Nfree.png"), width = 18, height = 9.5, dpi=1200)

# ScatterGraphs
if(is.numeric(SPATIAL_SCALE))
{
  (worldmap <- ggplot()+
     geom_tile(data=subset(smry, phi==2e-5), aes(x, y, fill=avg))+
     geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory")+
     labs(x="Longitude", y="Latitude", fill=paste0("Average biomass of free-swimming YFT-10kg (", brr:::greek_utf8("phi"), "=2e-5) over 2013-2019"))+
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
                       y   = df$se_Nfree,
                       group = as.factor(df$phi),
                       se   = df$se_Nfree,
                       size=0.5,
                       cellSize = 10,
                       linetype="solid")
  ggsave(filename = file.path(OUTPUTS_DIR, "3.3-Absolute_Nfree.png"),
         width = 30, height = 20, dpi = 1200, units="cm")
}
cat("Done.\n")

#### 4. Quarterly average of <Ntotal>   ----------------------------------------------------------------------------------
cat(crayon::green("\t\t     3.5.4. Time series of Ntotal...."))

df <- as.data.frame(read.csv2(file = file.path(OUTPUTS_DIR, "abundance_estimates.csv"), stringsAsFactors = F)%>%
                      dplyr::mutate(timestamp = as.Date(timestamp))%>%
                      dplyr::select(x,y,timestamp, total_biomass, se_total_biomass, phi, CRT))

smry <- df%>%
  dplyr::group_by(x, y, phi, CRT)%>%
  dplyr::summarise(avg = mean(total_biomass, na.rm = T),
                   sd_  = sd(total_biomass, na.rm=T),
                   .groups="keep")
df$y <- factor(x = df$y, levels = unique(sort(df$y, decreasing = T)), ordered = T)

ggplot(df, aes(x=timestamp, y=total_biomass, color=as.factor(phi)))+
  geom_errorbar(aes(ymin=total_biomass-se_total_biomass, ymax=total_biomass+se_total_biomass), width=40)+
  geom_line(size=0.8,na.rm = T)+
  geom_point(shape=21, fill="white", size=2,na.rm = T)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_y_continuous(position = "right")+
  scale_color_brewer(palette = "Paired")+
  facet_grid(CRT+y~x, switch = "y", scales = "free_y")+
  labs(x="Time (Quarters)", y="Biomass (tons)", color=expression(phi))+
  theme_minimal(base_size = 12)+
  theme(panel.background = element_rect(color="black", fill="white"),
        plot.background = element_rect(fill=NA, color="transparent"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_line(colour="black", size=1),
        strip.background = element_rect(color = "black"),
        panel.spacing = unit(2, "lines"))
ggsave(filename = file.path(OUTPUTS_DIR, "4.1-absolute_Ntotal(CRT_sens).png"), width = 30, height = 20, dpi=600)

df <- subset(df, CRT==CRT_THEO)
ggplot(df,  aes(x=timestamp, y=total_biomass, color=as.factor(phi)))+
  geom_errorbar(aes(ymin=total_biomass-se_total_biomass, ymax=total_biomass+se_total_biomass), width=40)+
  geom_line(size=0.8,na.rm = T)+
  geom_point(shape=21, fill="white", size=2,na.rm = T)+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  scale_y_continuous(position = "right")+
  scale_color_brewer(palette = "Paired")+
  facet_grid(CRT+y~x, switch = "y", scales = "free_y")+
  labs(x="Time (Quarters)", y="Biomass (tons)", color=expression(phi))+
  theme_minimal(base_size = 24)+
  theme(panel.background = element_rect(color="black", fill="white"),
        plot.background = element_rect(fill=NA, color="white"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        axis.ticks = element_line(colour="black", size=1),
        strip.background = element_rect(color = "black"),
        panel.spacing = unit(2, "lines"))
ggsave(filename = file.path(OUTPUTS_DIR, "4.2-absolute_Ntotal.png"), width = 18, height = 9.5, dpi=600)

#### Scattergraph
if(is.numeric(SPATIAL_SCALE))
{
  (worldmap <- ggplot()+
     geom_tile(data=subset(smry, phi==2e-5), aes(x, y, fill=avg))+
     geom_map(data=world, map=world,aes(map_id =region),fill = "dimgray", color="ivory")+
     labs(x="Longitude", y="Latitude", fill=paste0("Average biomass of YFT-10kg (", brr:::greek_utf8("phi"), "=2e-5) over 2013-2019"))+
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
                       y   = df$total_biomass,
                       group = as.factor(df$phi),
                       se   = df$se_total_biomass,
                       size=0.7,
                       cellSize = 10,
                       palette="Set1",
                       linetype="solid")
  ggsave(filename = file.path(OUTPUTS_DIR, "4.3-absolute_Ntotal.png"),
         width = 30, height = 20, dpi = 1200, units="cm")
}
cat("Done.\n")


#### 5. Global average abundance ------------------------------------------------------------------------
cat(crayon::green("\t\t     3.5.5. Time series of global average abundance...."))

df <- as.data.frame(read.csv2(file = file.path(OUTPUTS_DIR, "globalAbundanceAvg.csv"), stringsAsFactors = F)%>%
                      dplyr::filter(CRT==CRT_THEO)%>%
                      dplyr::mutate(timestamp = as.Date(timestamp)))


df <- melt_nd(data = df,id.vars = c("timestamp", "phi"),
              measure.vars = list(avg= c("total_biomass", "Nassoc", "Nfree", "Itotal", "Iassoc", "Ifree"),
                                  std = c("total_biomass.sd", "Nassoc.sd", "Nfree.sd", "Itotal.sd", "Iassoc.sd", "Ifree.sd")))

df$phi      <- ifelse(df$variable.avg=="Nassoc" | df$variable.avg == "Iassoc", NA, df$phi)  
df$index    <- ifelse(df$variable.avg == "Itotal" | df$variable.avg == "Ifree" | df$variable.avg == "Iassoc",
                      "Relative abundance", "Absolute abundance")
df$variable.avg <- plyr::mapvalues(x = df$variable.avg, 
                                   from = c("total_biomass", "Nfree", "Nassoc", "Itotal", "Ifree", "Iassoc"),
                                   to = rep(c(paste("Total population"), 
                                              paste("Free-swimming population"),
                                              paste("Associated population")),2))


#### 5.1. Associated Population -----------------------
var <- "Associated population"
df_sub <- subset(df, index=="Absolute abundance" & variable.avg==var)

(Nass_abs <- ggplot(data=df_sub, aes(x=timestamp, y=avg))+
    geom_line(size=0.8, show.legend = F)+
    geom_errorbar(aes(x=timestamp, ymin=avg-std, ymax=avg+std), width=40)+
    geom_point(shape=21, fill="white", size=1.5)+
    
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=2))+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    scale_alpha_manual(values=c(1, 1, 1, rep(0.3, 9)))+
    scale_colour_brewer(palette = "Paired", na.value="black")+
    labs(x="Time", y="Biomass (tons)", color=expression(phi), alpha=expression(phi))+
    theme_classic(base_size = 16)+
    theme(panel.background = element_rect(colour = "black", fill="white"),
          panel.grid.major =  element_line(colour ="white", size=0.5, linetype = "dashed"),
          legend.position = "none")+
    facet_grid(variable.avg~., scales = "free_y"))

df_sub <- subset(df, index=="Relative abundance" & variable.avg==var)
(Nass_rel <- ggplot(data=df_sub, aes(x=timestamp, y=avg))+
    geom_line(size=0.8, show.legend = F)+
    geom_errorbar(aes(x=timestamp, ymin=avg-std, ymax=avg+std), width=40)+
    geom_point(shape=21, fill="white", size=1.5)+
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    scale_alpha_manual(values=c(1, 1, 1, rep(0.3, 9)))+
    geom_hline(yintercept = 1, linetype="dashed", color="black", size=1)+
    scale_colour_brewer(palette = "Paired", na.value="black")+
    labs(x="Time", y="Index", color=expression(phi), alpha=expression(phi))+
    theme_classic(base_size = 16)+
    theme(panel.background = element_rect(colour = "black", fill="white"),
          panel.grid.major =  element_line(colour ="white", size=0.5, linetype = "dashed"),
          legend.position = "none")+
    facet_grid(variable.avg~., scales = "free_y"))



#### 5.2. Free swimming Population -----------------------
var <- "Free-swimming population"
df_sub <- subset(df, index=="Absolute abundance" & variable.avg==var)
(Nfree_abs <- ggplot(data=df_sub, aes(x=timestamp, y=avg, group =phi))+
    geom_line(aes(alpha=as.factor(phi), color=as.factor(phi)), size=0.8, show.legend = F)+
    geom_errorbar(aes(x=timestamp, ymin=avg-std, ymax=avg+std), width=40, color="black")+
    geom_point(shape=21, fill="white", size=1.5, color="black", alpha=1, show.legend = F)+
    
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=2))+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    scale_alpha_manual(values=c(1, 1, 1, rep(0.3, 9)))+
    scale_colour_brewer(palette = "Paired", na.value="black")+
    labs(x="Time", y="Biomass (tons)", color=expression(phi), alpha=expression(phi))+
    theme_classic(base_size = 16)+
    theme(panel.background = element_rect(colour = "black", fill="white"),
          panel.grid.major =  element_line(colour ="white", size=0.5, linetype = "dashed"),
          legend.position = "none")+
    facet_grid(variable.avg~., scales = "free_y"))

df_sub <- subset(df, index=="Relative abundance" & variable.avg==var)
(Nfree_rel <- ggplot(data=df_sub, aes(x=timestamp, y=avg))+
    geom_line(size=0.8, show.legend = F)+
    geom_errorbar(aes(x=timestamp, ymin=avg-std, ymax=avg+std), width=40)+
    geom_point(shape=21, fill="white", size=1.5, show.legend = F)+
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    scale_alpha_manual(values=c(1, 1, 1, rep(0.3, 9)))+
    geom_hline(yintercept = 1, linetype="dashed", color="black", size=1)+
    scale_colour_brewer(palette = "Paired", na.value="black")+
    labs(x="Time", y="Index", color=expression(phi), alpha=expression(phi))+
    theme_classic(base_size = 16)+
    theme(panel.background = element_rect(colour = "black", fill="white"),
          panel.grid.major =  element_line(colour ="white", size=0.5, linetype = "dashed"),
          legend.position = "none")+
    facet_wrap(~variable.avg, scales = "free_y", strip.position = "right"))



#### 5.3. Total Population -----------------------
var <- "Total population"

df_sub <- subset(df, index=="Absolute abundance" & variable.avg==var)
(Ntot_abs <- ggplot(data=df_sub, aes(x=timestamp, y=avg, group =phi))+
    geom_line(aes(alpha=as.factor(phi), color=as.factor(phi)), size=0.8)+
    geom_errorbar(aes(x=timestamp, ymin=avg-std, ymax=avg+std),
                  color="black", alpha=1, width=40)+
    geom_point(shape=21, fill="white", color="black", alpha=1, size=1.5, show.legend = T)+
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=2))+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    scale_alpha_manual(values=c(1, 1, 1, rep(0.3, 9)))+
    scale_colour_brewer(palette = "Paired", na.value="black")+
    labs(x="Time", y="Biomass (tons)", color=expression(phi), alpha=expression(phi))+
    theme_classic(base_size = 16)+
    theme(panel.background = element_rect(colour = "black", fill="white"),
          panel.grid.major =  element_line(colour ="white", size=0.5, linetype = "dashed"),
          legend.position = "bottom")+#, 
    #strip.background.y = element_blank(), strip.text.y = element_blank())+
    guides(alpha = "none")+
    facet_grid(variable.avg~.))

df_sub <- subset(df, index=="Relative abundance" & variable.avg==var)
(Ntot_rel <- ggplot(data=df_sub, aes(x=timestamp, y=avg, group =phi))+
    geom_line(aes(alpha=as.factor(phi), color=as.factor(phi)), size=0.8)+
    geom_errorbar(aes(x=timestamp, ymin=avg-std, ymax=avg+std, alpha=as.factor(phi)),
                  color="black", alpha=1, width=40)+
    geom_point(shape=21, fill="white", color="black", alpha=1, size=1.5, show.legend = T)+
    geom_hline(yintercept = 1, linetype="dashed", color="black", size=1)+
    
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    scale_alpha_manual(values=c(1, 1, 1, rep(0.3, 9)))+
    scale_colour_brewer(palette = "Paired", na.value="black")+
    labs(x="Time", y="Index", color=expression(phi), alpha=expression(phi))+
    theme_classic(base_size = 16)+
    theme(panel.background = element_rect(colour = "black", fill="white"),
          panel.grid.major =  element_line(colour ="white", size=0.5, linetype = "dashed"),
          legend.position = "none")+
    facet_wrap(~variable.avg, scales = "free_y", strip.position = "right")+
    guides(alpha="none"))



##### Absolute Indices
ggpubr::ggarrange(Nass_abs, Nfree_abs,  Ntot_abs, 
                  ncol = 1, nrow = 3,
                  labels = LETTERS[1:3],
                  # label.x = c(0.2, 0.1, 0.2),
                  # label.y = c(0.75, 0.75, 0.85),
                  font.label = list(size = 16, face = "bold"),
                  common.legend = T, legend = "bottom")
ggsave(filename = file.path(OUTPUTS_DIR, "5.1-absolute_abundance_time_series.png"), width = 20, height = 30, units="cm", dpi=1200)


##### Relative Indices
ggpubr::ggarrange(Nass_rel, Nfree_rel, Ntot_rel,
                  ncol = 1, nrow = 3,
                  labels = LETTERS[1:3],
                  # label.x = c(0.2, 0.1, 0.2, 0.1, 0.2, 0.1),
                  # label.y = c(0.75, 0.75, 0.85, 0.85, 0.85, 0.85 ),
                  font.label = list(size = 16, face = "bold"),
                  common.legend = T, legend = "bottom")
ggsave(filename = file.path(OUTPUTS_DIR, "5.2-relative_abundance_time_series.png"), width = 20, height = 30, units="cm",dpi=1200)


### ALL panels
ggpubr::ggarrange(Nass_abs,  Nass_rel, 
                  Nfree_abs, Nfree_rel, 
                  Ntot_abs,  Ntot_rel, 
                  ncol = 2, nrow = 3,
                  labels = LETTERS[1:6],
                  font.label = list(size = 16, face = "bold"),
                  common.legend = T, legend = "bottom")
ggsave(filename = file.path(OUTPUTS_DIR, "5.3-abundance_time_series.png"), width = 40, height = 30, units="cm", dpi=1200)
cat("Done.\n")
