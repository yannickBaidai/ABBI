


geom_scattergraph_v3 <- function(map, x, y, lon, lat, cellSize = 5,
                                 group =NULL, se=NULL,  ...)
{
  # Building base dataframe
  data <- data.frame(x, y, lon, lat)
  
  if(!is.null(group)){
    data$group <- group
  }
  
  if(!is.null(se)){
    data$se <- se
  }
  
  # get graph size
  graph_size <- cellSize/2
  
  # Get lon and Lat where plot data
  pos <- unique(data[, c("lon", "lat")])
  
  # Adjust box bounding of maps p
  minLon <- min(lon) - (2*graph_size)
  maxLon <- max(lon) + (2*graph_size)
  minLat <- min(lat) - (2*graph_size)
  maxLat <- max(lat) + (2*graph_size)
  
  
  x_breaks <- seq(minLon - (graph_size), maxLon + - (graph_size), (2*graph_size))
  y_breaks <- seq(minLat - (graph_size), maxLat + - (graph_size), (2*graph_size))
  p <- map +
    coord_quickmap(xlim = c(minLon, maxLon), ylim=c(minLat, maxLat))+
    # Scale bar
    ggsn::scalebar(dist = 1000, dist_unit = "km", model = 'WGS84', transform = T, location = "bottomleft",
                   height = 0.03, st.dist = 0.02, st.size = 4,
                   x.min = minLon, x.max = maxLon,
                   y.min = minLat, y.max = maxLat)+
    ggsn::north(location = "topright", scale = 0.1,
                x.min = minLon, x.max = maxLon,
                y.min = minLat, y.max = maxLat)+
    scale_x_continuous(breaks= x_breaks , labels = x_breaks)+
    scale_y_continuous(breaks=y_breaks, labels = y_breaks)
  
  # Empty plot for instantiate grobs
  grobs <- p + annotation_custom(grid::grob(
    ggplot()+
      theme_void()))
  # Adding Grobs  with a loop
  semiTranspWhite1 <- rgb(1, 1, 1, 0.1)
  semiTranspWhite2 <- rgb(1, 1, 1, 0.5)
  
  for(i in 1:nrow(pos))
  {
    Lon <- pos$lon[i]
    Lat <- pos$lat[i]
    df  <- subset(data, lon== Lon & lat==Lat)
    
    # plot
    if(!is.null(se)){
      p   <- ggplot(data=df, aes(x, y, ymin=y-se, ymax=y+se, group=group, color=group))+
        geom_errorbar(...)+      
        geom_line(...)+
        geom_point(size=1, shape=21, fill="white")+
        scale_color_brewer(palette="Set1")+
        # scale_color_brewer(palette="Accent")+
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "3 months")+
        theme_classic(base_size = 10)+
        theme(panel.background = element_rect(colour="dimGray",  fill = semiTranspWhite1),
              plot.background = element_rect(colour = "dimGray", fill = semiTranspWhite2),
              axis.text.x = element_text(angle=90),
              axis.title = element_blank(),
              legend.position = "none")
    }else{
      p   <- ggplot(data=df, aes(x, y, group=group, color=group))+
        geom_line(...)+
        geom_point(size=1, shape=21, fill="white")+
        scale_color_brewer(palette="Set1")+
        # scale_color_brewer(palette="Accent")+
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "3 months")+
        theme_classic(base_size = 5)+
        theme(panel.background = element_rect(colour="dimGray",  fill = semiTranspWhite1),
              plot.background = element_rect(colour = "dimGray", fill = semiTranspWhite2),
              axis.text.x = element_text(angle=90),
              axis.title = element_blank(),
              legend.position = "none")
    }
    
    
    p_grob <- ggplotGrob(p)
    p_anno <- annotation_custom(grob = p_grob, 
                                xmin = Lon-graph_size, xmax = Lon+graph_size, 
                                ymin = Lat-graph_size, ymax = Lat+graph_size)
    grobs <- grobs + p_anno
  }
  
  plot(grobs)
  invisible(return(grobs))
}

#' Title
#'
#' @param p : Ggplot object
#' @param x : data on graph x absicsse
#' @param y : data on graph y absicsse
#' @param lon : vector of longitude where graphs should be plotted (should have the same length that )
#' @param lat : vector of latitude  where graphs should be plotted
#' @param graph_size : graph size (should be equal to cell resolution)
#' @param ... : arguments for graphs
#'
#' @return
#' @export
#'
#' @examples :geom_scattergraph(p = p1, 
#'                              lon = fob_DATA$x,
#'                              lat = fob_DATA$y,
#'                              x = fob_DATA$timestamp,
#'                              y = fob_DATA$catches,
#'                              size = 5)
#' 
geom_scattergraph <- function(p, x, y, se=NULL, pointFill=NULL,  lon, lat,  graph_size = 5, geom=NULL, geom1 =NULL, geom2=NULL, geom3=NULL, ...)
{
  if(!is.null(se)){
     data <- data.frame(x, y, se, lon, lat)
  }else{
    data <- data.frame(x, y, lon, lat)
  }
 
  if(!is.null(pointFill)){
    data$pointFill <- pointFill
  }

  
  graph_size <- graph_size/2
  # Get lon and Lat where plot data
  pos <- unique(data[, c("lon", "lat")])

  # Adjust box bounding of maps p
  minLon <- min(lon) - (2*graph_size)
  maxLon <- max(lon) + (2*graph_size)
  minLat <- min(lat) - (2*graph_size)
  maxLat <- max(lat) + (2*graph_size)
  
  
  x_breaks <- seq(minLon - (graph_size), maxLon + - (graph_size), (2*graph_size))
  y_breaks <- seq(minLat - (graph_size), maxLat + - (graph_size), (2*graph_size))
  p <- p +
      coord_quickmap(xlim = c(minLon, maxLon), ylim=c(minLat, maxLat))+
    # Scale bar
    ggsn::scalebar(dist = 1000, dist_unit = "km", model = 'WGS84', transform = T, location = "bottomleft",
                   height = 0.03, st.dist = 0.02, st.size = 4,
                   x.min = minLon, x.max = maxLon,
                   y.min = minLat, y.max = maxLat)+
    ggsn::north(location = "topright", scale = 0.2,
                x.min = minLon, x.max = maxLon,
                y.min = minLat, y.max = maxLat)+
    scale_x_continuous(breaks= x_breaks , labels = x_breaks)+
    scale_y_continuous(breaks=y_breaks, labels = y_breaks)
  
  # Empty plot for instantiate grobs
  grobs <- p + annotation_custom(grid::grob(
                                ggplot()+
                                  theme_void()))
  # Adding Grobs  with a loop
  semiTranspWhite1 <- rgb(1, 1, 1, 0.2)
  semiTranspWhite2 <- rgb(1, 1, 1, 0.4)
  
  for(i in 1:nrow(pos))
  {
    Lon <- pos$lon[i]
    Lat <- pos$lat[i]
    df  <- subset(data, lon== Lon & lat==Lat)
    
    # plot
    pointGeom <- geom_point(data=df, aes(x, y), shape=21,  size=0.75, stroke=0.2, show.legend = F)
    if(!is.null(pointFill)){
      pointGeom <- geom_point(data=df, aes(x, y, fill=pointFill), shape=21,  size=0.75, stroke=0.2, show.legend = F)
    }

    if(!is.null(se)){
      p   <- ggplot(data=df, aes(x, y, ymin=y-se, ymax=y+se))+
              geom_errorbar(...)  +      
              geom_line(...)+
              pointGeom+
              geom  +
              geom1 +
              geom2 +
              geom3 +
              scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "3 months")+
              theme_classic(base_size = 5)+
              theme(panel.background = element_rect(colour="dimGray",  fill = semiTranspWhite1),
                    plot.background = element_rect(colour = "dimGray", fill = semiTranspWhite2),
                    axis.text.x = element_text(angle=90),
                    axis.title = element_blank())
    }else{
      p   <- ggplot(data=df, aes(x, y))+
              geom_line(...)+
              pointGeom+
              geom  +
              geom1 +
              geom2 +
              geom3 +
              scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "3 months")+
              theme_classic(base_size = 5)+
              theme(panel.background = element_rect(colour="dimGray",  fill = semiTranspWhite1),
                    plot.background = element_rect(colour = "dimGray", fill = semiTranspWhite2),
                    axis.text.x = element_text(angle=90),
                    axis.title = element_blank())
    }
    
    
    p_grob <- ggplotGrob(p)
    p_anno <- annotation_custom(grob = p_grob, 
                                xmin = Lon-graph_size, xmax = Lon+graph_size, 
                                ymin = Lat-graph_size, ymax = Lat+graph_size)
    grobs <- grobs + p_anno
  }
  
  plot(grobs)
  invisible(return(grobs))
}



# geom_scattergraph(p = p1, 
#                   lon = fob_DATA$x,
#                   lat = fob_DATA$y,
#                   x = fob_DATA$timestamp,
#                   y = fob_DATA$catches,
#                   graph_size = 5,
#                   color ="red", linetype="dashed")


geom_scattergraph2 <- function(p, x, y, se=NULL, pointFill=NULL, phi=NULL, lon, lat, nominal_catches, graph_size = 5, geom=NULL, geom1 =NULL, geom2=NULL, geom3=NULL, ...)
{
  if(!is.null(se)){
    data <- data.frame(x, y, se, lon, lat)
  }else{
    data <- data.frame(x, y, lon, lat)
  }
  
  if(!is.null(pointFill)){
    data$pointFill <- pointFill
  }
  
  if(!is.null(phi)){
    data$phi <- phi
  }

  
  graph_size <- graph_size/2
  # Get lon and Lat where plot data
  pos <- unique(data[, c("lon", "lat")])
  
  # Adjust box boundig of maps p
  minLon <- min(lon) - (2*graph_size)
  maxLon <- max(lon) + (2*graph_size)
  minLat <- min(lat) - (2*graph_size)
  maxLat <- max(lat) + (2*graph_size)
  
  
  x_breaks <- seq(minLon - (graph_size), maxLon + - (graph_size), (2*graph_size))
  y_breaks <- seq(minLat - (graph_size), maxLat + - (graph_size), (2*graph_size))
  p <- p +
    coord_quickmap(xlim = c(minLon, maxLon), ylim=c(minLat, maxLat))+
    # Scale bar
    ggsn::scalebar(dist = 1000, dist_unit = "km", model = 'WGS84', transform = T, location = "bottomleft",
                   height = 0.03, st.dist = 0.02, st.size = 4,
                   x.min = minLon, x.max = maxLon,
                   y.min = minLat, y.max = maxLat)+
    ggsn::north(location = "topright", scale = 0.2,
                x.min = minLon, x.max = maxLon,
                y.min = minLat, y.max = maxLat)+
    scale_x_continuous(breaks= x_breaks , labels = x_breaks)+
    scale_y_continuous(breaks=y_breaks, labels = y_breaks)
  
  # Empty plot for instantiate grobs
  grobs <- p + annotation_custom(grid::grob(
    ggplot()+
      theme_void()))
  # Adding Grobs  with a loop
  semiTranspWhite1 <- rgb(1, 1, 1, 0.2)
  semiTranspWhite2 <- rgb(1, 1, 1, 0.4)
  
  for(i in 1:nrow(pos))
  {
    Lon <- pos$lon[i]
    Lat <- pos$lat[i]
    df  <- subset(data, lon== Lon & lat==Lat)
    
    # plot
    pointGeom <- geom_point(data=df, aes(x, y), shape=21,  size=0.75, stroke=0.2, show.legend = F)
    if(!is.null(pointFill)){
      pointGeom <- geom_point(data=df, aes(x, y, fill=pointFill), shape=21,  size=0.75, stroke=0.2, show.legend = F)
    }
    
    # catches
    catchesGeom <-NULL
    catches <- subset(nominal_catches, x==Lon & y==Lat)
    if(nrow(catches)>0)
    {
       catchesGeom <- geom_line(data=catches, aes(x=timestamp, y=catches), color="red",  size=0.3, show.legend = F)
    }

    
    if(!is.null(se)){
      p   <- ggplot()+
        geom_errorbar(data=df, aes(x, y, ymin=y-se, ymax=y+se, group=phi),...)  +      
        geom_line(data=df, aes(x, y, group=phi, color=as.factor(phi)), show.legend = F, ...)+
        scale_color_brewer(palette="Pastel1")+
        pointGeom+
        geom  +
        geom1 +
        geom2 +
        geom3 +
        catchesGeom +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "3 months")+
        theme_classic(base_size = 5)+
        theme(panel.background = element_rect(colour="dimGray",  fill = semiTranspWhite1),
              plot.background = element_rect(colour = "dimGray", fill = semiTranspWhite2),
              axis.text.x = element_text(angle=90),
              axis.title = element_blank())
    }else{
      p   <- ggplot()+
        geom_line(data=df, aes(x, y, group=phi, color=as.factor(phi)), show.legend = F, ...)+
        scale_color_brewer(palette="Pastel1")+
        pointGeom+
        geom  +
        geom1 +
        geom2 +
        geom3 +
        catchesGeom +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y", date_minor_breaks = "3 months")+
        theme_classic(base_size = 5)+
        theme(panel.background = element_rect(colour="dimGray",  fill = semiTranspWhite1),
              plot.background = element_rect(colour = "dimGray", fill = semiTranspWhite2),
              axis.text.x = element_text(angle=90),
              axis.title = element_blank())
    }
    
    
    p_grob <- ggplotGrob(p)
    p_anno <- annotation_custom(grob = p_grob, 
                                xmin = Lon-graph_size, xmax = Lon+graph_size, 
                                ymin = Lat-graph_size, ymax = Lat+graph_size)
    grobs <- grobs + p_anno
  }
  
  plot(grobs)
  invisible(return(grobs))
}