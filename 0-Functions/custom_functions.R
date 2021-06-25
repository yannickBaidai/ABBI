#'#*******************************************************************************************************************
#'@title : Format and spatialize catches data from logbook
#'@author : Yannick BAIDAI
#'@date : 2020-03-16
#'@email : yannick.baidai@gmail.com>
#'#*******************************************************************************************************************
#'@description :  
#'@requiredPackages :
#'#****************************
#'***************************************************************************************
#'@revision
#' Spatialize function (2021-04-28): Retrieving points on grid overlay using spatial points, spatialGrid and function
#'                                   sp::over, due to unexplained behaviour of the function concerning points inter-
#'                                   secting with grid lines. 
#'                                   New method used is based on a basic omputations of cel centre and zone assignment
#'                                   (see L201)
#'#*******************************************************************************************************************



countBuoys <-function(data)
{
  brand <- substr(unique(data), start = 1, stop = 3)
  count <- as.data.frame(t(as.matrix(addmargins(table(brand), margin = 1))))
  return(count)
}



load_zone <- function(map_df="D:/THESE_scripts/0-Resources/Maps/LonghurstArea")
{
  if(dir.exists(map_df))
  {
    layername <- list.files(path = map_df, pattern = ".shp$", all.files = T)
    layername <- substr(layername, start = 0, stop = nchar(layername)-4)
    require(rgdal)
    zoning <- readOGR(dsn =map_df,
                      layer = layername,
                      verbose = F)
    
    # Transform into dataframe
    zoning@data$id <- rownames(zoning@data)
    zoning.df      <- plyr::join(x = ggplot2::fortify(zoning, region ="id"), 
                                 y = zoning@data, by ="id")
    
    group_ptr <- which(colnames(zoning.df) %in% "group")
    if(length(group_ptr) >= 2){
      zoning.df <- zoning.df[, -group_ptr[2:length(group_ptr)]]
    }
    
    return(zoning.df)
  }
  return(NULL)
}

get_area <- function(map_df)
{
  if(dir.exists(map_df))
  {
    layername <- list.files(path = map_df, pattern = ".shp$", all.files = T)
    layername <- substr(layername, start = 0, stop = nchar(layername)-4)
    require(rgdal)
    zoning <- readOGR(dsn =map_df,
                      layer = layername,
                      verbose = F)
    zoning <- sp::spTransform(x = zoning, CRSobj = CRS("+proj=longlat +datum=WGS84"))
    
    
    require(geosphere)
    area <- data.frame(area = areaPolygon(zoning)*1e-6)
    area$ID <- as.character(as.numeric(row.names(area))-1)
    zoning@data$ID <- row.names(zoning@data)
    area <- merge.data.frame(x=area, y=zoning@data, by="ID")
    
    # Transform into dataframe
    return(area)
  }
  return(NULL)
}


#' Title
#'
#' @param map_df 
#' @param lon 
#' @param lat 
load_and_join_zones <- function(map_df="D:/THESE_scripts/0-Resources/Maps/LonghurstArea",
                                lon, lat, field=NULL)
{
  zones <- NULL
  # Case data is a folder (shapefile)
  if(dir.exists(map_df))
  {
    layername <- list.files(path = map_df, pattern = ".shp$", all.files = T)
    layername <- substr(layername, start = 0, stop = nchar(layername)-4)
    require(rgdal)
    zoning <- readOGR(dsn =map_df,
                      layer = layername,
                      verbose = F)
    
    # Transform into dataframe
    zoning@data$id <- rownames(zoning@data)
    zoning.df      <- plyr::join(x = ggplot2::fortify(zoning, region ="id"), 
                                 y = zoning@data, by ="id")
    # Get zones centre
    zone_centralPos <-  aggregate(cbind(long, lat) ~ id, 
                              data= zoning.df, 
                              FUN=function(x)mean(x))
    colnames(zone_centralPos) <- c("id", "x", "y")
    
    # Select zone name from a specified columns
    if(!is.null(field)){
      zone_centralPos$zone <- zoning.df[match(zone_centralPos$id, zoning.df$id) ,field]
    }
    
    # Join data
    data  <- sp::SpatialPoints(cbind(lon, lat), proj4string =zoning@proj4string)
    zones <- sp::over(x = data, y = zoning)
    zones <- cbind.data.frame(rows = 1:nrow(zones),
                              longitude = lon,
                              latitude = lat,
                              zones)
    
    # Get cell centre
    zones <- merge.data.frame(x=zones, y=zone_centralPos, by = "id", all.x=T, sort=F)
    
    # reorder rows
    zones <- zones[order(zones$rows), ]
    require(dplyr)
    zones <- zones%>%
      dplyr::select(-rows)
  }
  return(zones)
}


#' Title
#'
#' @param longitude 
#' @param latitude 
#' @param resolution 
#' @param extent 
#' @param crs 
#'
#' @examples
spatialize <- function(longitude, latitude, resolution, extent=c(b=-90, l=-180, t=90, r=180),
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", field=NULL)
{
  if(is.character(resolution))
  {
    if(dir.exists(resolution))
    {
      df_ <- load_and_join_zones(map_df = resolution, lon = longitude, lat = latitude, field = field)
      if(!"zone" %in% colnames(df_)){
        require(dplyr) 
        df_ <- df_%>%
          dplyr::rename(zone=id)
      }
    }else{
      stop("Invalid shape file folder:", resolution)
    }
  }else
  {
    # 1. Cells centre for lat and lon
    x <- seq(extent["l"] + (resolution/2),
             extent["r"] - (resolution/2),
             resolution)
    
    y <- seq(extent["b"] + (resolution/2),
             extent["t"] - (resolution/2),
             resolution)
    
    #2. Creating zone for each lon/lat combinations
    gd <- unique(expand.grid(x=x, y=y))
    gd$zone <- 1:nrow(gd)
    
    #3 Applying formula for determining cell centre 
    df_ <- data.frame(order=1:length(longitude),
                      lon = longitude, lat =latitude,
                      x   = resolution*floor(longitude/resolution) + (resolution/2),
                      y   = resolution*floor(latitude/resolution)  + (resolution/2))
    #4. Get zone
    df_  <- merge.data.frame(x=df_, y=gd, all.x = T)
    df_  <- df_[order(df_$order), c("zone", "x", "y", "lon", "lat")]
    
    # #1.Create Grid with the specified topology
    #   require(sp)
    #   origin <- c(extent["l"] + (resolution/2),
    #               extent["b"] + (resolution/2))
    #   cellsCount <- c((extent["r"] - extent["l"])/resolution,
    #                   (extent["t"] - extent["b"])/resolution)
    #   topology <- GridTopology(cellcentre.offset = origin, cellsize = rep(resolution,2), cells.dim =cellsCount)
    #   grid     <- SpatialGridDataFrame(topology, data = data.frame(No = 1:prod(cellsCount)), proj4string = CRS(crs))
    #   cell_centre <- coordinates(grid)
    #   colnames(cell_centre) <- c("x", "y")
    #   grid@data <- data.frame(zone=grid@data$No, cell_centre)
    #   
    #   #2.Plot points over grid
    #   cell <- rgeos::overGeomGeom(x = SpatialPoints(coords = data.frame(longitude, latitude), proj4string = CRS(crs)),
    #                y = grid, returnList = F, minDimension=2)
    #   
    #   
    #   #3.Match cell number with their corresponding coordinates
    #   df_ <- cbind.data.frame(lon = longitude,
    #                           lat = latitude,
    #                           cell)
      
      # #3.Match cell number with their corresponding coordinates
      # # df_ <- data.frame(lon = longitude,
      # #                  lat = latitude,
      # #                  zone = cell$No)
      # 
      # # #4. Compute cell centre
      # # lon_range <- seq(extent["l"], extent["r"], resolution)
      # # lat_range <- seq(extent["b"], extent["t"], resolution)
      # # x <- lon_range[findInterval(x = df_$lon, vec = lon_range, all.inside =  T)] + (resolution/2)
      # # y <- lat_range[findInterval(x = df_$lat, vec = lat_range, all.inside =  T)] + (resolution/2)
      # # df_ <- cbind.data.frame(x=x, y=y, df_)
  }
  return(df_)
}

#' Title
#'
#' @param timestamp 
#' @param resolution 

#' @examples
#resolution <- c("January", "March", "June", "October")
timescaling <- function(timestamp, resolution)
{
  resolution <- toupper(resolution)
  if(length(resolution)>1)
  { # if list: aggregate time by custom interval
    months <- toupper(c("January","February","March","April","May","June",
                        "July","August","September","October","November", "December"))
    ##### Control
    if(!all(resolution %in% months)){
      stop("Function **timescaling(timestamp, resolution)**: unknown month from argument <resolution> !")
    }
    # Get time bins and period labels
    timebins <- which(months %in% resolution)
    months   <- data.frame(month = months,
                           num =1:12,
                           period = findInterval(x  = 1:12,
                                                 vec = timebins,
                                                 rightmost.closed = F))
    months <- months%>%
      dplyr::group_by(period)%>%
      dplyr::mutate(label = paste0(lubridate::month(range(num),label =T,
                                                    abbr=T, locale ="English_United Kingdom.1252"),
                                   collapse="-"))
    months <- months[order(months$num),]
    months$label <- factor(months$label, levels = unique(months$label), ordered = T)
    
    # Get labels and timescale
    timescale <- months$label[match(x = lubridate::month(timestamp), table = months$num)]

  }else{
    if(resolution == "YEAR"){
      timescale <- lubridate::year(timestamp)
    }else if(resolution == "QUARTER"){
      timescale <- lubridate::quarter(timestamp)
    }else if(resolution == "MONTH") {
      timescale <- lubridate::month(timestamp, 
                                    label =T,
                                    abbr=F, 
                                    locale ="English_United Kingdom.1252")
    }else if(resolution =="WEEK"){
      timescale <- lubridate::floor_date(as.Date(timestamp), unit="week", week_start = getOption("lubridate.week.start", 1))
    }
    else if(resolution =="DAY"){
      timescale <- as.Date(timestamp)
    }
  }

  return(timescale)
}




#' Title
#'
#' @param inputdir : <character> directory with logbook data csv files
#' @param outputfile : <character> outputs filename for spatialized catches data
#' @param spatialRes : <integer> resolution of cells
#' @param timeRes : <integer> time resolution
#' @examples
format_and_spatialize <- function(inputdir, outputfile, spatialRes = 5, timeRes = "Month")
{
  # 0.loading data------------------------------------------------------------------------------------------------
  logBookData_files <- list.files(path =  inputdir, pattern = ".csv", full.names = T, recursive = F)
  cat("+ Loading LogBook data files:\n")
  logBookData <- do.call(rbind, lapply(logBookData_files, function(x)
  {
    cat("\"",x, "\"\n", sep="")
    df <- read.csv2(x, stringsAsFactors=FALSE, dec = ".", na.strings = "", encoding = "latin1")
    if(!all(c("ocean", "catches", "fishing_mode", "activity_date") %in% colnames(df))){
      df <- NULL
    }
    df
  }))
  
  # 1. Formatting data-------------------------------------------------------------------------------------------
  # 1.1.Discarding unused fields
  require(plyr)
  require(dplyr)
  logBookData <- logBookData%>%
    dplyr::select(-gear, -landing_date, -flag)
  logBookData <- unique(logBookData)
  
  # 1.2.Translating fields ocean in english for uniformization purposes
  logBookData$ocean <- ifelse(logBookData$ocean =="Indien", "Indian", "Atlantic")
  
  # 1.3. Date fields into Date
  logBookData$activity_date <- as.Date(logBookData$activity_date, format = "%d/%m/%Y", tz ="UTC")
  
  # 1.4. keep only positive catches data
  logBookData <- logBookData%>%
    dplyr::filter(type_operation=="coup(s) positif(s)")
  
  # 1.5. Time scaling
  logBookData$timescale <- timescaling(timestamp = logBookData$activity_date, resolution=timeRes)
  
  # 1.6. Spatialize catches data
  spat <- spatialize(longitude = logBookData$longitude, latitude = logBookData$latitude, resolution = spatialRes, field = "region")
  logBookData <- cbind.data.frame(logBookData, spat[, c("x", "y", "zone")])
  
  # end
  write.csv2(logBookData, file =  outputfile, row.names = F)
  return(logBookData)
}


#' @param ggdf 
#' @param period 
#' @param mode 
#' @param species 
#' @param timescale 
#'
#' @examples
quick_view <- function(df, zoneFilePath = NULL, period=NULL, mode=NULL, specie=NULL, timescale=T, pal="Spectral")
{
  ggdf <- subset(df,  year  %in% period)# & 
                   #fishing_mode  %in% mode &
                   #species %in% specie)
  period_label  <- period
  species_label <- specie
  
  # Subsetting data
  if(is.null(period)){
    period <- unique(ggdf$year)
    period_label <- paste("from ", min(period), "to", max(period))
  }
  if(is.null(mode)){
    mode <- unique(ggdf$fishing_mode)
  }
  if(is.null(specie)){
    specie <- unique(ggdf$species)
    species_label <- "from all species"
  }

  
  # Zones
  zoning_layer.poly <- NULL
  zoning_layer_names <- NULL
  
  if(!is.null(zoneFilePath) && dir.exists(zoneFilePath))
  {
      layername <- list.files(path = zoneFilePath, pattern = ".shp$", all.files = T)
      layername <- substr(layername, start = 0, stop = nchar(layername)-4)
      require(rgdal)
      zoning <- readOGR(dsn =zoneFilePath,
                        layer = layername,
                        verbose = F)

      # Transform into dataframe
      zoning@data$id <- rownames(zoning@data)
      zoning.df      <- plyr::join(x = ggplot2::fortify(zoning, region ="id"), 
                                   y = zoning@data, by ="id")
      # fortify add a supplementary column group depending on the spaefile provided 
      # it may be possible that the dataframe already contain a colum "group". correct this here
      group_ptr <- which(colnames(zoning.df) %in% "group")
      if(length(group_ptr) >= 2){
        zoning.df <- zoning.df[, -group_ptr[2:length(group_ptr)]]
      }
      
      # Names
      cnames <-  aggregate(cbind(long, lat) ~ id, 
                           data= zoning.df,
                           FUN=function(x)mean(x))
      zoning_layer_names <- geom_text(data = cnames, 
                                      mapping = aes(x=long, y=lat, label = id),
                                      color ="black")
      
      # Building polygons
      zoning_layer.poly <- geom_polygon(data = zoning.df, show.legend = F,
                                        mapping = aes(long, lat, group = group),#color =ProvCode
                                        color ="black",
                                        fill = "transparent", 
                                        linetype ="dashed", 
                                        size = 0.6,
                                        alpha = 0)
  }

  # Map
  require(ggplot2)  
  world <- map_data("world")
  p <- ggplot()+
    #geom_raster(data=ggdf, aes(x=x, y=y, fill=mean))+
    zoning_layer.poly +
    geom_map(data=world, map=world,aes(map_id=region),
             color="black", fill="gray", size=0.05, alpha=1)+
    zoning_layer_names +
    #scale_fill_distiller(palette=pal)+
    # labs(x="Longitude", y="Latitude", subtitle = period_label, 
    #      fill= paste("Average catches\n", 
    #                  species_label,"(t)"),
    #      title=paste0("Catch distribution on ", paste0(mode, collapse=" / ")))+
    # scale_x_continuous(breaks = seq(-180, 180, 10), labels = seq(-180, 180, 10))+
    # scale_y_continuous(breaks = seq(-90, 90, 10), labels = seq(-90, 90, 10))+
    # coord_quickmap(xlim=range(ggdf$x)+c(-10,10), ylim=range(ggdf$y)+c(-10,10))+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "grey"))
  if(timescale==T){
    p <- p + facet_wrap(~timescale)
  }
  dev.new();plot(p)
}



#' Title: get_cart
#' @param series : <integer vector> time series of fad number to which the fish is associated
#' @param consider_interFad_movement : <logical> one item cannot travel directly 
#' from one FOB to the other but must pass through the reservoir.
#'
#' @return : dataframe with field: chrono (chronological succession of CAT/CRT)
#' state (CAT or CRT), duration (time), FAD_number (id of fad to whihc the fish was associated)
get_CART <- function(series)
{
  series <- cbind.data.frame(time = seq.int(length(series)),
                             FAD_number = replace(x = series, list = is.na(series), values = 0))
  series$comp <-  c(0,diff(series$FAD_number)) 
  series$comp <-  ifelse(series$comp==0, 0, 1)
  series$chrono <- cumsum(series$comp)
  
  CART <- as.data.frame(table(series$chrono))
  names(CART) <- c("chrono", "duration")
  CART$FAD_number  <- series$FAD_number[match(x = CART$chrono, table = series$chrono)]
  CART$state <- ifelse(CART$FAD_number==0, "CAT", "CRT")
  return(CART)
}

get_aCART <- function(series)
{ 
  if(length(series)>0){
    series <- data.frame(tunaPresence = series)
    series$comp <-  c(0,diff(series$tunaPresence)) 
    series$comp <-  ifelse(series$comp==0, 0, 1)
    series$chrono <- cumsum(series$comp)
    
    aCART <- as.data.frame(table(series$chrono))
    names(aCART) <- c("chrono", "duration")
    aCART$chrono <- as.numeric(as.character.factor(aCART$chrono))
    aCART$tunaPresence  <- series$tunaPresence[match(x = aCART$chrono, table = series$chrono)]
    aCART$state <- ifelse(aCART$tunaPresence==0, "aCAT", "aCRT")
  }
  # Return value according to metrics value
  if(nrow(aCART)==0){
    res <- NULL
  }else{
    res <- aCART
  }
  return(res)
}


track_maps <- function(buoy_id, trajId, lon, lat, groups=NULL)
{
  tracks <- data.frame(longitude=lon, latitude=lat, buoy_id=buoy_id, trajId=trajId)
  if(!is.null(groups)){
    tracks$groups <- as.factor(groups)
  }
  # Track maps
  library(ggplot2)
  world <- map_data("world")
  p <- ggplot(data = tracks)+
    geom_path(aes(x=longitude, y=latitude, group=paste(buoy_id, trajId)),
              color = "royalblue", alpha = 0.3)+
    # World map plotting
    geom_map(data=world,
             map=world,
             mapping = aes(map_id=region),
             color="gray50", fill="lightgray", size=0.25, alpha=1)+
    #Labelling
    labs(x = "Longitude", 
         y ="Latitude")+
    # zoom
    coord_quickmap()+
    #theme
    theme_minimal(base_size = 18)+
    theme(panel.background = element_rect(fill = "white",
                                          colour = "gray50",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                          colour = "grey"))
    
  if(!is.null(groups)){
   p <- p + facet_wrap(~groups)
  }
  dev.new(); plot(p)
}



#' Title: Melting dataframe with  multiple variable columns
#'
#' @param data      : <data.frame> dataframe to be melted
#' @param id.vars   : <character> character vector of the columns names to be used a id variables (kept in the melted dataframe)
#' @param measure.vars : <list> list of character vectors to be uses as variables. Each list vector will be sued as a variable 
#'                       if named, variable column wil be naemd with concatenation of "variable" with the name
#' @param value.names : <character> character vector of new variable and values

melt_nd <- function(data, id.vars, measure.vars,  value.names = NULL)
{
    # Check columns occurence in the dataframe
    columns      <- c(id.vars, unlist(measure.vars))
    checkColumns <- columns %in% colnames(data)
    
    if(!all(checkColumns))
    {
      stop("Fields not found in the dataframe: ", paste(columns[checkColumns==T], collapse = ", "), ".")
    }else
    {
      # If measure.vars unamed: name it with value names (if provided)
      if(!is.null(names(measure.vars)))
      {
        warning("Arg. <value.names> has been updated with <measure.vars> names.")
        value.names <- names(measure.vars)
      }else{
        
        if(!is.null(value.names) && length(measure.vars)==length(value.names))
        {
          names(measure.vars) <- value.names
        }else{
          
          value.names <- paste0("V", 1:length(measure.vars))
          names(measure.vars) <- value.names
        }
      }
    }



    ############ Melting process
    df  <- as.data.frame(data[, columns])
    df.j <- NULL
    for(i in 1:length(measure.vars))
    {
      columns.i <- c(id.vars, measure.vars[[i]])
      df.i <- reshape::melt.data.frame(data = df[, columns.i], 
                                       measure.vars = measure.vars[[i]])
      df.i$variable <- as.character.factor(df.i$variable)
      df.i$id       <- 1:nrow(df.i)
      
      colnames(df.i)[which(colnames(df.i)=="value")]    <- value.names[i]
      colnames(df.i)[which(colnames(df.i)=="variable")] <- paste0("variable.", value.names[i])
                                                                  
      # Merging data with other created dataframe 
      if(i==1)
      {
        df.j <- df.i
      }else
      {
        df.j <- merge.data.frame(x=df.j, y=df.i, by=c(id.vars, "id"), all.x = T)
      }
    }
    return(df.j)
}


#' Title
#'
#' @param lon : integer vector of longitude
#' @param lat : integer vector of latitude
#' @param zones : integer vector of zone
#' @param boundingBox : list contianing bounding box parameter or NULL
#'
zone_delimitation <- function(lon, lat, zones=NULL, boundingBox =NULL)
{
  if(is.null(boundingBox)){
    res <- rep(TRUE, length(lon))
  }else{
    xMin  = boundingBox$xmin
    xMax  = boundingBox$xmax
    yMin  = boundingBox$ymin
    yMax  = boundingBox$ymax
    excludedzones = boundingBox$excludedZones
      
      
    df <- data.frame(lon, lat)
    if(!is.null(zones) && !is.null(excludedzones)){
      df$zone <- zones
      res <- !(df$zone %in% excludedzones) & (df$lon >= xMin & df$lon <= xMax) & (df$lat >= yMin & df$lat <= yMax)
    }else{
      res <- (df$lon >= xMin & df$lon <= xMax) & (df$lat >= yMin & df$lat <= yMax)
      
    }
  }
  return(res)
}



get_dailyBuoyCount <- function(buoysDataLoc, spatialScale, timeScale)
{
  # Retrieving on-board buoys
  buoysDataLoc <- subset(buoysDataLoc, is_onBoardShip == F & is_onLand==F)
  
  if(nrow(buoysDataLoc)>0)
  {
    # Spatialize and get buoy density
    spat_echo <- spatialize(longitude = buoysDataLoc$longitude,
                            latitude  = buoysDataLoc$latitude,
                            resolution = spatialScale,#, spatialRes ="D:/THESE_scripts/0-Resources/Maps/LonghurstArea")
                            field = "region")
    buoysDataLoc <- cbind.data.frame(buoysDataLoc, spat_echo[, c("x", "y", "zone")])
    rm(spat_echo)
    
    # Apply Time scaling, bounding box and exclude zone
    buoysDataLoc <- buoysDataLoc%>%
      dplyr::mutate(zone = forcats::fct_explicit_na(as.factor(zone), "OFF_ZONE"),
                    timescale = timescaling(timestamp = position_date, resolution = timeScale))
    
    # Getting buoy density
    dens <- buoysDataLoc%>%
      dplyr::mutate(year = lubridate::year(position_date))%>%
      dplyr::group_by(year, timescale, zone, x, y, date=as.Date(position_date))%>%
      dplyr::group_modify(~countBuoys(.x$buoy_id))%>%
      dplyr::rename(buoyCount = Sum)
    
    return(dens)
  }
  return(NULL)
}
