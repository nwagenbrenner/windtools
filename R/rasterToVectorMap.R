#=======================================================
#     Convert speed, dir rasters to a vector map
#=======================================================
#' @title Convert speed, dir rasters to a vector map
#' @description
#' \code{rasterToVectorMap} Converts spd, dir datafame to vector plot in a ggmap object
#' @param speed raster of wind speeds
#' @param dir raster of wind directions (direction wind is coming from; 0-360)
#' @param lat center lat for ggmap object
#' @param lon center lon for ggmap object
#' @param zoom zoom level for ggmap object (1-14)
#' @param maptype basemap for ggmap object (terrain, satellite, hybrid)
#' @param colorscale discrete or continuous (default is discrete)
#' @param axis_labels TRUE or FALSE (default is TRUE)
#' @return ggmap object 
#' @export
#' @details
#' This function takes two rasters (speed and direction) and converts
#' them to a vector plot of wind overlaid on a Google Maps basemap. The
#' output is a ggmap object.

rasterToVectorMap <- function(spd, dir, lat, lon, zoom, maptype, colorscale='discrete',
                          axis_labels=TRUE){
    stopifnot(require("raster"))
    stopifnot(require("ggmap"))
    stopifnot(require("grid"))

    #convert to a dataframe
    r.brick<-brick(spd, dir)

    sp<-rasterToPoints(r.brick, spatial=TRUE)
    colnames(sp@data) <- c('spd', 'dir')

    crs<-CRS("+proj=longlat +datum=WGS84")
    sp<-spTransform(sp, crs)

    #convert to regular dataframe
    df<-sp@data
    df<-cbind(sp@coords, df)
    colnames(df)<-c('lon', 'lat', 'spd', 'dir')

    myMap<-get_map(location = c(lon=lon, lat=lat), zoom=zoom, maptype=maptype)

    p <- ggmap(myMap)

    if(colorscale=='discrete'){
        #scale u and v so that speed = 1, maintaining u:v ratio
        #this will allow us to plot vectors of equal length, but oriented in the correct direction
        u_scaled<-mapply(speed2u, 2, df$dir)
        v_scaled<-mapply(speed2v, 2, df$dir)
        speed_bracket <- binSpeeds(df$spd)
        df <- cbind(df, u_scaled, v_scaled, speed_bracket)
        p <- p + geom_segment(data=df, aes(x=lon+u_scaled/1500.0, y=lat+v_scaled/1500.0,
            xend = lon-u_scaled/1500.0, yend = lat-v_scaled/1500.0, 
            colour = speed_bracket), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7) +
	    scale_colour_manual(values = c("red", "darkorange", "darkgreen", "blue"), name="Speed (m/s)")
    }
    else{
        p <- p + geom_segment(data=df, aes(x=lon+u/1500.0, y=lat+v/1500.0,
            xend = lon-u/1500.0, yend = lat-v/1500.0, 
            colour = obs_speed), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7) +
            scale_colour_gradient(limits=c(min(df$spd),max(df$spd)), name="Speed (m/s)", low="blue", high="red")
    }
    p <- p + theme(legend.title=element_text(size=14))
    p <- p + theme(legend.text=element_text(size = 14))
    p <- p + theme(strip.text.x=element_text(size = 18))
    p <- p + theme(axis.text.x = element_text(size=18))
    p <- p + theme(strip.text.y=element_text(size = 18))
    p <- p + theme(axis.text.y = element_text(size=18))
    p <- p + xlab("") + ylab("")

    if(axis_labels == TRUE){
        p <- p + theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16))
    }
    else{
        p <- p + theme(axis.text.x = element_blank())
        p <- p + theme(axis.ticks.x = element_blank())
        p <- p + theme(axis.text.y = element_blank())
        p <- p + theme(axis.ticks.y = element_blank())
    }
    
    return(p)
}
