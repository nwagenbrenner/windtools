#=======================================================
#     plot speed time series for a single sensor
#=======================================================
#' @title Plot wind speed as a time series 
#' @description
#' \code{plotSensorSpeed} returns speed time series as a ggplot2 object 
#' @param df dataframe
#' @param sensor name of sensor ('plot')
#' @param threshold threshold speed to indicate with horizontal bar
#' @return ggplot2 object
#' @export
#' @details
#' This fucntion creates a ggplot2 object of wind speed
#' vs. time for a single sensor.
#'
#' @examples
#' data(wind)
#' plotSensorSpeed(wind, 'R26')

plotSensorSpeed <- function(df, sensor, threshold=NULL){
    stopifnot(require("ggplot2"))
    df<-subset(df, subset=(plot == sensor))
    df[,"datetime"] <- as.character(df[,"datetime"])
    df[,"datetime"] <- as.POSIXct(strptime(df[,"datetime"], '%Y-%m-%d %H:%M:%S'))

    p<-ggplot(df, aes(x=datetime, y=obs_speed)) +
        geom_point(shape=19, size=1.5, alpha = 1) +
        geom_line() +
        #geom_smooth(method=loess) +
        xlab("Time") + ylab("Observed Speed (m/s)") +
        #scale_y_continuous(breaks=c(3,6,9,12,15,18)) + 
        #scale_colour_brewer(palette='Set1') +
        theme_bw() +
        ggtitle(sensor)
    if(is.null(threshold) == FALSE){ 
        p <- p+ geom_hline(yintercept = threshold)
    }
    p <- p + theme(axis.text = element_text(size = 14))
    p <- p + theme(axis.title = element_text(size = 14))
     
    return(p)
}
                          
#=======================================================
#    vector field
#=======================================================
#' @title Make a vector map of the wind field
#' @description
#' \code{makeVectorMap} returns a ggmap object of the vector field 
#' @param df dataframe
#' @param lat center lat of Google Maps image
#' @param lon center lon of Google Maps image
#' @param zoom zoom for Google Maps image (1-20)
#' @param maptype type of Google Maps image (terrain, hybrid, satellite, roadmap)
#' @return ggmap object representation of the wind field
#' @export
#' @details
#' This fucntion returns a vector plot of the wind field overlayed on 
#' a static Google Maps image. If multiple hours are supplied, the plot
#' is faceted on the hour. Note that if more than 4-6 hours are requested
#' this can take some time.
#' @examples
#' data(wind)
#' s <- subsetOnSpeed(wind, 'R2', '<', 6.0)
#' s.avg <- buildHourlyAverages(s)
#' h <- c(0, 6, 12, 18)
#' s.hr <- subsetOnHour(s.avg, h)
#' m <- makeVectorMap(s.hr, 43.45, -113.15, 12, 'terrain')

makeVectorMap <- function(df, lat, lon, zoom, maptype){
    stopifnot(require("ggmap"))
    stopifnot(require("grid"))
    myMap<-get_map(location = c(lon=lon, lat=lat), zoom=zoom, maptype=maptype)
    #note that xend,yend directions are reversed bc of weird issue with arrow (only plots correctly with ends=first)
    #line segements centered on sensor location
    p <- ggmap(myMap)
    p <- p + geom_segment(data=df, aes(x=lon+u/1000, y=lat+v/1000,
            xend = lon-u/1000, yend = lat-v/1000, 
            colour = obs_speed), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7) + 
        scale_colour_continuous(name="Speed (m/s)", low="blue", high="red")
    p <- p + theme(legend.title=element_text(size=12))
    p <- p + theme(legend.text=element_text(size = 12))
    p <- p + theme(strip.text.x=element_text(size = 12))
    p <- p + xlab("") + ylab("")
    #p <- p + theme(axis.text.x = theme_blank())
    #p <- p + theme(axis.ticks.x = theme_blank())
    
    p <- p + facet_grid(. ~ hour, labeller=facetLabeller)
    
    return(p)
}

#=======================================================
#    Relable plot facets
#=======================================================
#' @title Relable facets for wind regimes
#' @description
#' \code{facetLabeller} renames facet lables
#' @param var varible to rename
#' @param value new name to use in label
#' @return value to use in new label
#' @details
#' Internal fucntion that returns a new name to use for
#' a facet label. Labels are fixed for particular wind
#' regimes. This function is called by \code{makeVectorMap}

facetLabeller <- function(var, value){
    value <- as.character(value)
    if (var=="hour") { 
        value[value==0] <- "Downslope"
        value[value==11]   <- "Upslope"
        value[value==16]   <- "Convective Mixing"
    }
    return(value)
}