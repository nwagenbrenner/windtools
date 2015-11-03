#=======================================================
#     plot a time series for a single sensor
#=======================================================
#' @title Plot wind speed/direction as a time series 
#' @description
#' \code{plotSensor} returns speed and/or direction time series as a ggplot2 object 
#' @param df dataframe
#' @param sensor name of sensor ('plot')
#' @param var variable to plot ('speed', 'direction', or 'both')
#' @param dir_symbol symbol to use for direction if var=='both'.  
#' @param threshold threshold speed to indicate with horizontal bar; only shown when speed is plotted
#' @return ggplot2 object (unless var=='both' and dir_sybmol != 'vector')
#' @export
#' @details
#' This fucntion creates a ggplot2 object of wind speed and/or direction
#' vs. time for a single sensor. If var=='both' and dir_symbol != 'vector', 
#' direction will be plotted as points. This object includes two y-axes and is
#' created using gtable and cannot be captured by the function; instead it 
#' is drawn when the function runs. All other plots are returned as ggplot2 objects.
#'
#' @examples
#' data(wind)
#' plotSensor(wind, 'R26', var='both')

plotSensor <- function(df, sensor, var="speed", dir_symbol="vector", threshold=NULL){
    stopifnot(require("ggplot2"))
    df<-subset(df, subset=(plot == sensor))
    df[,"datetime"] <- as.character(df[,"datetime"])
    df[,"datetime"] <- as.POSIXct(strptime(df[,"datetime"], '%Y-%m-%d %H:%M:%S'))

    if(var=="speed"){    
        p<-ggplot(df, aes(x=datetime, y=obs_speed)) +
            geom_point(shape=19, size=1.5, alpha = 1) +
            geom_line() +
            xlab("Time") + ylab("Speed (m/s)") +
            theme_bw() +
            ggtitle(sensor)
        if(is.null(threshold) == FALSE){ 
            p <- p+ geom_hline(yintercept = threshold)
        }
        p <- p + theme(axis.text = element_text(size = 14))
        p <- p + theme(axis.title = element_text(size = 14))
    }
    else if(var=="direction"){
        p<-ggplot(df, aes(x=datetime, y=obs_dir)) +
            geom_point(shape=19, size=1.5, alpha = 1) +
            xlab("Time") + ylab("Direction") +
            theme_bw() +
            ggtitle(sensor)
        p <- p + theme(axis.text = element_text(size = 14))
        p <- p + theme(axis.title = element_text(size = 14))
    }
    else if(var=='both'){
        if(dir_symbol == 'vector'){
            stopifnot(require("grid"))
            u_scaled<-mapply(speed2u, 0.5, df$obs_dir) #just arrows, not scaled with speed
            v_scaled<-mapply(speed2v, 0.5, df$obs_dir) #just arrows, not scaled with speed

            df <- cbind(df, u_scaled, v_scaled)

            p<-ggplot(df, aes(x=datetime, y=obs_speed)) +
                geom_point(shape=19, size=1.5, alpha = 1) +
                geom_line() +
                xlab("Time") + ylab("Speed (m/s)") +
                theme_bw() +
                ggtitle(sensor) + 
                theme(axis.text = element_text(size = 14)) +
                theme(axis.title = element_text(size = 14)) +
                geom_segment(data=df, aes(x=datetime+u_scaled*60*60, y=max(obs_speed)+1+v_scaled/4,
                xend=datetime-u_scaled*60*60, yend=max(obs_speed)+1-v_scaled/4), 
                    arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7)
        }
        else{
            #plot speed and direction as points with different y-axes
            #the graph can't be captured, so just return after it plots
            p1<-ggplot(df, aes(x=datetime, y=obs_speed)) +
            geom_point(shape=19, size=1.5, alpha = 1, colour='darkblue') +
            geom_line(colour='darkblue') +
            xlab("Datetime") + ylab("Speed (m/s)") +
            theme_bw() +
            ggtitle(sensor) +
            theme(axis.text.y = element_text(colour = 'darkblue', size=rel(1.5))) + 
            theme(axis.title.y = element_text(colour = 'darkblue', size=rel(1.5)))
   
            p2<-ggplot(df, aes(x=datetime, y=obs_dir)) +
            geom_point(shape=19, size=1.5, alpha = 1, colour='red') +
            geom_line(colour='red') +
            ylab("Direction") +
            theme(panel.background = element_rect(fill = NA)) + 
            theme(axis.text.y = element_text(colour = 'red', size=rel(1.5))) + 
            theme(axis.title.y = element_text(colour = 'red', size=rel(1.5)))

            grid.newpage()

            # extract gtable
            g1 <- ggplot_gtable(ggplot_build(p1))
            g2 <- ggplot_gtable(ggplot_build(p2))

            # overlap the panel of 2nd plot on that of 1st plot
            pp <- c(subset(g1$layout, name == "panel", se = t:r))
            g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                pp$l, pp$b, pp$l)

            # axis tweaks
            ia <- which(g2$layout$name == "axis-l")
            ga <- g2$grobs[[ia]]
            ax <- ga$children[[2]]
            ax$widths <- rev(ax$widths)
            ax$grobs <- rev(ax$grobs)
            ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
            g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
            g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
            
            # draw it
            grid.draw(g) 
            return()
        }
    }
    else{
        print (paste0("Var '",var,"' not recognized. Options are 'speed', 'direciton', or 'both'"))
        return
    }
     
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
#' @param colorscale color scale to use for vectors (discrete or continuous)
#' @param axis_labels whether or not to plot axis labels on map (TRUE or FALSE)
#' @param scaling_factor controls the size of the wind vectors
#' @param hourly_averaging whether or not the data should be averaged by hour
#' @return ggmap object representation of the wind field
#' @export
#' @details
#' This fucntion returns a vector plot of the wind field overlayed on 
#' a static Google Maps image. If hourly_averaging is TRUE, the data will be
#' averaged by the hour and the plot will be faceted on the hour. If hourly_averaging
#' is set to FALSE, the data will be averaged by plot location only.
#' @examples
#' data(wind)
#' #' #hourly vector maps for cases when R2 measured speeds < 6 m/s
#' s <- subsetOnSpeed(wind, 'R2', '<', 6.0)
#' m <- makeVectorMap(s, 43.45, -113.15, 12, 'terrain')
#' #average vector map for a specific time period
#' t<-as.POSIXct(strptime("2010-08-16 11:00:00", '%Y-%m-%d %H:%M:%S'))
#' s <- subset(wind, subset=(datetime == t))
#' m <- makeVectorMap((s, 43.45, -113.15, 12, 'terrain', hourly_averaging=FALSE)

makeVectorMap <- function(df, lat, lon, zoom, maptype, colorscale='discrete',
                          axis_labels=TRUE, scaling_factor=800.0, hourly_averaging=TRUE){
    stopifnot(require("ggmap"))
    stopifnot(require("grid"))
    
    if(hourly_averaging == TRUE){
        df<-buildHourlyAverages(df)
    }
    else{
        df<-buildAverages(df)
    }

    df<-cbind(df, scaling_factor)

    myMap<-get_map(location = c(lon=lon, lat=lat), zoom=zoom, maptype=maptype)
    p <- ggmap(myMap)

    #line segements centered on sensor location
    if(colorscale=='discrete'){
        #scale u and v so that speed = 1, maintaining u:v ratio
        #plot vectors of equal length, but oriented in the correct direction
        u_scaled<-mapply(speed2u, 2, df$obs_dir)
        v_scaled<-mapply(speed2v, 2, df$obs_dir)
        speed_bracket <- binSpeeds(df$obs_speed)
        df <- cbind(df, u_scaled, v_scaled, speed_bracket)
        p <- p + geom_segment(data=df, aes(x=lon+u_scaled/scaling_factor, y=lat+v_scaled/scaling_factor,
            xend = lon-u_scaled/scaling_factor, yend = lat-v_scaled/scaling_factor, 
            colour = speed_bracket), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7) +
	    scale_colour_manual(values = c("red", "darkorange", "darkgreen", "blue"), name="Speed (m/s)")
    }
    else{
        p <- p + geom_segment(data=df, aes(x=lon+u/scaling_factor, y=lat+v/scaling_factor,
            xend = lon-u/scaling_factor, yend = lat-v/scaling_factor, 
            colour = obs_speed), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7) +
            scale_colour_gradient(limits=c(min(df$obs_speed),max(df$obs_speed)), name="Speed (m/s)", low="blue", high="red")
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
    
    if(!is.null(df$hour)){
        #don't do custom facet labeling for now... 
        #p <- p + facet_grid(. ~ hour, labeller=facetLabeller)
        p <- p + facet_wrap( ~ hour)
    }
    
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
#' @export
#' @details
#' Internal fucntion that returns a new name to use for
#' a facet label. Labels are fixed for particular wind
#' regimes. This function is called by \code{makeVectorMap}

facetLabeller <- function(var, value){
    value <- as.character(value)
    if (var=="hour") { 
        value[value==0] <- "Downslope"
        value[value==11]   <- "Upslope"
        value[value==16]   <- "Afternoon"
    }
    return(value)
}


