#=======================================================
# convert speed to u,v
#=======================================================
#' @title Convert speed/direction to u-component of wind
#' @description
#' \code{speed2u} returns u-component of wind
#' @param s wind speed
#' @param d wind direction in degrees
#' @return u-component of wind
#' @export
#' @details
#' This fucntion takes a wind speed and direction and returns
#' the u-component of the wind. Direction must be supplied in
#' degrees.
#'
#' @examples
#' data(wind)
#' speed2u(wind$obs_speed[1], wind$obs_dir[1])

speed2u<-function(s,d){
    u <- -s * sin(d * pi/180)
    return (u) 
}

#' @title Convert speed/direction to v-component of wind
#' @description
#' \code{speed2v} returns v-component of wind
#' @param s wind speed
#' @param d wind direction in degrees
#' @return v-component of wind
#' @export
#' @details
#' This fucntion takes a wind speed and direction and returns
#' the v-component of the wind. Direction must be supplied in
#' degrees.
#'
#' @examples
#' data(wind)
#' speed2v(wind$obs_speed[1], wind$obs_dir[1])

speed2v<-function(s,d){
    v <- -s * cos(d * pi/180)
    return (v)
}

#=======================================================
#     build initial df from file
#=======================================================
#' @title Read wind data
#' @description
#' \code{readData} reads wind data into a dataframe
#' @param fileName full path to wind data file to be read in
#' @return dataframe for use by other windtools functions
#' @export
#' @details
#' This fucntion reads in a file containing wind data. The input
#' file must contain the following data in this order: 
#' 'identifier', 'lat', 'lon', 'datetime', 'obs_speed', obs_dir'

readData <- function(fileName){
    speed<-as.data.frame(read.table(fileName, sep=",", header=TRUE, as.is=TRUE))
    colnames(speed)<-c("plot", "lat", "lon", "datetime", "obs_speed", "obs_dir")
    speed[,"datetime"] <- as.POSIXct(strptime(speed[,"datetime"], '%Y-%b-%d %H:%M:%S'))
    return(speed)
}

#=======================================================
#     plot speed time series for a single sensor
#=======================================================
#' @title Plot wind speed as a time series 
#' @description
#' \code{plotSensorSpeed} returns speed time series as a ggplot2 object 
#' @param df dataframe
#' @param sensor name of sensor ('plot')
#' @return ggplot2 object
#' @export
#' @details
#' This fucntion creates a ggplot2 object of wind speed
#' vs. time for a single sensor.
#'
#' @examples
#' data(wind)
#' plotSensorSpeed(wind, 'R26')

plotSensorSpeed <- function(df, sensor){
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
        geom_hline(yintercept = 6) +
        theme_bw() +
        ggtitle(sensor)
     
    return(p)
}

#=======================================================
#     subset on speed criteria
#=======================================================
#' @title Subset wind data on speed criteria 
#' @description
#' \code{subsetOnSpeed} returns subsetted dataframe 
#' @param df dataframe
#' @param sensor name of sensor ('plot')
#' @param condition '<' or '>'
#' @param threshold threshold speed to subset on
#' @return subsetted dataframe
#' @export
#' @details
#' This fucntion subsets the wind data frame based on
#' speed criteria for a single sensor.
#'
#' @examples
#' data(wind)
#' s <- subsetOnSpeed(wind, 'R2', '<', 6.0)


subsetOnSpeed <- function(df, sensor, condition, threshold){
    if(condition == '>'){
        s<-subset(df, subset=(plot == sensor & obs_speed > threshold))
    }
    else if(condition == '<'){
        s<-subset(df, subset=(plot == sensor & obs_speed < threshold))
    }
    speedTest <- rep(NA, length(df$datetime)) #vector of T/F
    
    for(i in 1:length(df$datetime)){ 
        speedTest[i] <- any(s$datetime == df$datetime[i]) #True if sensor obs_speed meets threshold condition
    }

    df<-cbind(df,speedTest)
    df<-subset(df, subset=(df$speedTest == TRUE))
    df <- subset(df, select = -speedTest) #drop speedTest from df

    df[,"datetime"] <- as.factor(df[,"datetime"])
    df[,"plot"] <- as.factor(df[,"plot"])
    
    return(df)
}

#============================================================
#  Build df with hourly avg speeds
#============================================================
#' @title Build a dataframe with hourly averaged data 
#' @description
#' \code{buildHourlyAverages} returns dataframe with hourly averaged data 
#' @param df dataframe
#' @return dataframe with hourly averages
#' @export
#' @details
#' This fucntion returns a dataframe with hourly averages of wind data.
#' Data are averaged over all timesteps to produce hourly averages for 
#' each hour of the day. This is useful to see, for example, what the 
#' typcial wind field looks like at 1000 LT. It may be most usefule to
#' call this function after other subsetting operations have been done.
#' For example, you may want to first subset on speed to filter out
#' high-wind event cases to examine diurnal wind patterns.
#'
#' @examples
#' data(wind)
#' s <- subsetOnSpeed(wind, 'R2', '<', 6.0)
#' s.avg <- buildHourlyAverages(s)

buildHourlyAverages <- function(df){
    stopifnot(require("circular"))

    obs_dir_radians <- df$obs_dir * pi/180 #convert to radians
    df <- cbind(df, obs_dir_radians)

    hrSpeed<-data.frame(rbind(rep(NA,8)))
    names(hrSpeed)<-c("obs_speed", "obs_dir", "lat", "lon", "plot", "u", "v", "hour")
    
    for (i in 0:23){
        hour<-subset(df, subset=(as.POSIXlt(datetime)$hour == i))
    
        #make df with avgs for each plot
        spdAvg<-tapply(hour$obs_speed, hour$plot, mean)
        dirAvgRad<-tapply(hour$obs_dir_radians, hour$plot, mean.circular)
        latAvg<-tapply(hour$lat, hour$plot, mean)
        lonAvg<-tapply(hour$lon, hour$plot, mean)
        dirAvg<-dirAvgRad * 180/pi
        
        for (m in 1:length(dirAvg)){
            if(!is.na(dirAvg[m]) && dirAvg[m] < 0.0){
                dirAvg[m]<-dirAvg[m] + 360.0
            }
        }
        
        hourlyAvg<-as.data.frame(cbind(spdAvg, dirAvg, latAvg, lonAvg))
        plot<-rownames(hourlyAvg)
        hourlyAvg<-as.data.frame(cbind(hourlyAvg, plot))
        row.names(hourlyAvg) <- NULL
        
        # calc u, v for avg speeds and add to speed df
        u<-mapply(speed2u, hourlyAvg$spdAvg, hourlyAvg$dirAvg)
        v<-mapply(speed2v, hourlyAvg$spdAvg, hourlyAvg$dirAvg)
        hourlyAvg<-as.data.frame(cbind(hourlyAvg,u,v,i))
        colnames(hourlyAvg)<-c('obs_speed', 'obs_dir', 'lat', 'lon', 'plot', 'u', 'v', 'hour')
        hrSpeed<-rbind(hrSpeed, hourlyAvg)
    }

    hrSpeed<-na.omit(hrSpeed)
    hrSpeed[,"hour"] <- as.factor(hrSpeed[,"hour"])
    
    return(hrSpeed)
}

#======================================================
#   subset the averaged hourly ds on hours
#======================================================
#' @title Subset an averaged hourly dataframe by hour
#' @description
#' \code{subsetOnHour} returns subsetted dataframe with requested hours 
#' @param df dataframe
#' @param h vector of hours
#' @return subsetted dataframe with requested hours
#' @export
#' @details
#' This fucntion returns a subsetted dataframe with specific hours 
#' from an hourly averaged dataframe.
#' @examples
#' data(wind)
#' s <- subsetOnSpeed(wind, 'R2', '<', 6.0)
#' s.avg <- buildHourlyAverages(s)
#' h <- c(0, 6, 12, 18)
#' s.hr <- subsetOnHour(s.avg, h)

subsetOnHour <- function(df, h){
    subHrSpeed<-subset(df, subset=(hour %in% h))
    return(subHrSpeed)
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
    p <- p + facet_wrap( ~ hour)
    
    return(p)
}


