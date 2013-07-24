#=======================================================
# convert speed to u,v
#=======================================================
speed2u<-function(s,d){
    u <- -s * sin(d * pi/180)
    return (u) 
}
speed2v<-function(s,d){
    v <- -s * cos(d * pi/180)
    return (v)
}

#=======================================================
#     build initial df from file
#=======================================================
readData <- function(fileName){
    speed<-as.data.frame(read.table(fileName, sep=",", header=TRUE, as.is=TRUE))
    colnames(speed)<-c("plot", "lat", "lon", "datetime", "obs_speed", "obs_dir")
    speed[,"datetime"] <- as.POSIXct(strptime(speed[,"datetime"], '%Y-%b-%d %H:%M:%S'))
    return(speed)
}
#=======================================================
#     plot speed time series for a single sensor
#=======================================================
plotSensorSpeed <- function(sensor){
    s<-subset(speed, subset=(plot == sensor))
    s[,"datetime"] <- as.character(s[,"datetime"])
    s[,"datetime"] <- as.POSIXct(strptime(s[,"datetime"], '%Y-%m-%d %H:%M:%S'))

    p<-ggplot(s, aes(x=datetime, y=obs_speed)) +
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
#make df with just NM1 data for subsetting the larger df
subsetOnSpeed <- function(df, sensor, threshold){
    nm1<-subset(df, subset=(plot == sensor & obs_speed > threshold))
    speedTest <- rep(NA, length(df$datetime)) #vector of T/F

    for (i in 1:length(df$datetime)){ 
        speedTest[i] <- any(nm1$datetime == speed$datetime[i]) #True if R2 obs_speed < threshold
    }

    df<-cbind(df,speedTest)
    df<-subset(df, subset=(df$speedTest == TRUE))
    df[,"datetime"] <- as.factor(df[,"datetime"])
    #df<-subset(df, subset=(obs_speed >= 0.0)) #get rid of -888.0
    df[,"plot"] <- as.factor(df[,"plot"])

    obs_dir_radians <- df$obs_dir * pi/180 #convert to radians
    df <- cbind(df, obs_dir_radians)
    
    return(df)
}

#============================================================
#  Build df with hourly avg speeds
#============================================================
buildHourlyAverages <- function(df){
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
        #rbind this hour's avg data to the full ds
        hrSpeed<-rbind(hrSpeed, hourlyAvg)
    }

    hrSpeed<-na.omit(hrSpeed)
    hrSpeed[,"hour"] <- as.factor(hrSpeed[,"hour"])
    
    return(hrSpeed)
}

#======================================================
#   subset the averaged hourly ds on hours
#======================================================
subsetOnHour <- function(df, h1, h2, h3, h4){
    subHrSpeed<-subset(df, subset=(hour == h1 |
                                    hour == h2 |
                                    hour == h3 |
                                    hour == h4))
    return(subHrSpeed)
}
                          
#=======================================================
#    vector field
#=======================================================
makeVectorMap <- function(df, site){
    if(site == 'bsb'){
        myMap<-get_map(location = c(lon = -113.027724, lat = 43.402726), zoom = 12, maptype = 'terrain')
    }
    else if(site == 'src'){
        myMap<-get_map(location = c(lon = -116.228889, lat = 45.401667), zoom = 14, maptype = 'terrain')
    }

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


