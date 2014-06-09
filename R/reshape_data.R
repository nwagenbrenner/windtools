#=======================================================
#     subset on direction criteria
#=======================================================
#' @title Subset wind data on direction criteria 
#' @description
#' \code{subsetOnDirection} returns subsetted dataframe 
#' @param df dataframe
#' @param sensor name of sensor ('plot')
#' @param min min direction
#' @param max max direction
#' @return subsetted dataframe
#' @export
#' @details
#' This fucntion subsets the wind data frame based on
#' direction criteria for a single sensor.
#'
#' @examples
#' data(wind)
#' s <- subsetOnDireciton(wind, 'R2', 210, 230)

subsetOnDirection <- function(df, sensor, min, max, threshold){

    s<-subset(df, subset=(plot == sensor & obs_dir > min & obs_dir < max))

    speedTest <- rep(NA, length(df$datetime)) #vector of T/F
    
    for(i in 1:length(df$datetime)){ 
        speedTest[i] <- any(s$datetime == df$datetime[i]) #True if sensor obs_speed meets threshold condition
    }

    df<-cbind(df,speedTest)
    df<-subset(df, subset=(df$speedTest == TRUE))
    df <- subset(df, select = -speedTest) #drop speedTest from df

    #df[,"datetime"] <- as.factor(df[,"datetime"])
    df[,"plot"] <- as.factor(df[,"plot"])
    
    return(df)
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

    
    #df[,"datetime"] <- as.factor(df[,"datetime"])
    df[,"plot"] <- as.factor(df[,"plot"])
    
    return(df)
}

#=======================================================
#     subset on datetime criteria
#=======================================================
#' @title Subset wind data on datetime criteria 
#' @description
#' \code{subsetOnDate} returns subsetted dataframe 
#' @param df dataframe
#' @param condition '<', '==', or '>'
#' @param dt datetime (as a POSIXct object) to subset on
#' @return subsetted dataframe
#' @export
#' @details
#' This fucntion subsets the wind data frame based on
#' a datetime criteria. dt must be a POSIXct object.
#'
#' @examples
#' data(wind)
#' dt <- as.POSIXct(strptime("2010-08-01 00:00:00", '%Y-%m-%d %H:%M:%S'))
#' s <- subsetOnDate(wind, '<', dt)

subsetOnDate <- function(df, condition, dt){
    if(condition == '>'){
        s<-subset(df, subset=(datetime > dt))
    }
    if(condition == '=='){
        s<-subset(df, subset=(datetime == dt))
    }
    else if(condition == '<'){
        s<-subset(df, subset=(datetime < dt))
    }   
    return(s)
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
    
    #for (i in 0:23){
    start = unique(as.POSIXlt(df$datetime)$hour)[1]
    end = unique(as.POSIXlt(df$datetime)$hour)[length(unique(as.POSIXlt(df$datetime)$hour))]
    for (i in start:end){
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

#======================================================
#   reorder a factor in a dataframe
#======================================================
#' @title Reorder a factor in a dataframe
#' @description
#' \code{reorderFactor} returns dataframe with reordered factor
#' @param df dataframe
#' @param var variable (factor) to reorder; options are: 'hour'
#' @param order list of factor levels in desired order
#' @return dataframe with reordered factor levels for var
#' @export
#' @details
#' This fucntion reorders the levels of factor var. This
#' is useful, for example, for changing the facet order
#' in ggplot2 graphics.
#' @examples
#' data(wind)
#' order <- c(11, 16, 0, 1:10, 12:15, 17:23)
#' s <- reorderFactor(wind, hour, order)

reorderFactor <- function(df, var, order){
    if (var == 'hour'){
        df$hour <- factor(df$hour, levels = order)
    }
    return(df)
}

#======================================================
#   bin wind speeds for vector plotting
#======================================================
#' @title Bin wind speeds for vector plotting
#' @description
#' \code{binSpeeds} returns a vector of speed bins as factors
#' @param speedVector vector of speeds to bin
#' @return vector of speed bins as factors
#' @export
#' @details
#' This fucntion bins wind speeds into discrete bins
#' for use with \code{makeVectorMap} 
#' @examples
#' data(wind)
#' speed_bracket <- binSpeeds(wind$obs_speed)

binSpeeds <- function(speedVector){
    b <- speedVector
    range <- max(speedVector)
    b1 <- round((0.25 * range), digits = 2)
    b2 <- round((0.5 * range), digits = 2)
    b3 <- round((0.75 * range), digits = 2)
    b4 <- round((0.85 * range), digits = 2)
    for (i in 1:length(speedVector)){
	#print(b1)
        if (speedVector[i] < b1){
           b[i] <- paste("<", b1)
        }
        else if(speedVector[i] < b2){
            b[i] <- paste0(b2, "-", b3)
        }
        else if(speedVector[i] < b3){
            b[i] <- paste0(b3, "-", b4)
        }
        else (b[i] <- paste(">", b4))
    }
    b<-as.factor(b)
    order<-c(paste(">", b4), paste0(b3, "-", b4), paste0(b2, "-", b3), paste("<", b1))
    b <- factor(b, levels=order)
    return(b) 
}


