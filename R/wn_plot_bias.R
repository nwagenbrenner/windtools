#=======================================================
#    wind speed time series plots faceted on wx type
#=======================================================
#' @title Make a faceted wind speed time series plot
#' @description
#' \code{wnPlotSpeedTs} plots wind speed time series faceted on sensor and weather model type
#' @param df dataframe returned from wnBuildTimeSeriesDf()
#' @param color_list optional list of colors to use (e.g., colors <- c("WindNinja" = "red", "NWP" = "darkblue"))
#' @param xscale optional scale_x_datetime() argument
#' @return ggplot2 object
#' @export
#' @details
#' Plots the wind speed time series faceted on NWP model and model type 
wnPlotSpeedTs <- function(df, color_list=NULL, xscale=NULL){
    stopifnot(require("ggplot2"))
    p <- ggplot(df, aes(x=datetime, y=obs_speed)) +
        geom_point(shape=19, size=1.5, alpha = 1.0, colour='black', position="jitter") +
        xlab("Time") + ylab("Speed (m/s)")

    p <- p + theme(axis.text.x = element_text(angle = 45))
    p <- p + theme(axis.text.x = element_text(vjust = 0.5))
    p <- p + theme(axis.text.x = element_text(color='black', size=12))
    p <- p + theme(axis.text.y = element_text(color='black', size=12))
    
    if(is.null(xscale)){
        #try to set reasonable breaks for x-axis
        p<-p + scale_x_datetime(breaks=c(min(df$datetime),
                       (max(df$datetime) - min(df$datetime))/4 + min(df$datetime),
                       (max(df$datetime) - min(df$datetime))/4*2 + min(df$datetime),
                       (max(df$datetime) - min(df$datetime))/4*3 + min(df$datetime),
                       max(df$datetime)))
        #p<-p + scale_x_datetime(breaks=c(min(df$datetime)+12*60*60,
        #               min(df$datetime)+12*3*60*60,
        #               min(df$datetime)+12*5*60*60,
        #               min(df$datetime)+12*7*60*60,
        #               min(df$datetime)+12*9*60*60))
    }
    else{
        p <- p + xscale
    }

    p<-p + geom_point(data=df, aes(x=datetime, y=pred_speed, colour=fcastType),shape = 19, size=1.5, alpha = 0.5, position="jitter")
    #p<-p + geom_smooth(method="loess", aes(x=datetime, y=pred_speed, colour=fcastType))
    if(!is.null(color_list)){
        p<-p + scale_colour_manual(values=color_list, name="Model Type")
    }
    p<-p + facet_grid(fcastType ~ wxType)

    if(length(unique(df$plot)) == 1){
        p<-p + ggtitle(df$plot[1])
    }
    theme(strip.text = element_text(size=rel(1.5)))
    return(p)
}


#=======================================================
#  faceted wind direction time series plots
#=======================================================
#' @title Make a faceted wind direction time series plot
#' @description
#' \code{wnPlotDirTs} plots wind direction time series faceted on sensor and weather model type
#' @param df dataframe returned from wnBuildTsDf()
#' @param color_list optional list of colors to use (e.g., colors <- c("WindNinja" = "red", "NWP" = "darkblue"))
#' @param xscale optional scale_x_datetime() argument
#' @return ggplot2 object
#' @export
#' @details
#' Plots the wind direction time series faceted on NWP type and model type
wnPlotDirTs <- function(df, color_list=NULL, xscale=NULL){
    stopifnot(require("ggplot2"))
    p <- ggplot(df, aes(x=datetime, y=obs_dir)) +
        geom_point(shape=19, size=1.5, alpha = 1.0, colour='black', position="jitter") +
        xlab("Time") + ylab("Direction")

    p <- p + theme(axis.text.x = element_text(angle = 45))
    p <- p + theme(axis.text.x = element_text(vjust = 0.5))
    p <- p + theme(axis.text.x = element_text(color='black', size=12))
    p <- p + theme(axis.text.y = element_text(color='black', size=12))
    
    if(is.null(xscale)){
        #try to set reasonable breaks for x-axis
        p<-p + scale_x_datetime(breaks=c(min(df$datetime),
                       (max(df$datetime) - min(df$datetime))/4 + min(df$datetime),
                       (max(df$datetime) - min(df$datetime))/4*2 + min(df$datetime),
                       (max(df$datetime) - min(df$datetime))/4*3 + min(df$datetime),
                       max(df$datetime)))

        #p<-p + scale_x_datetime(breaks=c(min(df$datetime)-1*60*60,
        #               min(df$datetime)+5*60*60,
        #               min(df$datetime)+11*60*60,
        #               min(df$datetime)+17*60*60))

    }
    else{
        p <- p + xscale
    }

    p<-p + geom_point(data=df, aes(x=datetime, y=pred_dir, colour=fcastType),shape = 19, size=1.5, alpha = 0.5, position="jitter")
    #p<-p + geom_smooth(method="loess", aes(x=datetime, y=pred_speed, colour=fcastType))
    if(!is.null(color_list)){
        p<-p + scale_colour_manual(values=color_list, name="Model Type")
    }
    p<-p + facet_grid(fcastType ~ wxType)

    if(length(unique(df$plot)) == 1){
        p<-p + ggtitle(df$plot[1])
    }
    theme(strip.text = element_text(size=rel(1.5)))
    return(p)
}

#=======================================================
#       box plots
#=======================================================
#' @title Make a box plot
#' @description
#' \code{wnBoxplot} makes box plots for bias_speed or bias_dir in main bias dataframe
#' @param df dataframe returned from wnBuildBiasDf()
#' @param var variable to plot ('bias_speed' or 'bias_dir')
#' @param jitter indicates if points should be plotted (TRUE, FALSE)
#' @return ggplot2 object
#' @export
#' @details
#' Makes box plots for either speed or direction.

wnBoxplot <- function(df, var, jitter=FALSE){
    stopifnot(require("ggplot2"))
    if(var == 'bias_speed'){
        p<-ggplot(df, aes(x=fcastNameOrdered, y=bias_speed, colour=fcastType)) +
            geom_boxplot(outlier.shape=21, notch=TRUE) +
            ggtitle(paste(min(data$datetime), '-', max(data$datetime))) +
            #facet_wrap( ~ fcastType, ncol=1)
            theme(axis.text.x = element_text(angle = 45))
    }
    else if(var == 'bias_dir'){
        p<-ggplot(df, aes(x=fcastNameOrdered, y=bias_dir, colour=fcastType)) +
            geom_boxplot(outlier.shape=21, notch=TRUE) +
            ggtitle(paste(min(data$datetime), '-', max(data$datetime))) +
            #facet_wrap( ~ fcastType, ncol=1)
            theme(axis.text.x = element_text(angle = 45))
    }
    else{
        error <- 'Incorrect variable specified. Options are \'bias_speed\' and \'bias_dir\'.'
        return(error)
    }
    
    if(jitter==TRUE){
        p <- p + geom_jitter(alpha=0.5)
    }
    
    return(p)
    
}

#=======================================================
#       scatter plots
#=======================================================
#' @title Plot bias vs observed data
#' @description
#' \code{wnPlotBiasVsObs} plots bias vs observed data faceted on wx model type
#' @param df dataframe returned from wnBuildBiasDf()
#' @param var variable to plot ('speed' or 'direction')
#' @return ggplot2 object
#' @export
#' @details
#' Returns a scatter plot of speed or direction bias vs observed data faceted
#' on weather model type.

wnPlotBiasVsObs <- function(df, var){
    stopifnot(require("ggplot2"))
    if(var == 'speed'){
        p <- ggplot(df, aes(x=obs_speed, y=bias_speed, colour=wxTypeOrdered)) +
            ggtitle('Observed Speed vs Bias Speed')
    }
    else if(var == 'direction'){
        p <- ggplot(df, aes(x=obs_dir, y=bias_dir, colour=wxTypeOrdered)) +
            ggtitle('Observed Direction vs Bias Direction')
    }
    else{
        stop('Incorrect variable specified for var. Options are \'speed\' or \'direction\'.')
    }

        p <- p + geom_point(shape=19, size=1.5, alpha = 0.5) +
            geom_smooth(method=loess) + # uses loess fit
            scale_colour_brewer(palette='Set1') +
            facet_wrap( ~ wxType, ncol=1)
        
    return(p)
}

#' @title Plot observed vs predicted values
#' @description
#' \code{wnPlotObsVsPred} plots observed vs. predicted data
#' @param df dataframe returned from wnBuildBiasDf()
#' @param var variable to plot ('speed' or 'direction')
#' @param color_list optional list of colors to use for legend
#' @return ggplot2 object
#' @export
#' @details
#' Returns a scatter plot of observed vs predicted speed or direction data.
wnPlotObsVsPred <- function(df, var, color_list=NULL){
    stopifnot(require("ggplot2"))
    if(var == 'speed'){
        p<-ggplot(df, aes(x=obs_speed, y=pred_speed, linetype = fcastTypeOrdered, colour=wxTypeOrdered)) +
            xlab("Observed Speed (m/s)") + ylab("Predicted Speed (m/s)")
    }
    else if(var == 'direction'){
        p<-ggplot(df, aes(x=obs_dir, y=pred_dir, linetype = fcastTypeOrdered, colour=wxTypeOrdered)) +
            xlab("Observed Direction (m/s)") + ylab("Predicted Direction (m/s)")
    }
    else{
        stop('Incorrect variable specified for var. Options are \'speed\' or \'direction\'.')
    }
    
    p <- p + geom_point(size=2.5, alpha = 0.05) + #shape=19
            #geom_smooth(method=loess, size=0.75) +
            geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
            scale_colour_brewer(palette='Set2', name="Model") +
            #theme_bw() +
            geom_abline(intercept=0, slope=1, linetype='dashed') +
            scale_y_continuous(limits = c(-0.05, 14))
            if(!is.null(color_list)){
             p <- p + scale_colour_manual(values = color_list, name="Model")
            }
            
    return(p)
}

#' @title Create bubble maps of wind prediction errors 
#' @description
#' \code{wnCreateBubbleMap} creates bubble maps of wind prediction errors
#' @param df dataframe returned from wnBuildBiasDf() or wnBuildTsDf()
#' @param model weather model to subset on (a level in fcastName)
#' @param var variable to plot (speed, dir)
#' @param stat statistic to plot (bias, rmse)
#' @param breaks number of breaks to set in legend
#' @return bubbleGoogleMaps object
#' @export
#' @details
#' Returns a bubble map of wind prediction errors. WindNinja and weather
#' model errors are displayed in the same map.

wnCreateBubbleMap <- function(df, model, var="speed", stat="bias", breaks=5){
    stopifnot(require("plotGoogleMaps"))
    stopifnot(require("plyr"))
    
    if(var!="speed" & var!="dir"){
        p<-"Invalid input for var. Options are speed and dir."
        return(p)
    }

    wx<-subset(df, subset=(fcastName==model))
    if(model == "WRF-NARR (1.33 km)"){
        ninja<-subset(df, subset=(fcastName=="WindNinja-WRF-NARR"))
    }
    else if(model == "NAM (12 km)"){
        ninja<-subset(df, subset=(fcastName=="WindNinja-NAM"))
    }
    else if(model == "HRRR (3 km)"){
        ninja<-subset(df, subset=(fcastName=="WindNinja-HRRR"))
    }
    else if(model == "WRF-UW (4 km)"){
        ninja<-subset(df, subset=(fcastName=="WindNinja-WRF-UW"))
    }
    else{
        p<-"Can't determine model. Options are: WRF-NARR (1.33 km), NAM (12 km), HRRR (3 km), and WRF-UW (4 km)"
        return(p)
    }  

    #drop unused columns
    drops <- c("datetime","fcastName","fcastType","wxType",
               "fcastNameOrdered","fcastTypeOrdered","wxTypeOrdered")
    ninja<-ninja[,!(names(ninja) %in% drops)]
    wx<-wx[,!(names(wx) %in% drops)]

    #compute stats
    ninja <- ddply(ninja, .(plot), function(df)c(mean(df$lat), 
            mean(df$lon), mean(df$bias_speed), 
            rmse(df$bias_speed), mean(df$bias_dir), rmse(df$bias_dir)))
    colnames(ninja) <- c("plot", "lat", "lon", "bias_speed", "rmse_speed", "bias_dir", "rmse_dir")

    wx <- ddply(wx, .(plot), function(df)c(mean(df$lat), 
            mean(df$lon), mean(df$bias_speed), 
            rmse(df$bias_speed), mean(df$bias_dir), rmse(df$bias_dir)))
    colnames(wx) <- c("plot", "lat", "lon", "bias_speed", "rmse_speed", "bias_dir", "rmse_dir")

    #round (avoid errors in caclulating data breaks)
    ninja$bias_speed<-round(ninja$bias_speed, digits = 1)
    ninja$rmse_speed<-round(ninja$rmse_speed, digits = 1)
    ninja$bias_dir<-round(ninja$bias_dir, digits = 0)
    ninja$rmse_dir<-round(ninja$rmse_dir, digits = 0)
    wx$bias_speed<-round(wx$bias_speed, digits = 1)
    wx$rmse_speed<-round(wx$rmse_speed, digits = 1)
    wx$bias_dir<-round(wx$bias_dir, digits = 0)
    wx$rmse_dir<-round(wx$rmse_dir, digits = 0)

    #convert to SpatialPointsDataFrame
    coordinates(ninja)<-c("lon", "lat")
    ninja@proj4string<-CRS("+proj=longlat +datum=WGS84")
    coordinates(wx)<-c("lon", "lat")
    wx@proj4string<-CRS("+proj=longlat +datum=WGS84")    

    v<-paste0(stat, "_", var)
    max<-max(ninja@data[, v])
    min<-min(ninja@data[, v])
    b<-c((max-min)/4,(max-min)/2, (max-min)/1.4, max)

    m<-bubbleGoogleMaps(wx, zcol=v,
                    key.entries = quantile(ninja@data[, v], (1:breaks)/breaks),
                    #key.entries = b, #upper endpoints, do not include min!
                    add=TRUE,
                    layerName=paste(model, v),
                    max.radius=200, 
                    do.sqrt=FALSE, 
                    strokeOpacity=0)

    m2<-bubbleGoogleMaps(ninja, zcol=v,
                    key.entries = quantile(ninja@data[, v], (1:breaks)/breaks),
                    #key.entries = b, #upper endpoints, do not include min!
                    previousMap=m,
                    layerName=paste0("WN-", model, " ", v),
                    max.radius=200, 
                    do.sqrt=FALSE, 
                    strokeOpacity=0)

    return(m2)

}

#' @title Create vector maps of overlaid observed and predicted winds 
#' @description
#' \code{wnCreateBiasVectorMap} creates bubble maps of wind prediction errors
#' @param df dataframe returned from buildBiasHourlyAverages
#' @param colors color vectors by speed (TRUE/FALSE) 
#' @return plotGoogleMaps object
#' @export
#' @details
#' Returns a vector map of observed and predcited winds.

wnCreateBiasVectorMap <- function(df, colors=FALSE){
    stopifnot(require("plotGoogleMaps"))
    stopifnot(require("plyr"))

    obs_dir_radians <- df$obs_dir * pi/180 #convert to radians
    pred_dir_radians <- df$pred_dir * pi/180 #convert to radians
    df <- cbind(df, obs_dir_radians, pred_dir_radians)

    df <- ddply(df, .(plot), function(d)c(mean(d$lat), mean(d$lon), 
                mean(d$obs_speed), mean.circular(d$obs_dir_radians),
                mean(d$pred_speed), mean.circular(d$pred_dir_radians)))
    colnames(df) <- c("plot", "lat", "lon", "obs_speed", "obs_dir", "pred_speed", "pred_dir")

    df$obs_dir<-df$obs_dir * 180/pi
    df$pred_dir<-df$pred_dir * 180/pi

    for (m in 1:length(df$obs_dir)){
        if(!is.na(df$obs_dir[m]) && df$obs_dir[m] < 0.0){
            df$obs_dir[m]<-df$obs_dir[m] + 360.0
        }
        if(!is.na(df$pred_dir[m]) && df$pred_dir[m] < 0.0){
            df$pred_dir[m]<-df$pred_dir[m] + 360.0
        }
    }

    #convert to SpatialPointsDataFrame
    coordinates(df)<-c("lon", "lat")
    df@proj4string<-CRS("+proj=longlat +datum=WGS84")
        
    df$obs_dir<-df$obs_dir - 180
    df$obs_dir[df$obs_dir < 0] <- df$obs_dir[df$obs_dir < 0] + 360

    df$pred_dir<-df$pred_dir - 180
    df$pred_dir[df$pred_dir < 0] <- df$pred_dir[df$pred_dir < 0] + 360

    obs_vect=vectorsSP(df, maxlength=500, zcol=c('obs_speed','obs_dir'))
    pred_vect=vectorsSP(df, maxlength=500, zcol=c('pred_speed','pred_dir'))

    pal<-colorRampPalette(c("blue","green","yellow", "orange", "red"))
    cp<-pal(5)
    if(colors==FALSE){
        pal<-colorRampPalette(c("red"))
        cp<-pal(1)
    }

    
    m<-plotGoogleMaps(obs_vect, zcol='obs_speed', 
                           colPalette=cp,
                           mapTypeId='HYBRID',
                           strokeWeight=2,
                           layerName='Observed',
                           clickable=FALSE,add=TRUE)
   
    if(colors==FALSE){
        pal<-colorRampPalette(c("blue"))
        cp<-pal(1)
    }
    m2<-plotGoogleMaps(pred_vect, 
                           zcol='pred_speed', 
                           colPalette=cp,
                           previousMap=m,
                           strokeWeight=2,
                           layerName='Predicted',
                           strokeOpacity = 1,
                           clickable=FALSE)

    return(m2)

}

        







