#=======================================================
#    wind speed time series plots faceted on wx type
#=======================================================
#' @title Make a faceted wind speed time series plot
#' @description
#' \code{wnPlotSpeedTs} plots wind speed time series faceted on sensor and weather model type
#' @param df dataframe returned from wnBuildTimeSeriesDf()
#' @return ggplot2 object
#' @export
#' @details
#' Plots the wind speed time series faceted on weather model type if the supplied dataframe
#' contains a single sensor. If the dataframe contains multiple sensors, the time series plot
#' is facect on sensor and weather model type.
wnPlotSpeedTs <- function(df){
    stopifnot(require("ggplot2"))
    p<-ggplot(df, aes(x=datetime, y=pred_speed, colour=fcastType)) +
        geom_line(linetype="dashed") + 
        geom_point(shape=19, size=1.5, alpha = 1.0) +
        xlab("Time") + ylab("Speed (m/s)") +
        scale_colour_brewer(name='Model', palette='Set1')

    p<-p + theme(axis.text.x = element_text(angle = 45))
    p<-p + theme(axis.text.x = element_text(vjust = 0.5))
    #p<-p + theme(axis.text.x = element_text(hjust = 0.2))
    p<-p + geom_line(data=df, aes(x=datetime, y=obs_speed), colour='black')
    p<-p + geom_point(data=df, aes(x=datetime, y=obs_speed),shape = 19, size=1.5, colour='black', alpha = 1.0)

    p<-p + scale_x_datetime(breaks=c(min(df$datetime)+12*60*60,
                       min(df$datetime)+12*3*60*60,
                       min(df$datetime)+12*5*60*60,
                       min(df$datetime)+12*7*60*60,
                       min(df$datetime)+12*9*60*60))
    if(length(unique(df$plot)) == 1){
        p<-p + ggtitle(df$plot[1])
        p<-p + facet_wrap( ~ wxType, ncol = 1)
    }
    else{
        p<-p + facet_grid(wxType ~ plot)
    }
    return(p)
}


#=======================================================
#  wind direction time series plots faceted on wx type
#=======================================================
#' @title Make a faceted wind direction time series plot
#' @description
#' \code{wnPlotDirTs} plots wind direction time series faceted on sensor and weather model type
#' @param df dataframe returned from wnBuildTsDf()
#' @return ggplot2 object
#' @export
#' @details
#' Plots the wind direction time series faceted on weather model type if the supplied dataframe
#' contains a single sensor. If the dataframe contains multiple sensors, the time series plot
#' is facect on sensor and weather model type.
wnPlotDirTs <- function(df){
    stopifnot(require("ggplot2"))
    p<-ggplot(df, aes(x=datetime, y=pred_dir, colour=fcastType)) +
        geom_line(linetype="dashed") + 
        geom_point(shape=19, size=1.5, alpha = 1.0) +
        xlab("Time") + ylab("Direction") +
        scale_colour_brewer(name='Model', palette='Set1')

    p<-p + theme(axis.text.x = element_text(angle = 45))
    p<-p + theme(axis.text.x = element_text(vjust = 0.5))
    #p<-p + theme(axis.text.x = element_text(hjust = 0.2))
    p<-p + geom_line(data=df, aes(x=datetime, y=obs_dir), colour='black')
    p<-p + geom_point(data=df, aes(x=datetime, y=obs_dir),shape = 19, size=1.5, colour='black', alpha = 1.0)

    p<-p + scale_x_datetime(breaks=c(min(df$datetime)+12*60*60,
                       min(df$datetime)+12*3*60*60,
                       min(df$datetime)+12*5*60*60,
                       min(df$datetime)+12*7*60*60,
                       min(df$datetime)+12*9*60*60))
    if(length(unique(df$plot)) == 1){
        p<-p + ggtitle(df$plot[1])
        p<-p + facet_wrap( ~ wxType, ncol = 1)
    }
    else{
        p<-p + facet_grid(wxType ~ plot)
    }
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
        p <- ggplot(df, aes(x=obs_speed, y=bias_speed, colour=fcastType)) +
            ggtitle('Observed Speed vs Bias Speed')
    }
    else if(var == 'direction'){
        p <- ggplot(df, aes(x=obs_dir, y=bias_dir, colour=fcastType)) +
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
#' @return ggplot2 object
#' @export
#' @details
#' Returns a scatter plot of observed vs predicted speed or direction data.
wnPlotObsVsPred <- function(df, var){
    stopifnot(require("ggplot2"))
    if(var == 'speed'){
        p<-ggplot(data, aes(x=obs_speed, y=pred_speed, colour=fcastName)) +
            xlab("Observed Speed (m/s)") + ylab("Predicted Speed (m/s)")
    }
    else if(var == 'direction'){
        p<-ggplot(data, aes(x=obs_dir, y=pred_dir, colour=fcastName)) +
            xlab("Observed Direction (m/s)") + ylab("Predicted Direction (m/s)")
    }
    else{
        stop('Incorrect variable specified for var. Options are \'speed\' or \'direction\'.')
    }
    p <- p + geom_point(shape=19, size=1.5, alpha = 0.5) +
            geom_smooth(method=loess) +
            scale_colour_brewer(palette='Set1') +
            theme_bw() +
            geom_abline(intercept=0, slope=1, linetype='dashed')
            
    return(p)
}

        







