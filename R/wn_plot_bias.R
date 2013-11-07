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
        #p<-p + scale_x_datetime(breaks=c(min(df$datetime)-1*60*60,
                       #min(df$datetime)+5*60*60,
                       #min(df$datetime)+11*60*60,
                       #min(df$datetime)+17*60*60))
        p<-p + scale_x_datetime(breaks=c(min(df$datetime)+12*60*60,
                       min(df$datetime)+12*3*60*60,
                       min(df$datetime)+12*5*60*60,
                       min(df$datetime)+12*7*60*60,
                       min(df$datetime)+12*9*60*60))
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
        p<-p + scale_x_datetime(breaks=c(min(df$datetime)-1*60*60,
                       min(df$datetime)+5*60*60,
                       min(df$datetime)+11*60*60,
                       min(df$datetime)+17*60*60))
        #p<-p + scale_x_datetime(breaks=c(min(df$datetime)+12*60*60,
      #                 min(df$datetime)+12*3*60*60,
      #                 min(df$datetime)+12*5*60*60,
      #                 min(df$datetime)+12*7*60*60,
      #                 min(df$datetime)+12*9*60*60))
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
#' @param color_list optional list of colors to use for legend
#' @return ggplot2 object
#' @export
#' @details
#' Returns a scatter plot of observed vs predicted speed or direction data.
wnPlotObsVsPred <- function(df, var, color_list=NULL){
    stopifnot(require("ggplot2"))
    if(var == 'speed'){
        p<-ggplot(df, aes(x=obs_speed, y=pred_speed, linetype = fcastTypeOrdered, colour=fcastNameOrdered)) +
            xlab("Observed Speed (m/s)") + ylab("Predicted Speed (m/s)")
    }
    else if(var == 'direction'){
        p<-ggplot(df, aes(x=obs_dir, y=pred_dir, linetype = fcastTypeOrdered, colour=fcastNameOrdered)) +
            xlab("Observed Direction (m/s)") + ylab("Predicted Direction (m/s)")
    }
    else{
        stop('Incorrect variable specified for var. Options are \'speed\' or \'direction\'.')
    }
    p <- p + geom_point(shape=19, size=1.5, alpha = 0.5) +
            geom_smooth(method=loess, size=0.75) +
            scale_colour_brewer(palette='Set1', name="Model") +
            theme_bw() +
            geom_abline(intercept=0, slope=1, linetype='dashed') +
            scale_y_continuous(limits = c(-0.05, 14))
            if(!is.null(color_list)){
             p <- p + scale_colour_manual(values = color_list, name="Model")
            }
            
    return(p)
}

        







