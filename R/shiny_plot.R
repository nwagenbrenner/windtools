#=======================================================
#     plot speed time series for a single sensor
#=======================================================
#' @title Plot wind speed as a time series 
#' @description
#' \code{shinyPlotSensorSpeed} returns speed time series as a ggplot2 object 
#' @param df dataframe containing a single sensor
#' @return ggplot2 object
#' @export
#' @details
#' This fucntion creates a ggplot2 object of wind speed
#' vs. time for a single sensor.
#'

shinyPlotSensorSpeed <- function(df){
    stopifnot(require("ggplot2"))
    df$date_time2<-as.POSIXct(strptime(df[,"date_time"], '%Y-%m-%d %H:%M:%S'))

    p<-ggplot(df, aes(x=date_time2, y=wind_speed)) + 
        geom_point(shape=19, size=1.5, color='blue') + 
        theme_bw() +
        xlab("Time") + 
        ylab("Observed Speed (m/s)") +
        ggtitle(df$plot_id)
  
     p<-p + scale_x_datetime(breaks=c(min(s$date_time2),
                       (max(s$date_time2) - min(s$date_time2))/4 + min(s$date_time2),
                       (max(s$date_time2) - min(s$date_time2))/4*2 + min(s$date_time2),
                       (max(s$date_time2) - min(s$date_time2))/4*3 + min(s$date_time2),
                       max(s$date_time2)))

     p <- p + theme(axis.text.x = element_text(angle = 45))
     p <- p + theme(axis.text.x = element_text(vjust = 0.5))

     p <- p + theme(axis.text = element_text(size = 14))
     p <- p + theme(axis.title = element_text(size = 14))
     
    return(p)
}
