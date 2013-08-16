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
