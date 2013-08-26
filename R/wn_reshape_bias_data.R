#=======================================================
#     build ds for time series plots.
#=======================================================
#' @title Build a subdataset from main bias dataframe for time series plots
#' @description
#' \code{wnBuildTsDf} subsets main bias dataframe for time series plots
#' @param df main bias dataframe
#' @param sensorList list of sensors to include
#' @param notSensorList list of sensors to omit
#' @return dataframe for use with wn* plotting functions
#' @export
#' @details
#' Builds a subdataset from main bias dataframe that can be used
#' for time series plots. Either sensorList or notSensorList should
#' be supplied. If neither sensorList or nonSensorList 
#' is supplied, all sensors will be included.

wnBuildTsDf <- function(df, sensorList='', notSensorList=''){
    if(sensorList != ''){
        df<-subset(df, subset=(plot %in% sensorList))
    }
    else{
        df<-subset(df, subset=(!(plot %in% notSensorList)))
    }
    df[,"datetime"] <- as.POSIXct(strptime(df[,"datetime"], '%Y-%b-%d %H:%M:%S'))

    levels(df$fcastName)[levels(df$fcastName)=="WN-NAM"] <- "WindNinja-NAM"
    levels(df$fcastName)[levels(df$fcastName)=="NAM"] <- "NAM (12 km)"
    levels(df$fcastName)[levels(df$fcastName)=="HRRR"] <- "HRRR (3 km)"
    levels(df$fcastName)[levels(df$fcastName)=="WN-HRRR"] <- "WindNinja-HRRR"
    levels(df$fcastName)[levels(df$fcastName)=="WRFUW"] <- "WRF-UW (4 km)"
    levels(df$fcastName)[levels(df$fcastName)=="WN-WRFUW"] <- "WindNinja-WRF-UW"
    levels(df$fcastName)[levels(df$fcastName)=="WRFNARR"] <- "WRF-NARR (1.33 km)"
    levels(df$fcastName)[levels(df$fcastName)=="WN-WRFNARR"] <- "WindNinja-WRF-NARR"

    levels(df$wxType)[levels(df$wxType)=="wx"] <- "WX Model"
    levels(df$wxType)[levels(df$wxType)=="wn"] <- "WindNinja"

    #reorder factors for facet plots
    df$fcastName2 <- factor(df$fcastName, levels=c("NAM (12 km)", "WindNinja-NAM", "HRRR (3 km)", 
        "WindNinja-HRRR", "WRF-UW (4 km)", "WindNinja-WRF-UW", "WRF-NARR (1.33 km)", "WindNinja-WRF-NARR"))
        
    return(df)
}
