#=======================================================
#     build initial df from bias.txt file
#     (bias.txt created from writeBias.py)
#=======================================================
#' @title Read bias.txt file create from writeBias.py
#' @description
#' \code{wnReadBiasData} reads bias.txt into a dataframe
#' @param fileName full path to bias.txt
#' @param type Type of WindNinja initializaiton used; p = point, w = weather
#' @return dataframe that can be sent to wnBuildBiasDf()
#' @export
#' @details
#' Reads in a bias.txt file created by writeBias.py. The bias.txt
#' file must contain the following data in this order: 
#' 'identifier', 'lat', 'lon', 'datetime', 'obs_speed', 'pred_speed', 
#' 'bias_speed', 'obs_dir', 'pred_dir', 'bias_dir', 'wx_speed', 'wx_bias_speed',
#' 'wx_dir', 'wx_bias_dir'

wnReadBiasData <- function(fileName, type){ #type p = point
    ds<-as.data.frame(read.table(fileName, sep=",", header=TRUE, as.is=TRUE))
    if (type == "p") colnames(ds)<-c("plot", "lat", "lon", "datetime", "obs_speed", 
        "pred_speed", "bias_speed", "obs_dir", "pred_dir", "bias_dir")
    else colnames(ds)<-c("plot", "lat", "lon", "datetime", "obs_speed", 
        "pred_speed", "bias_speed", "obs_dir", "pred_dir", "bias_dir", 
        "wx_speed", "wx_bias_speed", "wx_dir", "wx_bias_dir")
    return(ds)
}

#=======================================================
#     build main bias dataframe from multiple
#     bias.txt files
#=======================================================
#' @title Combine multiple bias.txt files into a single dataframe
#' @description
#' \code{wnBuildBiasDf} convert list of bias dataframes into single dataframe
#' @param dfList list of dataframes to combine returned from wnReadBiasData()
#' @param nameList list of weather forecasts used in WindNinja simulation
#' @return dataframe that can be used with other wintools functions
#' @export
#' @details
#' Combines multiple bias.txt files that have been read in with wnReadBiasData()
#' into a single dataframe that can be used with other windtools functions. Note 
#' that dfList and nameList MUST be in the same order.

wnBuildBiasDf <- function(dfList, nameList){
    d <- data.frame(rbind(rep(NA,13))) #set col names for bias data frame
    #fcastName = NAM, WN-NAM, etc
    #fcastType = wx or wn
    #wxType = NAM, HRRR, WRFNARR, or WRFUW
    names(d)<-c("plot", "lat", "lon", "datetime", "obs_speed", "pred_speed", "bias_speed",
                "obs_dir", "pred_dir", "bias_dir", "fcastName", "fcastType", "wxType")
                
    for(i in 1:length(dfList)){
        temp<-as.data.frame(cbind(dfList[[i]][1:5], dfList[[i]][11:12], dfList[[i]][8], 
                dfList[[i]][13:14], as.factor(nameList[[i]]), as.factor("wx"), as.factor(nameList[[i]])))
        colnames(temp)<-c("plot", "lat", "lon", "datetime", "obs_speed", "pred_speed",
                          "bias_speed", "obs_dir", "pred_dir", "bias_dir", "fcastName", "fcastType", "wxType")
        
        forecastName <- paste("WN-", nameList[[i]], sep="")
        temp2<-as.data.frame(cbind(dfList[[i]][1:10], as.factor(forecastName), as.factor("wn"), as.factor(nameList[[i]])))
        colnames(temp2)<-c("plot", "lat", "lon", "datetime", "obs_speed", "pred_speed",
                          "bias_speed", "obs_dir", "pred_dir", "bias_dir", "fcastName", "fcastType", "wxType")

        d<-rbind(d, temp, temp2)
    }
    
    d<-na.omit(d)
    d[,"fcastName"] <- as.factor(d[,"fcastName"])
    d[,"fcastType"] <- as.factor(d[,"fcastType"]) 
    d[,"wxType"] <- as.factor(d[,"wxType"])
    
    levels(d$fcastName)[levels(d$fcastName)=="WN-NAM"] <- "WindNinja-NAM"
    levels(d$fcastName)[levels(d$fcastName)=="NAM"] <- "NAM (12 km)"
    levels(d$fcastName)[levels(d$fcastName)=="HRRR"] <- "HRRR (3 km)"
    levels(d$fcastName)[levels(d$fcastName)=="WN-HRRR"] <- "WindNinja-HRRR"
    levels(d$fcastName)[levels(d$fcastName)=="WRFUW"] <- "WRF-UW (4 km)"
    levels(d$fcastName)[levels(d$fcastName)=="WN-WRFUW"] <- "WindNinja-WRF-UW"
    levels(d$fcastName)[levels(d$fcastName)=="WRFNARR"] <- "WRF-NARR (1.33 km)"
    levels(d$fcastName)[levels(d$fcastName)=="WN-WRFNARR"] <- "WindNinja-WRF-NARR"

    levels(d$wxType)[levels(d$wxType)=="HRRR"] <- "HRRR (3 km)"
    levels(d$wxType)[levels(d$wxType)=="NAM"] <- "NAM (12 km)"
    levels(d$wxType)[levels(d$wxType)=="WRFNARR"] <- "WRF-NARR (1.33 km)"
    levels(d$wxType)[levels(d$wxType)=="WRFUW"] <- "WRF-UW (4 km)"
    
    levels(d$fcastType)[levels(d$fcastType)=="wn"] <- "WindNinja"
    levels(d$fcastType)[levels(d$fcastType)=="wx"] <- "Weather Model"
    
    #reorder factors for facet plots
    d$fcastNameOrdered <- factor(d$fcastName, levels=c("NAM (12 km)", "WindNinja-NAM", "HRRR (3 km)",
                        "WindNinja-HRRR", "WRF-UW (4 km)", "WindNinja-WRF-UW", 
                        "WRF-NARR (1.33 km)", "WindNinja-WRF-NARR"))

    d$fcastTypeOrdered <- factor(d$fcastType, levels=c("WindNinja", "Weather Model"))

    d$wxTypeOrdered <- factor(d$wxType, levels=c("WRF-NARR (1.33 km)", "HRRR (3 km)",
                                                  "WRF-UW (4 km)", "NAM (12 km)"))
    
    return(d)
}

