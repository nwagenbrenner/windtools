#=======================================================
#    Fetch all data from a wind database
#=======================================================
#' @title Fetch all data from a database for specified time period
#' @description
#' \code{dbFetchAll} returns a dataframe of wind data for specified time period
#' @param dbDrv database driver ("SQLite" is only option right now)
#' @param db database to query
#' @param start_time format is '2011-08-15 06:00:00'
#' @param end_time format is '2011-08-15 06:00:00'
#' @return dataframe with id, date/time, speed, gust, direction, and quality
#' @export
#' @details
#' This fucntion returns a dataframe of raw, 30-s wind data for a specified
#' time period. Currently only connects to SQLite db. 

dbFetchAll <- function(drv, db, start_time, end_time){
    stopifnot(require("RSQLite"))
    con <- dbConnect(drv, dbname = db)
    
    sql <- paste0("SELECT * FROM mean_flow_obs ", 
            "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
            "AND Quality='OK'", collapse="")
            
    res <- dbSendQuery(con, statement = sql)
    d <- fetch(res, n = -1) #fetch all data
    dbClearResult(res)
    dbDisconnect(con)
    
    return(d)
}

