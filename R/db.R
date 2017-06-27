#=======================================================
#    Fetch all data from an sqlite database
#=======================================================
#' @title Fetch all data from an SQLite database for a specified time period
#' @description
#' \code{dbFetchAll} returns a dataframe from an SQLite database for a specified time period
#' @param db wind database to query
#' @param start_time format is '2011-08-15 06:00:00'
#' @param end_time format is '2011-08-15 06:00:00'
#' @return dataframe with id, date/time, speed, gust, direction, and quality
#' @export
#' @details
#' This fucntion returns a dataframe of raw, 30-s wind data for a specified
#' time period.

dbFetchAll <- function(db, start_time, end_time){
    stopifnot(require("RSQLite"))
    con <- dbConnect(SQLite(), dbname = db)
    
    sql <- paste0("SELECT * FROM mean_flow_obs ", 
            "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
            "AND Quality='OK'", collapse="")
            
    res <- dbSendQuery(con, statement = sql)
    d <- fetch(res, n = -1) #fetch all data
    dbClearResult(res)
    dbDisconnect(con)
    
    return(d)
}


#=======================================================
#    Fetch all data from a single sensor
#=======================================================
#' @title Fetch all data from a single sensor for a specified time period
#' @description
#' \code{dbFetchSensor} returns a dataframe from an SQLite database for a single
#' senosr for a specified time period
#' @param db wind database to query
#' @param sensor sensor to extract info for
#' @param start_time format is '2011-08-15 06:00:00'
#' @param end_time format is '2011-08-15 06:00:00'
#' @return dataframe with id, date/time, speed, gust, direction, and quality
#' @export
#' @details
#' This fucntion returns a dataframe of raw, 30-s wind data for a 
#' single sensor for a specified time period.

dbFetchSensor <- function(db, sensor, start_time, end_time){
    stopifnot(require("RSQLite"))
    con <- dbConnect(SQLite(), dbname = db)
    
    sql <- paste0("SELECT * FROM mean_flow_obs ", 
            "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
            "AND plot_id ='", sensor, "' ", "AND Quality='OK'", collapse="")
            
    res <- dbSendQuery(con, statement = sql)
    d <- fetch(res, n = -1) #fetch all data
    dbClearResult(res)
    dbDisconnect(con)
    
    return(d)
}

#=======================================================
#    Generic fetch
#=======================================================
#' @title Generic fetching function
#' @description
#' \code{dbFetch} returns a dataframe from an SQLite database
#' @param db wind database to query
#' @param query_string query to submit to database
#' @return dataframe
#' @export
#' @details
#' This fucntion returns a dataframe of data returned
#' from \code{query_string} 

dbFetch <- function(db, query_string){
    stopifnot(require("RSQLite"))
    con <- dbConnect(SQLite(), dbname = db)
    
    sql <- paste0(query_string)
            
    res <- dbSendQuery(con, statement = sql)
    d <- fetch(res, n = -1) #fetch all data
    dbClearResult(res)
    dbDisconnect(con)
    
    return(d)
}

#=======================================================
#    Fetch all data from multiple sensors
#=======================================================
#' @title Fetch all data from multiple sensors for a specified time period
#' @description
#' \code{dbFetchMultipleSensors} returns a dataframe from an SQLite database for mulitple 
#' senosrs for a specified time period
#' @param db wind database to query
#' @param sensors list of sensors to extract info for
#' @param start_time format is '2011-08-15 06:00:00'
#' @param end_time format is '2011-08-15 06:00:00'
#' @return dataframe with id, date/time, speed, gust, direction, and quality
#' @export
#' @details
#' This fucntion returns a dataframe of raw, 30-s wind data for 
#' multiple sensors for a specified time period.

dbFetchMultipleSensors <- function(db, sensors, start_time, end_time){
    for(s in length(sensors)){
        d <- dbFetchSensor(db, sensor, start_time, end_time)
        if(s == 1){
            master<-d
        }
        else{
            master <- rbind(master, d)
        }
    }
    return(master)
}

#=======================================================
#    Fetch averaged data from a wind database
#=======================================================
#' @title Fetch averaged data from an SQLite database for a specified time period
#' @description
#' \code{dbFetchAvg} returns a dataframe from an SQLite database for a specified time period
#' @param db database to query
#' @param start_time format is '2011-08-15 06:00:00'
#' @param end_time format is '2011-08-15 06:00:00'
#' @param avg_time averaging time in minutes
#' @return dataframe with id, date/time, speed, gust, direction, and quality
#' @export
#' @details
#' This fucntion returns a dataframe of averaged data for a specified
#' time period. The data is averaged at the top of the hour.
#' Currently only connects to SQLite db. 

dbFetchAvg <- function(db, start_time, end_time, avg_time){
    stopifnot(require("RSQLite"))
    stopifnot(require("plyr"))
    con <- dbConnect(SQlite(), dbname = db)
    
    sql <- paste0("SELECT * FROM mean_flow_obs ", 
            "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
            "AND Quality='OK'", collapse="")
            
    res <- dbSendQuery(con, statement = sql)
    d <- fetch(res, n = -1) #fetch all data

    #compute avgerage at top of hour
    #d <- ?plyr

    dbClearResult(res)
    dbDisconnect(con)
    
    return(d)
}




