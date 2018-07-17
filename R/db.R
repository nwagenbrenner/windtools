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
#' This function returns a dataframe of raw, 30-s wind data for a specified
#' time period.

dbFetchAll <- function(db, start_time, end_time){
    stopifnot(require("RSQLite"))
    con <- dbConnect(SQLite(), dbname = db)
    
    sql <- paste0("SELECT * FROM mean_flow_obs ", 
            "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
            "AND Quality='OK'", collapse="")
            
    res <- dbSendQuery(con, statement = sql)
    d <- fetch(res, n = -1) #fetch all data
   
    if (grepl("src.sqlite", db) == TRUE || grepl("birch.sqlite", db) == TRUE) {
	    d[,"date_time"] <- as.POSIXct(strptime(d[,"date_time"], '%Y-%m-%d %H:%M:%S'))
    }
    else if (grepl("bsb.sqlite", db) == TRUE) {
	    d[,"Date_time"] <- as.POSIXct(strptime(d[,"Date_time"], '%Y-%m-%d %H:%M:%S'))
    }
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
#' This function returns a dataframe of raw, 30-s wind data for a 
#' single sensor for a specified time period.

dbFetchSensor <- function(db, sensor, start_time, end_time){
    stopifnot(require("RSQLite"))
    con <- dbConnect(SQLite(), dbname = db)
    
    sql <- paste0("SELECT * FROM mean_flow_obs ", 
            "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
            "AND plot_id ='", sensor, "' ", "AND Quality='OK'", collapse="")
            
    res <- dbSendQuery(con, statement = sql)
    d <- fetch(res, n = -1) #fetch all data
    if (grepl("src.sqlite", db) == TRUE || grepl("birch.sqlite", db) == TRUE) {
	    d[,"date_time"] <- as.POSIXct(strptime(d[,"date_time"], '%Y-%m-%d %H:%M:%S'))
    }
    else if (grepl("bsb.sqlite", db) == TRUE) {
	    d[,"Date_time"] <- as.POSIXct(strptime(d[,"Date_time"], '%Y-%m-%d %H:%M:%S'))
    }
    dbClearResult(res)
    dbDisconnect(con)
    
    return(d)
}

#=======================================================
#    Fetch the sensor locations from the database
#=======================================================
#' @title Fetch the location (lat/lon) of one or more sensors
#' @description
#' \code{dbFetchSensorLocation} returns the lat/lon of one or more sensors
#' @param db wind database to query
#' @param sensors sensor(s) to extract location for
#' @return dataframe with sensor id(s), lat, lon
#' @export
#' @details
#' This function returns the lat/lon of one or more sensors

dbFetchSensorLocation <- function(db, sensors){
    stopifnot(require("RSQLite"))
    con <- dbConnect(SQLite(), dbname = db)
    
    for (s in 1:length(sensors)){
	    if (grepl("src.sqlite", db) == TRUE || grepl("birch.sqlite", db) == TRUE) {
		    sql <- paste0("SELECT plot_id, geometry, geometry FROM plot_location ",
				  "WHERE plot_id ='", sensors[s], "'", collapse="")
	    }
	    else if (grepl("bsb.sqlite", db) == TRUE) {
		    sql <- paste0("SELECT plot_id, latitude, longitude FROM plot_location ", 
               			  "WHERE plot_id ='", sensors[s], "'", collapse="")
	    }
       
	    res <- dbSendQuery(con, statement = sql)
            d <- fetch(res, n = -1) 
            dbClearResult(res)
            if(s == 1){
            	master <- d
            }
            else{
            	master <- rbind(master, d)
            }  
    }
    dbDisconnect(con)

    if (grepl("src.sqlite", db) == TRUE) {
	    colnames(master) <- c("Plot_id", "Latitude", "Longitude")
	    master$Latitude <- sapply(strsplit(master$Latitude, ","), "[[", 1)
	    master$Latitude <- gsub("POINT", "", master$Latitude)
	    master$Latitude <- as.numeric(substring(master$Latitude, 2))

	    master$Longitude <- sapply(strsplit(master$Longitude, ","), "[[", 2)
	    master$Longitude <- as.numeric(gsub(")", "", master$Longitude))
    }
    else if (grepl("birch.sqlite", db) == TRUE) {
	    colnames(master) <- c("Plot_id", "Latitude", "Longitude")
	    master$Latitude  <- as.numeric(sapply(strsplit(master$Latitude,  " "), "[[", 1))
	    master$Longitude <- as.numeric(sapply(strsplit(master$Longitude, " "), "[[", 2))
    }
    return(master)
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
#' This function returns a dataframe of raw, 30-s wind data for 
#' multiple sensors for a specified time period.

dbFetchMultipleSensors <- function(db, sensors, start_time, end_time){
    for(s in 1:length(sensors)){
        d <- dbFetchSensor(db, sensors[s], start_time, end_time)
        if(s == 1){
            master <- d
        }
        else{
            master <- rbind(master, d)
        }
    }
    return(master)
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
#' This function returns a dataframe of data returned
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
#    Fetch averaged data from a wind database
#=======================================================
#' @title Fetch averaged data from an SQLite database for a specified time period
#' @description
#' \code{dbFetchHourlyAvg} returns hourly averages from an SQLite database for a specified time period
#' @param db database to query
#' @param sensor sensor to extract info for
#' @param start_time format is '2011-08-15 06:00:00'
#' @param end_time format is '2011-08-15 06:00:00'
#' @param avg_time averaging time in minutes
#' @param align where to align the index of the result (left, middle, right)
#' @return time series of wind speed in xts format
#' @export
#' @details
#' This function returns a time series of hourly averaged wind speed data for a specified
#' time period. The data is averaged at the top of the hour and aligned
#' according to the 'align' parameter. Currently only connects to SQLite db.

dbFetchHourlyAvg <- function(db, sensor, start_time, end_time, avg_time, align='center'){
    stopifnot(require("RSQLite"))
    stopifnot(require("plyr"))
    stopifnot(require("xts"))
    con <- dbConnect(SQLite(), dbname = db)
    
    sql <- paste0("SELECT * FROM mean_flow_obs ", 
            "WHERE Date_time BETWEEN '", start_time, "' ", "AND '", end_time, "' ",
            "AND plot_id ='", sensor, "' ", "AND Quality='OK'", collapse="")
            
    res <- dbSendQuery(con, statement = sql)
    d <- fetch(res, n = -1) #fetch all data

    if (grepl("src.sqlite", db) == TRUE || grepl("birch.sqlite", db) == TRUE) {
	    d[, "date_time"] <- as.POSIXct(strptime(d[, "date_time"],
			   '%Y-%m-%d %H:%M:%S'), tz='America/Denver')
	    
	    #compute average at top of hour
	    #convert to xts format
	    ts <- xts(d$wind_speed, d$date_time)
    }
    else if (grepl("bsb.sqlite", db) == TRUE) {
	     d[,"Date_time"] <- as.POSIXct(strptime(d[,"Date_time"],
                           '%Y-%m-%d %H:%M:%S'), tz='America/Denver')

    	     #compute avgerage at top of hour
    	     #convert to xts format
    	     #ts<-xts(d$Wind_speed*0.447, d$Date_time) #*0.447 converts mph->m/s
    	     ts<-xts(d$Wind_speed, d$Date_time)
    }

    #compute a rolling average
    #avg_time*2 is integer of rolling window (we have 2 records per minute)
    rAvg<-rollmean(ts, avg_time*2, align=align)
    #extract the value from rAvg every hour
    e<-endpoints(rAvg,on="hours")
    ee<-rAvg[e]

    dbClearResult(res)
    dbDisconnect(con)
    
    return(ee)
}

#==============================================================
#    Fetch all data from an sqlite database in windtools format
#==============================================================
#' @title Fetch all data from an SQLite database in windtools format for a specified period of time
#' @description
#' \code{dbFetchWind} returns a dataframe from an SQLite database for a specified period of time
#' @param db wind database to query
#' @param sensors sensors to extract info for
#' @param start_time format is '2011-08-15 06:00:00'
#' @param end_time format is '2011-08-15 6:00:00'
#' @param progressBar includes progress bar for shiny web interface
#' @return dataframe with id, date/time, speed, gust, direction, quality, sensor quality, latitude, and longitude
#' @export
#' @details
#' This function returns a dataframe of raw, 30-s wind data for sensor for a specified time period in the windtools package format.

dbFetchWind <- function(db, sensors, start_time, end_time, progressBar=FALSE) {
	# Pulls database for sensor(s) and puts it in format usable by windtools package
	stopifnot(require("RSQLite"))
	con <- dbConnect(SQLite(), dbname = db)

	if (progressBar == TRUE) {
		stopifnot(require("shiny"))
		progress <- shiny::Progress$new()
		on.exit(progress$close())
		progress$set(message = "Accessing Data", value = 0)
	}

	for (s in 1:length(sensors)) {
		# Get wind data for sensor
		sql <- paste0("SELECT * FROM mean_flow_obs ",
			      "WHERE Date_time BETWEEN '", start_time, "' ", "AND '",
			      end_time, "' ", "AND plot_id = '", sensors[s], "' ",
			      "AND Quality='OK'", collapse="")
		res <- dbSendQuery(con, statement = sql)
		d <- fetch(res, n = -1) #fetch all data
		dbClearResult(res)

		# Get sensor lat/lon
		if (grepl("src.sqlite", db) == TRUE) {
			sql <- paste0("SELECT plot_id, geometry, geometry FROM plot_location ",
				      "WHERE plot_id = '", sensors[s], "'", collapse="")
			res <- dbSendQuery(con, statement = sql)
			latlon <- fetch(res, n = -1)
			dbClearResult(res)

			# Pull coordinates from text strings: POINT(###,###)
			colnames(latlon) <- c("Plot_id", "Latitude", "Longitude")
			latlon$Latitude <- sapply(strsplit(latlon$Latitude, ","), "[[", 1)
			latlon$Latitude <- gsub("POINT", "", latlon$Latitude)
			latlon$Latitude <- as.numeric(substring(latlon$Latitude, 2))
			latlon$Longitude <- sapply(strsplit(latlon$Longitude, ","), "[[", 2)
			latlon$Longitude <- as.numeric(gsub(")", "", latlon$Longitude))

			# Format lat/lon matrix
			latlon <- matrix(c(rep(latlon[2], NROW(d)), rep(latlon[3], NROW(d))),
		       			nrow = NROW(d), ncol = 2)	
		}
		else if (grepl("birch.sqlite", db) == TRUE) {
			sql <- paste0("SELECT plot_id, geometry, geometry FROM plot_location ",
				      "WHERE plot_id = '", sensors[s], "'", collapse="")
			res <- dbSendQuery(con, statement = sql)
			latlon <- fetch(res, n = -1)
			dbClearResult(res)

			# Pull coordinates from text strings: #### ####
			colnames(latlon) <- c("Plot_id", "Latitude", "Longitude")
			latlon$Latitude <-  as.numeric(sapply(strsplit(latlon$Latitude,  " "), "[[", 1))
			latlon$Longitude <- as.numeric(sapply(strsplit(latlon$Longitude, " "), "[[", 2))

			# Format lat/lon matrix
			latlon <- matrix(c(rep(latlon[2], NROW(d)), rep(latlon[3], NROW(d))), nrow = NROW(d), ncol = 2)
		}
		else if (grepl("bsb.sqlite", db) == TRUE) {
			sql <- paste0("SELECT latitude, longitude FROM plot_location ",
				      "WHERE plot_id ='", sensors[s], "'", collapse="")
			res <- dbSendQuery(con, statement = sql)
			latlon <- fetch(res, n = -1)
			dbClearResult(res)

			# Format lat/lon matrix
			latlon <- matrix(c(rep(latlon[1], NROW(d)), rep(latlon[2], NROW(d))), 
					 nrow = NROW(d), ncol = 2)
		}

		# Combine data
		d <- cbind(d, latlon)
		if (s == 1) {
			master <- d
		}
		else {
			master <- rbind(master, d)
		}

		if (progressBar == TRUE) {
			progress$inc(1/length(sensors), detail = paste("Sensor", s))
		}
	}

	dbDisconnect(con)

	# Rename columns to windtools standards
	colnames(master) <- c("plot", "datetime", "obs_speed", "wind_gust", "obs_dir", 
			      "quality", "sensor_quality", "lat", "lon")
	
	# Conversions
	master$obs_speed <- master$obs_speed*1609.34/3600 # convert to m/s
	master$wind_gust <- master$wind_gust*1609.34/3600 # convert to m/s
	master$datetime <- as.POSIXct(strptime(master$datetime, '%Y-%m-%d %H:%M:%S'))
	master <- transform(master, lat = as.numeric(lat, lon = as.numeric(lon)))

	return(master)
}

