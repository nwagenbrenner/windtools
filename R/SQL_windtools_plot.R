#SQL query to windtools-compatible format for use in plotting functions (makeVectorMap, etc.)

library(devtools)
library(windtools)
library(raster)
library(rgdal)
library(dplyr)
library(RSQLite)

#specify database and directory to query
dbConnect(RSQLite::SQLite(), dbname='src.sqlite')
dbPath <- '/home/abbyindreland/src/src.sqlite'

#specify interval
start <- '2011-07-16 05:00:00'
end <- '2011-07-16 12:00:00'

df<-dbFetchAll(db=dbPath, start_time=start, end_time=end)

#specify lat/lon txt file and directory
#the same txt file is called in twice for 'match' to recognize lat and lon as two separate entities
latloc<-read.table('/home/abbyindreland/src/Abby/SalmonRiverCanyon/salmon_locations.txt', sep=",", skip = 1, header=FALSE)
colnames(latloc) <- c('plot_id', 'lat', 'lon', 'z')
latloc$lon <- NULL
df$lat<-latloc[match(df$plot_id, latloc$plot_id),2]

lonloc<-read.table('/home/abbyindreland/src/Abby/SalmonRiverCanyon/salmon_locations.txt', sep=",", skip = 1, header=FALSE)
colnames(lonloc) <- c('plot_id', 'lat', 'lon', 'z')
lonloc$lat <- NULL
df$lon<-lonloc[match(df$plot_id, lonloc$plot_id),2]

df$quality <- NULL
df$sensor_qual <- NULL
df$wind_gust <- NULL

df <- df[c("plot_id", "lat", "lon", "date_time", "wind_speed", "wind_dir")]

colnames(df) <- c("plot", "lat", "lon", "datetime", "obs_speed", "obs_dir")

df$obs_speed <- df$obs_speed*0.44704

