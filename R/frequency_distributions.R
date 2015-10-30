#=======================================================
#     Generate a look up table for wind direction
#=======================================================
#' @title Generate a lookup table 
#' @description
#' \code{createLookUpTable} returns a wind direction lookup table
#' @param df dataframe with columns 'plot', 'lat', 'lon'
#' @param pathToRasters path to the raster files (must be in order)
#' @param rasterNames vector of individual raster names
#' @return dataframe
#' @export
#' @details
#' This function crates a lookup table of wind directions
#' for use in generating wind direction distributions.
#'

createLookUpTable <- function(df, pathToRasters, rasterNames){
    stopifnot(require("raster"))
    stopifnot(require("sp"))
    #spatial points to extract data from raster
    r<-raster(paste0(pathToRasters, rasterNames[1]))
    points<-(cbind(df$lon,df$lat))
    sp <- SpatialPoints(points, proj4string=CRS("+proj=longlat +datum=WGS84"))
    sp <- spTransform(sp, CRS(proj4string(r)))  

    #loop over the raster files
    for(i in 1:length(rasterNames)){
        print (i)
        path<-paste0(pathToRasters, rasterNames)[i]
        print(path)
        r<-raster(path)
        #extract the data from the raster object
        assign(paste0("e_", i), extract(r, sp, method='simple')) #nearest-neighbor
        #add new data to dataframe
        df<-cbind(df, get(paste0("e_",i)))
    }

    colnames(df)<-c("plot","lat","lon","0","23","45","68","90","113","135","158",
               "180","203","225","248","270","293","315","338")

    return(df)
}

#=======================================================
#     Generate a distribution from a look up table
#=======================================================
#' @title Generate wind direction distribution 
#' @description
#' \code{distributionFromTable} returns a wind direction distribution
#' @param v vector of binned observed wind directions (number of bins has to equal the number of bins in table) 
#' @param table the lookup table to use
#' @return dataframe of wind direction distributions
#' @export
#' @details
#' This function generates a distribution of wind directions
#' based on a lookup table and vector of observed directions.


distributionFromTable <- function(v, table){

    dist<-data.frame(rbind(rep(NA,1)))

    #look up direction from table
    lookUpDir<-function(bin, rowindex){  
        dir<-table[,as.character(eval(bin))][rowindex]
        return(dir)
    }

    #create the direction distributions (f1_dir, f2_dir, f3_dir, etc.)
    for(i in 1:length(table$plot)){
        assign(paste0(table$plot[i],'_dir'),(mapply(lookUpDir, v, i)))
        dist<-cbind(dist, get(paste0(table$plot[i],'_dir')))
    }
    
    dist[1]<-NULL #drop the column of NAs
    colnames<-paste0(table$plot,'_dir')
    colnames(dist)<-colnames

    return(dist)
}






