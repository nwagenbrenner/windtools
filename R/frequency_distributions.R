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
#' for use in generating wind direction distributions/
#'
#' @examples

createLookUpTable <- function(df, pathToRasters, rasterNames){

    #spatial points to extract data from raster
    r<-raster(paste0(pathToRasters, rasterNames[1]))
    points<-(cbind(df$lon,df$lat))
    sp <- SpatialPoints(points, proj4string=CRS("+proj=longlat +datum=WGS84"))
    sp <- spTransform(sp, CRS(proj4string(r)))

    directions<-seq(0, 359, by=22.5)
    #loop over the raster files
    for(i in directions){
        print (i)
        i<-round(i+0.1) #0.5 can go either way, 0 or 1
        path<-paste0(pathToRasters, rasterNames[i])
        print (path)
        r<-raster(path)
        #extract the data from the raster object
        assign(paste0("e_", i), extract(r, sp, method='simple')) #nearest-neighbor
        #add new data to dataframe
        d<-cbind(d, get(paste0("e_",i)))
    }

    colnames(d)<-c("plot","lat","lon","0","23","45","68","90","113","135","158",
               "180","203","225","248","270","293","315","338")
    return(d)
}

#=======================================================
#     Generate a distribution from a look up table
#=======================================================
#' @title Generate wind direction distribution 
#' @description
#' \code{distributionFromTable} returns a wind direction distribution
#' @param v vector of observed wind directions
#' @param table the lookup table to use
#' @return dataframe of wind direction distributions
#' @export
#' @details
#'
#' @examples

distributionFromTable <- function(v, table){
    
    bin<-round(v + 0.1) #since 0.5 could go to 0 or 1

    dist<-data.frame(rbind(rep(NA,2)))

    #create the direction distributions (f1_dir, f2_dir, f3_dir, etc.)
    for(i in 1:length(table$plot)){
        assign(paste0(table$plot,'_dir'),(mapply(lookUpDir, bin, i)))
        dist<-rbind(dist, paste0(table$plot,'_dir'))
    }

    #look up direction from table
    lookUpDir<-function(bin, rowindex){  
        dir<-table[,as.character(eval(bin))][rowindex]
        return(dir)
    }

    return(dist)
}






