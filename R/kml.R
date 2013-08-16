#=======================================================
#    Write a kml from raster
#=======================================================
#' @title Write a kml from a raster object
#' @description
#' \code{raster2kml} writes a kml from a raster object
#' @param r raster object 
#' @param outfile output file name
#' @export
#' @details
#' This fucntion writes a kml representation of a raster object.
#' @examples
#' r <- raster(ncol=10, nrow=10)
#' values(r) <- 1:ncell(r)
#' extent(r)<-c(-113.5732, -112.4852, 43.04848, 43.81728)
#' raster2kml(r, 'test_kml')

raster2kml <- function(r, outfile){
    stopifnot(require(plotKML))
    newproj <- "+proj=longlat +datum=WGS84"
    r2<-projectRaster(r, crs=newproj, method="bilinear") #convert to lat/lon CRS
    KML(r2, outfile, col=rev(terrain.colors(255)),colNA=NA, maxpixels=100000, blur=1, zip='', overwrite=TRUE)
}

points2kml <- function(points, outfile){
    #get unique lat/lons 
    latlon <- aggregate(x=cbind(points$lat, points$lon), by=list(points$plot), mean)
    colnames(latlon) <- c('plot', 'lat', 'lon')
    stopifnot(require(plotKML))
    stopifnot(require(sp))
    points<-cbind(latlon$lon, latlon$lat)
    sp<-SpatialPoints(points, proj4string=CRS("+proj=longlat +datum=WGS84"))
    KML(sp, 'testpoints', zip='', overwrite=TRUE)
}

#get unique lat/lons 
#latlon <- aggregate(x=cbind(bsb$lat, bsb$lon), by=list(bsb$plot), mean)
#colnames(latlon) <- c('plot', 'lat', 'lon')
#points<-cbind(latlon$lon, latlon$lat)
#sp<-SpatialPoints(points, proj4string=CRS("+proj=longlat +datum=WGS84"))
#KML(sp, 'testpoints', zip='', overwrite=TRUE)





