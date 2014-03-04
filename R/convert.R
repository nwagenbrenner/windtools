#=======================================================
# convert speed to u,v
#=======================================================
#' @title Convert speed/direction to u-component of wind
#' @description
#' \code{speed2u} returns u-component of wind
#' @param s wind speed
#' @param d wind direction in degrees
#' @return u-component of wind
#' @export
#' @details
#' This fucntion takes a wind speed and direction and returns
#' the u-component of the wind. Direction must be supplied in
#' degrees.
#'
#' @examples
#' data(wind)
#' speed2u(wind$obs_speed[1], wind$obs_dir[1])

speed2u<-function(s,d){
    u <- -s * sin(d * pi/180)
    return (u) 
}

#' @title Convert speed/direction to v-component of wind
#' @description
#' \code{speed2v} returns v-component of wind
#' @param s wind speed
#' @param d wind direction in degrees
#' @return v-component of wind
#' @export
#' @details
#' This fucntion takes a wind speed and direction and returns
#' the v-component of the wind. Direction must be supplied in
#' degrees.
#'
#' @examples
#' data(wind)
#' speed2v(wind$obs_speed[1], wind$obs_dir[1])

speed2v<-function(s,d){
    v <- -s * cos(d * pi/180)
    return (v)
}

#' @title Convert u,v to wind speed
#' @description
#' \code{uv2speed} returns wind speed
#' @param u 
#' @param v 
#' @return wind speed
#' @export
#' @details
#' This function converts u, v to wind speed.
#'

uv2speed<-function(u,v){
    spd <- ((u*u+v*v)**0.5)
    return (spd)
}

#' @title Convert u,v to wind direction
#' @description
#' \code{uv2dir} returns wind direction
#' @param u 
#' @param v 
#' @return wind direction
#' @export
#' @details
#' This function converts u, v to wind direction
#' in degrees.

uv2dir<-function(u,v){
    dir = atan2(-u,-v)*180/3.14
        if(dir < 0){
            dir = dir + 360
        }
    return(dir)
}
        
