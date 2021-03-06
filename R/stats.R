#' @title Calculate the root mean squared error
#' @description
#' \code{rmse} returns the root mean squared error
#' @param x vector
#' @return root mean squared error
#' @export
#' @details
#' This fucntion returns the root mean squred
#' error of a vector
rmse <- function(x){
    sqrt(sum(x^2)/(length(x)))
}

#' @title Calculate the standard deviation of error
#' @description
#' \code{sde} returns the standard deviation of error
#' @param x vector
#' @return standard deviation of error
#' @export
#' @details
#' This fucntion returns the standard deviation of
#' error of a vector
sde <- function(x){
    sqrt(sum((x - mean(x))^2)/(length(x)-1))
}

#' @title Calculate the mean absolute error
#' @description
#' \code{mae} returns the mean absolute error
#' @param x vector
#' @return mean absolute error
#' @export
#' @details
#' This fucntion returns the mean absolute
#' error of a vector
mae <- function(x){
    sum(abs(x))/length(x)
}
