



#===============================================================================
#                                                    ERROR
#===============================================================================
#' @title error
#' @description calculates the error value for some vector of values
#' @details calculates the error value for some vector of values
#'
#' @param x.pred (numeric) predicted value(s)
#' @param x.actual (numeric) actual values
#' @param type (string) type of error function to use. 
#' 
#'          "MSE"  Mean Squared Error
#'          
#'          "RMSE" Root Mean Squared Error
#'
#' @author Ronny Restrepo
#' @examples
#' 
#' mu = mean(mtcars$mpg) 
#' error(mu, mtcars$mpg, type="MSE")
#' 
#' @keywords error, residuals, stats, stat.convenience 
#' @export error
#===============================================================================
error <- function(x.pred, x.actual, type="MSE"){
    if (type=="MSE"){
        return(mean((x.actual - x.pred)^2))
    }
    else if (type=="RMSE"){
        return(sqrt(mean((x.actual - x.pred)^2)))
    }
}
