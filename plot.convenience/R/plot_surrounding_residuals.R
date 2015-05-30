# ==============================================================================
#                                                     PLOT_SURROUNDING_RESIDUALS 
# ==============================================================================
#' @title plot_surrounding_residuals
#' @description Creates a plot of what the residuals are for values surrounding 
#'      a central point that you specify. You specify how much to deviate from 
#'      the central point, and what measure of error you want. 
#' @details This can be used to visualise whether some value you picked out is 
#' indeed a local minimum, or it can be used to visually explore possible values 
#' via an informed trial and error (where the plot may guide you in which 
#' direction to follow). You chose how wide to make your search through the 
#' \code{"dev"} argument. 
#' 
#' @param x (vector of numerics) The empirical values
#' @param center (numeric) The value you wish to center you search around
#' @param dev (numeric) How far to the left and right should the search span
#' @param type (string) What measure of error should it plot? 
#' 
#'        "SSE"    # Sum of Squared Errors
#' 
#'        "MSE"    # Mean Squared Error
#' 
#'        (DEFAULT = "SSE")
#' 
#' @param res (integer)  Sets the resolution of the plot. Calculated as the 
#'      number of points that lie between (center - dev) and  (center + dev). 
#' 
#'      DEFAULT = 100
#' @param ... other arguments to be passed to plot
#' @examples
#' x <- c(12,11,13,15,16,12,11,9,14,11,10,7,15)
#' plot_surrounding_residuals(x)
#' plot_surrounding_residuals(x, center=7, dev=10)
#' plot_surrounding_residuals(x, center=13, dev=2, type="MSE")
#' plot_surrounding_residuals(x, center=13, dev=2, type="SSE")
#' 
#' @author Ronny Restrepo
#' @export plot_surrounding_residuals
# ==============================================================================
plot_surrounding_residuals <- 
    function(x, center=mean(x), dev=2*sd(x), type="SSE", res=100, ...){
    mean.x = mean(x)
    min = center - dev
    max = center + dev
    mus = seq(min, max, by = (max-min)/res)
    if (type=="MSE"){
        n = length(x)
        MSE = sapply(mus, function(mu) sum( (x - mu)^2 )/n)
        plot(mus, MSE, type="l", col="blue", 
             main="Residuals Plot", xlab="Values of mu Considered", 
             ylab="Mean Squared Error", ...)    
    } else {
        SSE = sapply(mus, function(mu) sum( (x - mu)^2 ))
        plot(mus, SSE, type="l", col="blue", 
             main="Residuals Plot", xlab="Values of mu considered", 
             ylab="Sum of Squared Errors", ...)    
    }
    abline(v=center, col="red")
}



