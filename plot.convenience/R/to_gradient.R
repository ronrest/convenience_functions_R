# TODO: have a way to specify your own color gradient theme. 
# TODO: have option to use non-linear interpolations of gradient, eg log. 

#===============================================================================
#                                                                    TO_GRADIENT
#===============================================================================
#' @title to_gradient
#' @description Takes a vector of values, and returns a mapping of those values 
#'              to a gradient of colors. 
#' @details You provide a vector of values, and the function maps those values 
#'          to a color that falls within a gradient of colors. 
#'          
#'          The mapping of colors can be based on minimum value from the vector, 
#'          to maximum value in that vector. Or you can specify that the values 
#'          in the vector fall within some theoretical minimum and maximum 
#'          (which may not be represeneted by any actual value in the vector). 
#'          eg, you may want a gradient that represents possible values between 
#'          0 to 100, even though your vector might only include values between 
#'          20 to 76. 
#'          
#'          You can also specify a preset theme for the color gradient.
#'
#' @param x (vector) The vector of values you wish to map
#' @param xrange (vector) A vector with 2 elements, representing min and max 
#'        values that x can possibly take. 
#'        
#'        If no value is provided here, then it just uses the min and max values
#'        of x itself.  
#'        
#'        (Default is range(x))
#' @param theme (string)
#' 
#'      "flame"
#'      
#'      "blue"
#'      
#'      "rainbow"
#'
#' @author Ronny Restrepo
#' @examples
#' x = c(4,7,3,1,9,6)
#' grad = to_gradient(x)
#' plot(x, pch=19, col=grad)
#' 
#' @keywords to_gradient gradient theme color
#' @import scales
#' @export to_gradient
#===============================================================================
to_gradient <- function(x, xrange=range(x), theme="flame"){
    gradientTheme = .create.theme(theme)
    rescaledValues = scales::rescale(x, to = c(1, 10), from = c(xrange[1], xrange[2]))
    #rescaledColors = .gradient.interpolation(x)
    return(gradientTheme(10)[rescaledValues])
}
    




#===============================================================================
#                                                                  .CREATE.THEME
#===============================================================================
#' generates a gradient object based on a preset theme
#' 
#===============================================================================
.create.theme <- function(grad.theme="flame", grad.scal="normal"){
    if (grad.theme=="rainbow"){
        gradientTheme <- colorRampPalette(c("darkblue", "blue", "cyan", "green", 
                                            "yellow", "orange", "red", "darkred"))
    } else if (grad.theme == "blue"){
        gradientTheme <- colorRampPalette(c("lightblue", "darkblue"))
    } else if (grad.theme == "flame"){
        gradientTheme <- colorRampPalette(c("yellow", "red"))
    } else {
        gradientTheme <- colorRampPalette(c("lightgrey", "black"))
    }
    return(gradientTheme)
}

#===============================================================================
#                                                        .GRADIENT.INTERPOLATION
#===============================================================================
#' takes a vector of numerical values and rescales them to be between 0 anf 10. 
#' You can speccify the method of interpolating the scaling. 
#' 
#' @param vals (vector of numerics)
#' @param scale.mode 
#' 
#'      "normal"
#'      
#'      "range"
#'      
#'      any other value, returns the original data, unscaled. 
#' @return a vector of the rescaled values between 0 and 10. 
#===============================================================================
.gradient.interpolation <- function(vals, scale.mode="normal"){
    # Set the way the gradient is interpolated
    # Linear scaling from min to max
    if (scale.mode=="range"){
        range1 = range(vals)
        span1 = range1[2] - range1[1]
        min1 = min(vals)
        rmult = 10/span1
        newscale = (vals - min1) * rmult 
        
        # Normal Scaling
    } else if (scale.mode == "normal"){
        newscale = 2 * (vals - mean(vals)) / sd(vals) + 5    
        
        # No scaling 
    } else {
        newscale = vals
    }
    return(newscale)
}


