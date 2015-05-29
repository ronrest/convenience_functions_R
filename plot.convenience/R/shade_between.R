#TODO: Find a better alternative to Hmisc, that doesnt rely on so many other 
#      secondary libraries which slow load time.
#      Hmisc is used here for floating point matches
#      find.matches() requires Hmisc

# ==============================================================================
#                                                                  SHADE BETWEEN
# ==============================================================================
#' shade_between
#' 
#' Creates a plot which shades the area under the curve between "lower" and 
#' "upper" points along x. 
#' 
#' Note, that depending on the resolution of the x points, you may not 
#' actually get the shaded region in the exact upper and lower points you 
#' specified. The edges of the shaded region will snap to the closest value 
#' that is actually in the x vector.
#' 
#' @param  x (vector) x coordinates of the plot
#' @param  y (vector) y coordinates of the plot
#' @param  lower (numeric) lower point (Output snaps to closest element that is 
#'          actually in x)
#' @param  upper (numeric) uppper point (Output snaps to closest element that is 
#'          actually in x)
#' @param primary (logical) Should it plot as a new plot (TRUE), or 
#'          overlay on top of an existing plot (FALSE)
#'          
#'          (DEFAULT = TRUE)
#' @param  shade.col (string) color to fill in the shaded area.
#' @param  shade.border (string, NA, NULL, logical) the color to draw the border. 
#'          The default,  NULL, means to use par("fg"). 
#'          
#'          Use border = NA to omit borders.
#'          
#'          For compatibility with S, border can also be logical, in 
#'          which case FALSE is equivalent to NA (borders omitted) 
#'          
#'          TRUE is equivalent to NULL (use foreground colour)
#' @param  shade.lty (string) the line type to be used for the shaded area
#' 
#' @param  shade.density (numeric) the density of shading lines, in lines per 
#'          inch. The  default value of NULL means that no shading lines are 
#'          drawn. A zero value of density means no shading nor 
#'          filling whereas negative values and NA suppress shading 
#'          (and so allow color filling).
#' 
#' @param  shade.angle (integer) the slope of shading lines, given as an angle 
#'          in degrees (counter-clockwise).
#' @param  ... aditional parameters for the plot, such as type, lty, etc
#' 
#' @examples 
#'    # Shades between -5 and -2 in a bell curve
#'    x = seq(-9,9,by=0.01)
#'    y = dnorm(x, mean=0, sd=3)
#'    shade_between(x,y,-5,-2, type="l", lwd=3, shade.density=20)
#' @import Hmisc
#' @export
shade_between <- function(x,y, lower, upper, primary=TRUE, shade.col="red", 
                          shade.border=NULL, 
                          shade.lty=par("lty"), shade.density=NULL, 
                          shade.angle=45, ...){
    # TODO: include an argument direction, to specify if the upper/lower points 
    #       should be along the vertical or horizontal direction
    
    #-------------------------------------------------------------------------
    #                                                                    Setup
    #-------------------------------------------------------------------------
    tolerance = tol=.Machine$double.eps ^ 0.5  # For floating point comparisons
    
    #-------------------------------------------------------------------------
    #                                      Create points for the shaded region
    #-------------------------------------------------------------------------
    # look for values in x between "lower" and "upper" 
    x2 = c(x[x+tolerance>=lower & x<=upper+tolerance])
    x2 = c(x2[1], x2, x2[length(x2)])   # Adds end points
    
    # Look for subset of points in y that correspont to the region of interest
    lower_index = Hmisc::find.matches(x2[2], x, tol=tolerance, maxmatch=1)$matches[[1]]
    upper_index = Hmisc::find.matches(x2[length(x2)-1], x, tol=tolerance, maxmatch=1)$matches[[1]]
    y2 = c(0, y[lower_index: upper_index],0)
    
    
    #-------------------------------------------------------------------------
    #                                                                  Plot it
    #-------------------------------------------------------------------------
    #preliminary plot to get the axes drawn
    if (primary){
        plot(x,y, ...)
    }
    
    # The sahded region
    polygon(x2,y2,col=shade.col, border=shade.border, lty=shade.lty, 
            density=shade.density, angle=shade.angle)
    
    # Now draw the actual plot to draw over the shaded area.
    points(x,y, ...)    
}
