
library(Hmisc)  # Used for floating point matches



shade_between <- function(x,y, lower, upper, shade.col="red", shade.border=NULL, 
                          shade.lty=par("lty"), shade.density=NULL, 
                          shade.angle=45, ...){
    #===========================================================================
    #                                                              SHADE BETWEEN
    #===========================================================================
    # Creates a plot which shades the area under the curve between points lower 
    # and upper. 
    # 
    # Note, that depending on the resolution of the x points, you may not 
    # actually get the shaded region in the exact upper and lower points you 
    # specified. The edges of the shaded region will snap to the closest value 
    # that is actually in the x vector.
    # 
    # ARGS: 
    #   x     : vec x coordinates of the plot
    #   y     : vec y coordinates of the plot
    #   lower : lower point (Output snaps to closest element that is actually in x)
    #   upper : lower point (Output snaps to closest element that is actually in x)
    #
    # OPTIONAL ARGS: 
    #   shade.col     : color to fill in the shaded area.
    #   shade.border : the color to draw the border. The default, NULL, means to 
    #                  use par("fg"). Use border = NA to omit borders.
    #                  
    #                  For compatibility with S, border can also be logical, in 
    #                  which case FALSE is equivalent to NA (borders omitted) 
    #                  and TRUE is equivalent to NULL (use foreground colour)
    #   shade.lty     : the line type to be used for the shaded area
    # 
    #   shade.density : the density of shading lines, in lines per inch. The 
    #                   default value of NULL means that no shading lines are 
    #                   drawn. A zero value of density means no shading nor 
    #                   filling whereas negative values and NA suppress shading 
    #                   (and so allow color filling).
    # 
    #   shade.angle	  : the slope of shading lines, given as an angle in degrees
    #                   (counter-clockwise).
    #   ...         : aditional parameters for the plot, such as type, lty, etc
    # 
    # TODO: include an argument direction, to specify if the upper/lower points 
    #       should be along the vertical or horizontal direction
    #
    # EXAMPLES:
    # 
    #    # Shades between -5 and -2 in a bell curve
    #    x = seq(-9,9,by=0.01)
    #    y = dnorm(x, mean=0, sd=3)
    #    shade_between(x,y,-5,-2, type="l", lwd=3, shade.density=20)
    #
    #===========================================================================
    
    #---------------------------------------------------------------------------
    #                                                                      Setup
    #---------------------------------------------------------------------------
    tolerance = tol=.Machine$double.eps ^ 0.5
    
    # Create points for the shaded region
    x2 = c(x[x+tolerance>=lower & x<=upper+tolerance])
    x2 = c(x2[1], x2, x2[length(x2)])
    
    lower_index = find.matches(x2[2], x, tol=tolerance, maxmatch=1)$matches[[1]]
    upper_index = find.matches(x2[length(x2)-1], x, tol=tolerance, maxmatch=1)$matches[[1]]
    y2 = c(0, y[lower_index: upper_index],0)
    
    
    #---------------------------------------------------------------------------
    #                                                                    Plot it
    #---------------------------------------------------------------------------
    plot(x,y, ...)
    
    polygon(x2,y2,col=shade.col, border=shade.border, lty=shade.lty, 
            density=shade.density, angle=shade.angle)
    
    # Draw over the shaded area border.
    points(x,y, ...)    
}



shade_outside <- function(x,y, lower, upper, shade.col="red", shade.border=NULL, 
                          shade.lty=par("lty"), shade.density=NULL, 
                          shade.angle=45, ...){
    #===========================================================================
    #                                                              SHADE BETWEEN
    #===========================================================================
    # Creates a plot which shades the area under the curve of the region outside 
    # lower and upper. SO for example it can be used to shade the tails of a 
    # bell curve. 
    # 
    # Note, that depending on the resolution of the x points, you may not 
    # actually get the shaded region in the exact upper and lower points you 
    # specified. The edges of the shaded region will snap to the closest value 
    # that is actually in the x vector.
    # 
    # ARGS: 
    #   x     : vec x coordinates of the plot
    #   y     : vec y coordinates of the plot
    #   lower : lower point (this MUST be an actual element in the x vector)
    #   upper : lower point (this MUST be an actual element in the x vector)
    #
    # OPTIONAL ARGS: 
    #   shade.col     : color to fill in the shaded area.
    #   shade.border : the color to draw the border. The default, NULL, means to 
    #                  use par("fg"). Use border = NA to omit borders.
    #                  
    #                  For compatibility with S, border can also be logical, in 
    #                  which case FALSE is equivalent to NA (borders omitted) 
    #                  and TRUE is equivalent to NULL (use foreground colour)
    #   shade.lty     : the line type to be used for the shaded area
    # 
    #   shade.density : the density of shading lines, in lines per inch. The 
    #                   default value of NULL means that no shading lines are 
    #                   drawn. A zero value of density means no shading nor 
    #                   filling whereas negative values and NA suppress shading 
    #                   (and so allow color filling).
    # 
    #   shade.angle      : the slope of shading lines, given as an angle in degrees
    #                   (counter-clockwise).
    #   ...         : aditional parameters for the plot, such as type, lty, etc
    # 
    # TODO: include an argument direction, to specify if the upper/lower points 
    #       should be along the vertical or horizontal direction
    #
    # EXAMPLES:
    # 
    #    # Shades the tails before -5 and after 5 in a bell curve
    #    x = seq(-9,9,by=0.01)
    #    y = dnorm(x, mean=0, sd=3)
    #    shade_outside(x,y,-5, 5, type="l", lwd=3, shade.density=20)
    #
    #===========================================================================
    
    #---------------------------------------------------------------------------
    #                                                                      Setup
    #---------------------------------------------------------------------------
    
    tolerance = tol=.Machine$double.eps ^ 0.5
    # Create points for the left hand shaded region
    x2 = x[x<=lower+tolerance]
    x2 = c(x2[1], x2, x2[length(x2)]) 
        
    lower_index = find.matches(x2[length(x2)], x, tol=tolerance, maxmatch=1)$matches[[1]]
    y2 = c(0, y[1: lower_index],0)
    
    
    # Create points for the right hand shaded region
    x3 = x[x+tolerance>=upper]
    x3 = c(x3[1], x3, x3[length(x3)])
    
    upper_index = find.matches(x3[2], x, tol=tolerance, maxmatch=1)$matches[[1]]
    y3 = c(0, y[upper_index: length(x)],0)
    
        
    #---------------------------------------------------------------------------
    #                                                                    Plot it
    #---------------------------------------------------------------------------
    plot(x,y, ...)
    
    polygon(x2,y2,col=shade.col, border=shade.border, lty=shade.lty, 
            density=shade.density, angle=shade.angle)
    polygon(x3,y3,col=shade.col, border=shade.border, lty=shade.lty, 
            density=shade.density, angle=shade.angle)
    
    # Draw over the shaded area border.
    points(x,y, ...)    
}






# Shades the tails before -5 and after 5 in a bell curve
#x = seq(-9,9,by=0.9)
#y = dnorm(x, mean=0, sd=3)
#shade_outside(x,y,-3, 2, type="o", lwd=2, shade.density=20)





