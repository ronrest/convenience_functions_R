
library(Hmisc)  # Used for floating point matches
#TODO: Find a better alternative to Hmisc, that doesnt rely on so many other 
#      secondary libraries which slow load time.


shade_between <- function(x,y, lower, upper, shade.col="red", shade.border=NULL, 
                          shade.lty=par("lty"), shade.density=NULL, 
                          shade.angle=45, ...){
    #===========================================================================
    #                                                              SHADE BETWEEN
    #===========================================================================
    # Creates a plot which shades the area under the curve between "lower" and 
    # "upper" points along x. 
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
    lower_index = find.matches(x2[2], x, tol=tolerance, maxmatch=1)$matches[[1]]
    upper_index = find.matches(x2[length(x2)-1], x, tol=tolerance, maxmatch=1)$matches[[1]]
    y2 = c(0, y[lower_index: upper_index],0)
    
    
    #-------------------------------------------------------------------------
    #                                                                  Plot it
    #-------------------------------------------------------------------------
    #preliminary plot to get the axes drawn
    plot(x,y, ...)
    
    # The sahded region
    polygon(x2,y2,col=shade.col, border=shade.border, lty=shade.lty, 
            density=shade.density, angle=shade.angle)
    
    # Now draw the actual plot to draw over the shaded area.
    points(x,y, ...)    
}



shade_outside <- function(x,y, lower, upper, shade.col="red", shade.border=NULL, 
                          shade.lty=par("lty"), shade.density=NULL, 
                          shade.angle=45, ...){
    #===========================================================================
    #                                                              SHADE BETWEEN
    #===========================================================================
    # Creates a plot which shades the area under the curve of the region outside 
    # "lower" and "upper" along x. So for example it can be used to shade the 
    # tails of a bell curve. 
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
    
    #-------------------------------------------------------------------------
    #                                                                    Setup
    #-------------------------------------------------------------------------
    
    tolerance = tol=.Machine$double.eps ^ 0.5 # For floating point comparisons
    
    #-------------------------------------------------------------------------
    #                            Create points for the left hand shaded region
    #-------------------------------------------------------------------------
    # Left hand portion looks for values in x less or equal to "lower" 
    x2 = x[x<=lower+tolerance]            
    x2 = c(x2[1], x2, x2[length(x2)])   # Add end points
    
    # Find subset of values in y that correspond to left hand region
    lower_index = find.matches(x2[length(x2)], x, tol=tolerance, maxmatch=1)$matches[[1]]
    y2 = c(0, y[1: lower_index],0)      # Add end points
    
    
    #-------------------------------------------------------------------------
    #                            Create points for the right hand shaded region
    #-------------------------------------------------------------------------
    # Right hand portion looks for values in x greater or equal to "upper" 
    x3 = x[x+tolerance>=upper]
    x3 = c(x3[1], x3, x3[length(x3)])       # Add end points
    
    # Find subset of values in y that correspond to right hand region
    upper_index = find.matches(x3[2], x, tol=tolerance, maxmatch=1)$matches[[1]]
    y3 = c(0, y[upper_index: length(x)],0)  # Add end points
    
        
    #-------------------------------------------------------------------------
    #                                                                  Plot it
    #-------------------------------------------------------------------------
    #preliminary plot to get the axes drawn
    plot(x,y, ...)
    
    # Shaded Region
    polygon(x2,y2,col=shade.col, border=shade.border, lty=shade.lty, 
            density=shade.density, angle=shade.angle)
    polygon(x3,y3,col=shade.col, border=shade.border, lty=shade.lty, 
            density=shade.density, angle=shade.angle)
    
    # Now draw the actual graph to pain over the shaded area border.
    points(x,y, ...)    
}



plot_distribution <- function(dist="normal", mean=NA, sd=NA, n=NA, p=NA, res=100){
    #===========================================================================
    #                                                          PLOT DISTRIBUTION
    #===========================================================================
    # Plots what a particular distribution looks like given parameters such as 
    # its mean and standard deviation. 
    # 
    # ARGS: 
    #   dist    : "normal" "poisson" "binomial" determines the distribution to 
    #             use.
    #   mean    : numeric. The mean of the distribution. If using poisson, this 
    #             is ths lambda value.
    #             DEFAULT = 0 if using normal distribution. 
    #             DEFAULT = 1 if using poisson distribution. 
    #   sd      : Standard deviation. 
    #             DEFAULT = 1
    #   n       : int. Used when dealing with binomial distribution. The number 
    #             of times we run the binomial event, eg flip a coin.
    #             DEFAULT: 1
    #   p       : numeric. probability of success when using binomial 
    #             deistribution
    #             DEFAULT: 0.5
    #   res     : integer. Resolution of the plot (measured as the number of 
    #             data points along the x axis)
    #             Not implemented for Poisson distribution yet.
    #             DEFAULT = 100 if using normal distribution
    #===========================================================================
    #TODO: check the data types of the inputs
    
    #-------------------------------------------------------------------------
    #                                               Handle Normal Distribution
    #-------------------------------------------------------------------------
    if (dist=="normal"){
        if (is.na(mean)){ mean = 0} 
        if (is.na(sd)){ sd = 1 }
        x_min = qnorm(0.0001, mean=mean, sd=sd)
        x_max = qnorm(0.9999, mean=mean, sd=sd)
        x = seq(x_min, x_max, by=(x_max-x_min)/res)
        y = dnorm(x, mean=mean, sd=sd)
        title = sprintf("Normal Distribution with\n mean=%.2f and sd=%.2f", mean, sd)
        plot(x,y, type="l", main=title)
        abline(v=qnorm(0.5, mean=mean, sd=sd), col="red")
    } 
    #-------------------------------------------------------------------------
    #                                              Handle Poisson Distribution
    #-------------------------------------------------------------------------
    else if (dist=="poisson"){
        if (is.na(mean)){ mean = 1} 
        x_min = qpois(0.0001, lambda=mean)
        x_max = qpois(0.9999, lambda=mean)
        x = x_min:x_max
        y = dpois(x, lambda=mean)
        title = sprintf("Poisson Distribution with\n lambda=%d", mean)
        barplot(y, names.arg=x, main=title)
        # TODO: Find an alternative to abline that actually places a vertical 
        #       line in the correct position when using in conjunction with 
        #       barplot.
        #abline(v=qpois(0.5, lambda=mean), col="red", lwd="5")
    }
    #-------------------------------------------------------------------------
    #                                             Handle Binomial Distribution
    #-------------------------------------------------------------------------
    else if (dist=="binomial"){
        #TODO: check the data types of the inputs
        if (is.na(n)){ n = 1}
        if (is.na(p)){ p = 0.5}
        x_min = qbinom(0.0001, size=n, prob=p)
        x_max = qbinom(0.9999, size=n, prob=p)
        x = x_min:x_max
        y = dbinom(x, n,prob=p)
        title = sprintf("Binomial Distribution with\n n=%d and p=%.3f", n, p)
        barplot(y, names.arg=x, main=title)
        # TODO: Find an alternative to abline that actually places a vertical 
        #       line in the correct position when using in conjunction with 
        #       barplot.
        #abline(v=qpois(0.5, lambda=mean), col="red", lwd="5")
    }
}

# Plot Common Distributions
#plot_distribution("normal", mean=100, sd=15)
#plot_distribution("poisson", mean=100)
#plot_distribution("binomial", n=3, p=0.5)
#plot_distribution("binomial", n=500, p=0.01)

# Shades the tails before -5 and after 5 in a bell curve
#x = seq(-9,9,by=0.9)
#y = dnorm(x, mean=0, sd=3)
#shade_outside(x,y,-3, 2, type="o", lwd=2, shade.density=20)
