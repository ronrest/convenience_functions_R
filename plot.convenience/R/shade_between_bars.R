shade_between_bars <- function(x,y, lower, upper, shade.col="red", shade.border=NULL, 
                               shade.density=NULL, shade.angle=45, main="", ...){
    #===========================================================================
    #                                                         SHADE BETWEEN BARS
    #===========================================================================
    # Creates a barplot which shades the bars between certain lower and upper 
    # x values (inclusive).
    # 
    # ARGS: 
    #   x     : (vec) the x value labels
    #   y     : (vec) the height of the bars  
    #   lower : (numeric) the x value that is the lower end of the shaded region
    #   upper : (numeric) the x value that is the upper end of the shaded region
    #
    # OPTIONAL ARGS: 
    #   shade.col     : color to fill in the shaded area.
    #   shade.border : the color to draw the border. The default, NULL, means to 
    #                  use par("fg"). Use border = NA to omit borders.
    #                  
    #                  For compatibility with S, border can also be logical, in 
    #                  which case FALSE is equivalent to NA (borders omitted) 
    #                  and TRUE is equivalent to NULL (use foreground colour)
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
    # EXAMPLES:
    #   # Shades between 12 and 17 in a binomial distribution 
    #   x = 1:20
    #   y = dbinom(x, size=20, p=0.7)
    #   shade_between_bars(x, y, 12, 17)
    #
    #===========================================================================
    
    # Main bars
    #title = sprintf("Binomial Distribution with\n n=%d and p=%.3f", n, p)
    title = main
    barplot(y, names.arg=x, main=title, ...)
    
    # Shaded Region bars 
    shade_index_filter = which(x>=lower & x<=upper)
    y2 = vector("numeric", length(x))
    y2[shade_index_filter] = y[shade_index_filter]
    barplot(y2, names.arg=x, add=TRUE, col=shade.col, border=shade.border, 
            density=shade.density, angle=shade.angle)
    
}
