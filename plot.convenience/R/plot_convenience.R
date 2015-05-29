#library(Hmisc)  # Used for floating point matches
#TODO: Find a better alternative to Hmisc, that doesnt rely on so many other 
#      secondary libraries which slow load time.
#      find.matches() requires Hmisc


shade_after <- function(x, y, boundary, ...){
    # Shades area under the curve from boundary to values of x that are higher.
    # TODO: BUG: Only printing graph with dots, specifying lines not working
    upper <- x[length(x)]
    shade_between(x, y, boundary, upper)
}

shade_before <- function(x, y, boundary, ...){
    # Shades area under the curve from boundary to values of x that are lower.
    # TODO: BUG: Only printing graph with dots, specifying lines not working
    lower <- x[1]
    shade_between(x, y, lower, boundary)
}


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
    lower_index = find.matches(x2[2], x, tol=tolerance, maxmatch=1)$matches[[1]]
    upper_index = find.matches(x2[length(x2)-1], x, tol=tolerance, maxmatch=1)$matches[[1]]
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


#' @import Hmisc
#' @export shade_outside
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


#===============================================================================
#                                                                      PLOT.COLS
#===============================================================================
#' @title plot.cols
#' @description Plots a grid of subplots, where each cell plotted corresponds to 
#'              data from each column of a dataframe. 
#' @details You can provide just a dataframe as an argument, and it will alow you to get 
#'          an idea of how each column of data is distributed. 
#' 
#'          Alternatively, you can also provide a y value, and it will plot y as
#'          a function of each of the columns in turn. 
#' 
#' @note Depending on whether you provide a value for y, there are different 
#'       values for the "type" argument whcih may or may not be legal. 
#' 
#' @param x (dataframe of numerics) dataframe that will act as the x values you 
#'        want to plot
#' @param y (vector of numerics) OPTIONAL. vector of the outcome values.
#' 
#'          - If y=NA (DEFAULT) then only x values are plotted 
#'          
#'          - If y is a vector of values, then it plots y as a function of the 
#'            values of each column in x. 
#'            
#' @param type (string) the type of plot to use for each cell.
#' 
#'      VALID VALUES WHEN y=NA : 
#'      
#'      - "scatter" scatter plot of x values as function of row index
#'      
#'      -"hist" histogram of the values
#'      
#'      -"density"  density plot
#'      
#'      -"boxplot"  Box whisker plot
#'      
#'      VALID VALUES WHEN y IS GIVEN
#'      
#'      - "scatter" scatter plot
#'      
#'      - "lines", "|", "line", "l"  line plot
#'      
#' @param labelCex (numeric) Controls size of the cell labels
#' @param col the color(s) of the points/lines of the plot. You can use whatever 
#'        value you would pass on to plot()
#'        
#'        You can pass an individual value, or a vector of values, eg you can 
#'        specify that the color of the points be determined by the value of 
#'        the output variable in your data. 
#'        
#' @param grad (logical)  Should gradient colours be used?
#'        
#'        TRUE - If you specified a vector of numeric values for col, 
#'               then you can chose to have the color be a gradient instead of 
#'               discrete colors. 
#'        FALSE - (DEFAULT) values in 'col' will be interpreted as discrete 
#'               color  changes.  
#' @param grad.theme (string) Controls the gradient color
#' 
#'        "flame" = from yellow to red
#'        
#'        "blue" = from light blue to dark blue
#'        
#'        "rainbow" = From blue,cyan, green, yellow, orange, red
#'        
#'         anything else = from light gray to black.
#'          
#' @param grad.scal (string) EXPERIMENTAL - controls how the gradient is 
#'         inerpolated. 
#'         
#'         "normal" normally distributed
#'         
#'         "range"  scales it linearly from minimum to maximum value
#'         
#'         Please note that this argument is experimental and may be removed 
#'         at any point. 
#'         
#' @param ... aditional arguments to be passed on to the cell plots
#'  
#'          - See ?plot(), ?boxplot(), ?hist() to see what aditional arguments 
#'            you can pass on to them. 
#' 
#' @examples 
#' # load some built in data
#' data(mtcars)
#' data(iris)
#' 
#' # Scatterplots of each column, with colors separated by Species Column 
#' plot.cols(iris, col=iris$Species, pch=19)
#' 
#' # Scatterplots of each column, with color gradient based on mpg Column 
#' plot.cols(mtcars, col=mtcars$mpg, grad=T, pch=19)
#' 
#' # Density plot of each column of variables
#' plot.cols(mtcars, type="density")
#' 
#' # Boxplot of each column of variables
#' plot.cols(iris, type="boxplot")
#' 
#' # Histogram each variable
#' plot.cols(iris, type="hist")
#' 
#' # output variable as a function of each predicto variable
#' plot.cols(mtcars[,-1], mtcars[,1])
#' plot.cols(iris[,-length(iris)], iris[,length(iris)], col=iris$Species)
#' 
#' @author Ronny Restrepo
#' @export plot.cols
#===============================================================================
plot.cols <-function(x, y=NA, type="scatter", labelCex=1, col="darkgray", 
                     grad=FALSE, grad.theme="flame", grad.scal="normal", 
                     ...){
    # TODO:  Have the ability to specify values for gradient color. At the 
    #        monent we can use col=y, but color changes are discrete and all 
    #        over the place, which is ok for categorical output values, but for 
    #        continuous output variables we would want gradients. 
    #--------------------------------------------------------------------------
    #                                             Set up Grid and Cell Settings
    #--------------------------------------------------------------------------
    # calculate number of cells needed
    if (is.null(ncol(x))){    # Hack to handle input of a single column/vector
        n = 1
        x = data.frame(x)
    } else {
        n = ncol(x)    
    }
    
    # Calculate number of cells per row/column to plot
    if (n == 1){
        ngridRows = 1
        ngridCols = 1
    } else if (n == 2){
        ngridRows = 2
        ngridCols = 1
    } else {
        ngridCols = ceiling(sqrt(n))
        ngridRows = ceiling(n/ngridCols)
    }
    
    # Take a snapshot of the current global plotting settings before modifying 
    # the settings to get a nice grid layout
    BU.par = par(c("mfrow", "mar", "mgp", "tck", "oma"))
    par(mfrow=c(ngridRows,ngridCols), mar=c(1, 1, 2, 0.5), 
        mgp = c(1.5, 0.3, 0), tck = -0.01, oma=c(1, 1, 0.5, 0.5))
    
    
    #--------------------------------------------------------------------------
    #                                            Set up Gradient Color Settings
    #--------------------------------------------------------------------------
    if (grad){
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
        
        # Set the way the gradient is interpolated
        # Linear scaling from min to max
        if (grad.scal=="range"){
            i = 2
            y = x[,1]
            col = x[,1]
            range1 = range(col)
            span1 = range1[2] - range1[1]
            min1 = min(col)
            rmult = 10/span1
            newscale = (col - min1) * rmult 
        
        # Normal Scaling
        } else if (grad.scal == "normal"){
            newscale = 2 * (col - mean(col)) / sd(col) + 5    
        
        # No scaling 
        } else {
            newscale = col
        }
        col = gradientTheme(10)[newscale]
    }
    
    
    #``````````````````````````````````````````````````````````````````````````
    #                                               PLOTS OF ONLY COLUMN VALUES
    #``````````````````````````````````````````````````````````````````````````
    #--------------------------------------------------------------------------
    #                                                  Scatter Plot of X values
    #--------------------------------------------------------------------------
    if (is.na(y[1]) & type=="scatter"){
        sapply(seq_along(x), function(i) {
            plot(x[,i], main="", col=col, ...)
            mtext(colnames(x)[i], side=3, line=0.5, cex = labelCex)  
        })    
    #--------------------------------------------------------------------------
    #                                                     Histogram of X values
    #--------------------------------------------------------------------------
    } else if (is.na(y[1]) & type=="hist"){
        sapply(seq_along(x), function(i) {
            hist(as.numeric(x[,i]), main="", col=col, ...)
            mtext(colnames(x)[i], side=3, line=0.5, cex = labelCex)  
        })
    #--------------------------------------------------------------------------
    #                                                  Density Plot of X values
    #--------------------------------------------------------------------------
    } else if (is.na(y[1]) & type=="density"){
        sapply(seq_along(x), function(i) {
            plot(density(as.numeric(x[,i])), main="", col=col, ...)
            mtext(colnames(x)[i], side=3, line=0.5, cex = labelCex)  
        })
    #--------------------------------------------------------------------------
    #                                              Box-Whisker Plot of X values
    #--------------------------------------------------------------------------
    } else if (is.na(y[1]) & type=="boxplot"){
        sapply(seq_along(x), function(i) {
            boxplot(as.numeric(x[,i]), horizontal=T, main="", col=col, ...)
            mtext(colnames(x)[i], side=3, line=0.5, cex = labelCex)  
        })
        
    #``````````````````````````````````````````````````````````````````````````
    #                                   PLOTS OF Y AS FUNCTION OF COLUMN VALUES
    #``````````````````````````````````````````````````````````````````````````
    #--------------------------------------------------------------------------
    #                                                     Scatter Plot of Y ~ X
    #--------------------------------------------------------------------------
    } else if (!is.na(y[1]) & type=="scatter"){
        sapply(seq_along(x), function(i) {
            plot(x[,i], y, main="", col=col, ...)
            mtext(paste("y ~", colnames(x)[i]), side=3, line=0.5, cex=labelCex)  
        })
    #--------------------------------------------------------------------------
    #                                                        Line Plot of Y ~ X
    #--------------------------------------------------------------------------
    } else if (!is.na(y[1]) & (type=="line" | type=="|" | type=="lines" | 
                               type=="l")){
        sapply(seq_along(x), function(i) {
            plot(x[,i], y, type="l", main="", col=col, ...)
            mtext(paste("y ~", colnames(x)[i]), side=3, line=0.5, cex=labelCex)  
        })
    
    #``````````````````````````````````````````````````````````````````````````
    #                                                        ILEGAL COMBINATION
    #``````````````````````````````````````````````````````````````````````````
    } else {
        warning("I'm affraid I can't let you do that!\n",
                "  You have asked for a combination of 'y' and 'type' that is ",
                "not allowed. \n",
                "  Type ?plot.cols to see what values are allowed.")
        
    }
    # Return the global plot parameters to their previous state
    par(BU.par)
}



