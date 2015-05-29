
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
    #
    # TODO: COnsider splitting this function up, having some sections executed 
    #       by helper functions. Eg, the theme, and grid setups. 
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
