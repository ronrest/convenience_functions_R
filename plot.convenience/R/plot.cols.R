################################################################################
# TODO: Add option 'by' to plot by groups. Eg, if you specify 'boxplot' then 
#       you can draw each boxplot within a cell to be representing a subset of 
#       the data specified by some category column.
# TODO: Add option to fix the axis scales, so that all cells display same range 
#       of x and y values, thus making it easier to compare all cells with each 
#       other. (currently uses auto centering of axis scales)
# TODO: add option to plot the scatter dots with the size being determined by 
#       the frequency of the values in order to relay information that some 
#       values carry more weight than others. 
# TODO: Allow y to be a string, which specifies a column name in x. 
# TODO: BUG: using grad.scal="range" gives wrong color for upper values, it
#            flips back to color that is used for low values. 
#
# TODO: Im not entirely confident that grad.scal="range" will give accurate 
#       results for highly skewed data. I suspect it might give wrong color
#       for very high values more than 4 standard deviations above the mean.
#       Test this. 
# TODO: Im not sure if the colors used when grad=T is used on factor type 
#       columns are actually meaningful. Try find a way to check this, so people
#       dont mistakenly interpret some relationship where there isnt any. 
#
################################################################################

#===============================================================================
#                                                                      PLOT.COLS
#===============================================================================
#' @title plot.cols
#' @description Plots a grid of subplots, where each cell plotted corresponds to 
#'              data from each column of a dataframe. 
#' @details You can provide just a dataframe as an argument, and it will alow 
#'          you to get an idea of how each column of data is distributed. 
#' 
#'          Alternatively, you can also provide a y value, and it will plot y as
#'          a function of each of the columns in turn. 
#' 
#' @note Depending on whether you provide a value for \code{y}, there are 
#'       different values for the \code{type} argument whcih may or may not be 
#'       legal. 
#' 
#' @param x (dataframe of numerics) dataframe that will act as the x values you 
#'        want to plot
#' @param y (vector of numerics) OPTIONAL. vector of the outcome values.
#' 
#'          - If \code{y=NA} (DEFAULT) then only \code{x} values are plotted 
#'          
#'          - If \code{y} is a vector of values, then it plots y as a function of the 
#'            values of each column in \code{x}. 
#'            
#' @param type (string) the type of plot to use for each cell.
#' 
#'      VALID VALUES WHEN \code{y=NA} : 
#'      
#'      - \code{"auto"} (DEFAULT) automatically determines a good type for the 
#'        data 
#'      
#'      - \code{"scatter"} scatter plot of x values as function of row index
#'      
#'      -\code{"hist"} histogram of the values
#'      
#'      -\code{"density"}  density plot
#'      
#'      -\code{"boxplot"}  Box whisker plot
#'      
#'      
#'      VALID VALUES WHEN \code{y} IS GIVEN
#'      
#'      - \code{"scatter"} scatter plot
#'      
#'      - \code{"lines"}, \code{"|"}, \code{"line"}, \code{"l"}  line plot
#'      
#' @param labelCex (numeric) Controls size of the cell labels
#' @param col the color(s) of the points/lines of the plot. You can use whatever 
#'        value you would pass on to \code{plot()}
#'        
#'        You can pass an individual value, or a vector of values, eg you can 
#'        specify that the color of the points be determined by the value of 
#'        the output variable in your data. 
#'        
#' @param grad (logical)  Should gradient colours be used?
#'        
#'        \code{TRUE} - If you specified a vector of numeric values for col, 
#'               then you can chose to have the color be a gradient instead of 
#'               discrete colors. 
#'        \code{FALSE} - (DEFAULT) values in 'col' will be interpreted as 
#'               discrete color changes.  
#' @param grad.theme (string) Controls the gradient color
#' 
#'        \code{"flame"} = from yellow to red
#'        
#'        \code{"blue"} = from light blue to dark blue
#'        
#'        \code{"rainbow"} = From blue,cyan, green, yellow, orange, red
#'        
#'         anything else = from light gray to black.
#'          
#' @param grad.scal (string) EXPERIMENTAL - controls how the gradient is 
#'         inerpolated. 
#'         
#'         \code{"normal"} normally distributed
#'         
#'         \code{"range"}  scales it linearly from minimum to maximum value. 
#'         NOTE that this option gives buggy results. Its not fit for production 
#'         use yet.  
#'         
#'         Please note that this argument is experimental and may be removed 
#'         at any point. 
#'         
#' @param ... aditional arguments to be passed on to the cell plots
#'  
#'          - See \code{?plot()}, \code{?boxplot()}, \code{?hist()} to see what 
#'            aditional arguments you can pass on to them. 
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
#' # output variable as a function of each predicton variable
#' plot.cols(mtcars[,-1], mtcars[,1])
#' plot.cols(iris[,-length(iris)], iris[,length(iris)], col=iris$Species)
#' 
#' @seealso \code{\link{plot}}, \code{\link{boxplot}}, \code{\link{hist}}
#' @author Ronny Restrepo
#' @keywords plot, plot.cols, plotting, column, columns
#' @export plot.cols
#===============================================================================
plot.cols <-function(x, y=NA, type="auto", labelCex=1, col="darkgray", 
                     grad=FALSE, grad.theme="flame", grad.scal="normal", 
                     ...){
    #--------------------------------------------------------------------------
    #                                             Set up Grid and Cell Settings
    #--------------------------------------------------------------------------
    # Take a snapshot of the current global plotting settings
    BU.par = par(c("mfrow", "mar", "mgp", "tck", "oma"))
    
    # Hack to handle input of a single column/vector
    ncols = ncol(x)
    if (is.null(ncols)){      
        ncols = 1
        x = data.frame(x)
    }
    # Set New global plotting settings for a grid layout
    set_par_for_n_subplots(ncols)
    
    #--------------------------------------------------------------------------
    #                                            Set up Gradient Color Settings
    #--------------------------------------------------------------------------
    if (grad & is.factor(col)){col = as.numeric(col)}
    if (grad & length(col) != nrow(x)){
        warning("When using gradient, 'col' should be same lenght as the ",
                "number of rows in x. Plotting witout any color.")
        col = "darkgray"
    } else if (grad){
        gradientTheme = .create.theme(grad.theme, grad.scal)
        rescaledVals = .gradient.interpolation(col, scale.mode="normal")
        col = gradientTheme(10)[rescaledVals]
    } 
    
    #**************************************************************************
    #                                               PLOTS OF ONLY COLUMN VALUES
    #**************************************************************************
    #--------------------------------------------------------------------------
    #                                                  Scatter Plot of X values
    #--------------------------------------------------------------------------
    if (is.na(y[1]) & (type=="scatter" | type=="auto")){
        sapply(seq_along(x), function(i) {
            plot(x[,i], main="", col=col, ...)
            mtext(colnames(x)[i], side=3, line=0.5, cex = labelCex)  
        })    
        #----------------------------------------------------------------------
        #                                                 Histogram of X values
        #----------------------------------------------------------------------
    } else if (is.na(y[1]) & type=="hist"){
        sapply(seq_along(x), function(i) {
            hist(as.numeric(x[,i]), main="", col=col, ...)
            mtext(colnames(x)[i], side=3, line=0.5, cex = labelCex)  
        })
        #----------------------------------------------------------------------
        #                                              Density Plot of X values
        #----------------------------------------------------------------------
    } else if (is.na(y[1]) & type=="density"){
        sapply(seq_along(x), function(i) {
            plot(density(as.numeric(x[,i])), main="", col=col, ...)
            mtext(colnames(x)[i], side=3, line=0.5, cex = labelCex)  
        })
        #----------------------------------------------------------------------
        #                                          Box-Whisker Plot of X values
        #----------------------------------------------------------------------
    } else if (is.na(y[1]) & type=="boxplot"){
        sapply(seq_along(x), function(i) {
            boxplot(as.numeric(x[,i]), horizontal=T, main="", col=col, ...)
            mtext(colnames(x)[i], side=3, line=0.5, cex = labelCex)  
        })
        
        #**********************************************************************
        #                               PLOTS OF Y AS FUNCTION OF COLUMN VALUES
        #**********************************************************************
        #----------------------------------------------------------------------
        #                                                 Scatter Plot of Y ~ X
        #----------------------------------------------------------------------
    } else if (!is.na(y[1]) & (type=="scatter" | type=="auto")){
        sapply(seq_along(x), function(i) {
            plot(x[,i], y, main="", col=col, ...)
            mtext(paste("y ~", colnames(x)[i]), side=3, line=0.5, cex=labelCex)  
        })
        #----------------------------------------------------------------------
        #                                                    Line Plot of Y ~ X
        #----------------------------------------------------------------------
    } else if (!is.na(y[1]) & (type=="line" | type=="|" | type=="lines" | 
                                   type=="l")){
        sapply(seq_along(x), function(i) {
            plot(x[,i], y, type="l", main="", col=col, ...)
            mtext(paste("y ~", colnames(x)[i]), side=3, line=0.5, cex=labelCex)  
        })
        
        #**********************************************************************
        #                                                    ILEGAL COMBINATION
        #**********************************************************************
    } else {
        warning("I'm affraid I can't let you do that!\n",
                "  You have asked for a combination of 'y' and 'type' that is ",
                "not allowed. \n",
                "  Type ?plot.cols to see what values are allowed.")
        
    }
    # Return the global plot parameters to their previous settings
    par(BU.par)
}
