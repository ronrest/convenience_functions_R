
#===============================================================================
#                                                                      INT_PAIRS
#===============================================================================
#' @title int_pairs
#' @description Plots an interactive scattermatrix.  
#'              You can interactively change the color of rhe points based on 
#'              setting some threshold value and chosing which column to 
#'              use as the one that determines color.  
#' @details  You set the threshold by using a slider from 0 - 100, representing 
#'           percentage of the range of values that exists in the column 
#'           selected. 
#' @param df (data.frame) the dataframe containing the data you want to plot
#'
#' @author Ronny Restrepo
#' @examples
#' int_pairs(iris)
#' 
#' @seealso \code{\link{plot.cols}}
#' @keywords plot interactive pairs manipulate int_pairs
#' @import scales manipulate
#' @export int_pairs
#===============================================================================
int_pairs <- function(df){
    #TODO: make the slider scale based on percentile of the data rather than 
    #      linear range, as it might be better in cases where data is heavily 
    #      skewed. 
    require(manipulate)
    myPlot <- function(thresh, column){
        thresh = scales::rescale(thresh, to = range(df[, column]), from = c(0, 100))  
        color = ifelse((df[,column] >= thresh), "red", "blue")
        pairs(df, panel=panel.smooth, pch=19, col=scales::alpha(color, 0.3))
    }
    manipulate(myPlot(thresh, factor), 
               thresh=slider(0, 100, step=1), 
               factor = picker(as.list(names(df))))    
    
}

