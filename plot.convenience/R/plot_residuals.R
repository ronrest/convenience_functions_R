# TODO: Add feature to allow a list of models to be input, and you compare the 
#       residual plots of mulitple models, each being laid out as a separate cell 
#       in a grid of plots, one for each model. if only one model is entered
#       then it just plots one plot as normal. 

#===============================================================================
#                                                                 PLOT_RESIDUALS
#===============================================================================
#' @title plot_residuals
#' @description Create a plot of observed x values vs residuals for some 
#'              particular model object. 
#' @details You can chose to either display the residuals as points, or as 
#'          vertical line segments, or both by setting the arguments "points", 
#'          and "lines" to TRUE or FALSE. 
#' @note Make sure that when you are creating models, that you avoid using the 
#'       "$" operator. 
#'       
#'       BAD:  lm(myData$y ~ myData$x)
#'       
#'       GOOD: lm(y ~ x, data=myData)
#'       
#'       Otherwise you will get error messages like  "'x' and 'y' lengths differ" 
#'       when you try running the predict() function which is used by 
#'       plot_models()
#'       
#' @param model (model object) the model you want to draw the residuals for 
#' @param points (logical) Draw the residuals as points? 
#' 
#'      (Default is TRUE)
#' @param lines (logical) Draw the residuals as lines? 
#' 
#'      (Default is FALSE)
#' @param point_col the color of the points
#'  
#'      (Default is "Blue")
#' @param point_alpha (numeric) the alpha of the points. 
#' 
#'      (Default is 0.3)
#' @param point_size (numeric) The size of the points. 
#' 
#'      (Default is 2)
#' @param line_col The color od the lines. 
#' 
#'      (Default is "orange")
#' @param line_alpha (numeric) the alpha of the lines. 
#' 
#'      (Default is 0.3)
#' @param line_width (numeric) The width of the lines. 
#' 
#'      (Default is 5)
#' @param draw_grid (logical) Should a grid be drawn? 
#' 
#'      (Default is TRUE)
#' @param ... other arguments to be passed on to the parent plot() function.
#' 
#'          use ?plot() to see what other arguments can be passed on.
#'
#' @author Ronny Restrepo
#' @examples
#' library(ElemStatLearn)
#' data(ozone,package="ElemStatLearn")
#' 
#' # create model
#' model = loess(temperature ~ ozone, data=ozone,span=0.15)
#'
#' # Plot the residuals as points
#' plot_residuals(model)
#' 
#' # Plot the residuals as lines
#' plot_residuals(model2, points=F, lines=T, line_width=10)
#' 
#' # Plot the residuals as lines and Points
#' plot_residuals(model3, points=T, lines=T)
#'
#' @seealso \code{\link{plot_models}} \code{\link{plot.cols}} \code{\link{plot}}  
#' @keywords residuals plot plot_residuals plot.convenience
#' @import scales
#' @export plot_residuals
#===============================================================================
plot_residuals <- function(model, points=TRUE, lines=FALSE, point_col="blue", 
                           point_alpha=0.3, point_size=2, line_col="orange", 
                           line_alpha=0.3, line_width=5, draw_grid=TRUE, ...){
    
    #---------------------------------------------------------------------------
    #                            Prepare the Cavas, and plot points if requested
    #---------------------------------------------------------------------------
    xy = model.frame(model)     # Get x and y points of original data
    resids = resid(model)       # Calculate the residual values 
    
    pointType = ifelse(points, "p", "n")
    plot(xy[,2],resids, col=scales::alpha(point_col, point_alpha), pch=19, 
         cex=point_size, type=pointType, ...)
    abline(h=0, lwd=2)    
    
    #---------------------------------------------------------------------------
    #                                      Draw grid lines if they are requested
    #---------------------------------------------------------------------------
    if (draw_grid){
        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
             lwd = 1, equilogs = TRUE)
    }
    
    #---------------------------------------------------------------------------
    #                                                    plot the residual lines
    #---------------------------------------------------------------------------
    if (lines){
        segments(x0 = xy[,2], y0 = rep(0, length(xy[,2])), 
                 x1 = xy[,2], y1 = resids, 
                 col=scales::alpha(line_col, line_alpha), 
                 lwd=line_width)
    }
}

