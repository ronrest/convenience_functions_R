# TODO: Add feature to allow a list of models to be input, and you compare the 
#       residual plots of mulitple models, each being laid out as a separate cell 
#       in a grid of plots, one for each model. if only one model is entered
#       then it just plots one plot as normal. 
#
# TODO: Allow option to input of x and y values instead of a model, and in this 
#       case, use the mean as the predictor. 

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
#' @param ctm (logical) Compare To the Mean? Compare the residuals left over 
#'      from the model compared to the residuals that would exist if the mean 
#'      of the putcome variable was used as the estimator. 
#'      
#'      (Default is FALSE)
#' @param point_col Color of the points used for residuals from the model. 
#'      
#'      See here for a chart of possible colors: 
#'      \url{http://research.stowers-institute.org/efg/R/Color/Chart/}
#'      
#'      (Default is "blue")
#' @param point_alpha (numeric) Alpha of points used for residuals from model.
#' 
#'      (Default is 0.3)
#' @param point_size (numeric) Size of points used for residuals from the model.
#' 
#'      (Default is 2)
#' @param line_col Color of the lines used for residuals from the model.
#' 
#'      (Default is same as point_col)
#' @param line_alpha (numeric) Alpha of lines used for residuals from the model. 
#' 
#'      (Default is 0.3)
#' @param line_width (numeric) Width of lines used for residuals from the model.
#' 
#'      (Default is a value that gives same visual thickness as point_size)
#' @param ctm_point_col Color of the points used for residuals from the mean.
#' 
#'      (Default is "coral")  
#' @param ctm_point_alpha Alpha of the points used for residuals from the mean.
#' 
#'      (Default is the same as point_alpha)  
#' @param ctm_point_size (numeric) Size of the points used for residuals from 
#'      the mean.
#' 
#'      (Default is 2)  
#' @param ctm_line_col Color of the lines used for residuals from the mean. 
#' 
#'      (Default is same value as ctm_point_col)  
#' @param ctm_line_alpha Alpha of the lines used for residuals from the mean. 
#' 
#'      (Default is same value as line_alpha)  
#' @param ctm_line_width Width of the lines used for residuals from the mean.
#' 
#'      (Default is a value that gives same visual thickness as ctm_point_size)   
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
#' # Plot the residuals as lines only
#' plot_residuals(model2, points=F, lines=T, line_width=10)
#' 
#' # Plot the residuals as points and lines
#' plot_residuals(model, lines=T)
#' 
#' # Plot the residuals of the model, compared to the residuals when the mean 
#' # is used as the predictor. 
#' plot_residuals(model, ctm=T, lines=T, points=T, point_size=1)
#'
#' @seealso \code{\link{plot_models}} \code{\link{plot.cols}} \code{\link{plot}}  
#' @keywords residuals plot plot_residuals plot.convenience
#' @import scales
#' @export plot_residuals
#===============================================================================
plot_residuals <- function(model,  
                           points=TRUE, lines=FALSE, ctm=FALSE,  
                           point_col="blue", point_alpha=0.3, point_size=2, 
                           line_col= point_col, line_alpha=0.3, 
                           line_width=10*point_size, 
                           ctm_point_col = "coral", 
                           ctm_point_alpha = point_alpha, 
                           ctm_point_size = 2,  
                           ctm_line_col = ctm_point_col, 
                           ctm_line_alpha = line_alpha, 
                           ctm_line_width = 10*ctm_point_size,  
                           draw_grid = TRUE, ...){
    
    xy = model.frame(model)     # Get x and y points of original data
    resids = resid(model)       # Calculate the residual values of the model
    
    #---------------------------------------------------------------------------
    #                                                          Prepare the Cavas
    #---------------------------------------------------------------------------
    # Plot canvas, using residuals from the mean line 
    if (ctm){
        mean_resids = xy[,1] - mean(xy[,1])     # Residuals from the mean
        plot(xy[,2], mean_resids, type="n", ...)
        
    } 
    # Plot canvas, using residuals from the model 
    else {
        plot(xy[,2], resids, type="n", ...)    
    }
    
    # Add a line at zero
    abline(h=0, lwd=2)    
    
    #---------------------------------------------------------------------------
    #                                      Draw grid lines if they are requested
    #---------------------------------------------------------------------------
    if (draw_grid){
        grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
             lwd = 1, equilogs = TRUE)
    }
    #---------------------------------------------------------------------------
    #                                           plot the residual lines for Mean
    #---------------------------------------------------------------------------
    if (ctm & lines){
        segments(x0 = xy[,2], y0 = rep(0, length(xy[,2])), 
                 x1 = xy[,2], y1 = mean_resids, 
                 col=scales::alpha(ctm_line_col, ctm_line_alpha), 
                 lwd=ctm_line_width)
    }
    #---------------------------------------------------------------------------
    #                                          plot the residual lines for Model
    #---------------------------------------------------------------------------
    if (lines){
        segments(x0 = xy[,2], y0 = rep(0, length(xy[,2])), 
                 x1 = xy[,2], y1 = resids, 
                 col=scales::alpha(line_col, line_alpha), 
                 lwd=line_width)
    }
    #---------------------------------------------------------------------------
    #                                   Add residual points of Mean if requested
    #---------------------------------------------------------------------------
    if (points & ctm){
        points(xy[,2], 
           mean_resids, col=scales::alpha(ctm_point_col, ctm_point_alpha), 
           pch=19, cex=ctm_point_size)
    }
    #---------------------------------------------------------------------------
    #                                  Add residual points of Model if requested
    #---------------------------------------------------------------------------
    if (points){
        points(xy[,2],resids, col=scales::alpha(point_col, point_alpha), pch=19, 
               cex=point_size)
    }
    
}

