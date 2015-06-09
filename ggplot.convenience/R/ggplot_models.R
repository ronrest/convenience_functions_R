library(ggplot2)

# ==============================================================================
#                                                               GGPLOT_RESIDUALS
# ==============================================================================
#' ggplot_residuals
#' 
#' Create a plot of observed x values vs residuals for some particular model 
#' object. 
#' 
#' You can chose to either display the residuals as points, or as vertical line 
#' segments, or both by setting the arguments "points", and "lines" to TRUE or 
#' FALSE. 
#' 
#' 
#' @param  model (model object) an object created from a modelling function such 
#'          as lm()
#' @param points (logical) Should it show the residuals as points?
#'        
#'      (DEFAULT = TRUE)
#' @param lines (logical) Should it show the residuals as vertical lines? 
#' 
#'      (DEFAULT = FALSE)
#' @param point.col (string) The color for the points
#' 
#'      (DEFAULT = "blue")
#' @param point.alpha (numeric) The alpha value for the points
#' 
#'      (DEFAULT = 0.4)
#' @param point.size (numeric) The size of the points
#' 
#'      (DEFAULT = 6)
#' @param point.border (numeric) size of border of points 
#' 
#'      (DEFAULT = 1)
#' @param line.col (string) the color for the lines
#' 
#'      (DEFAULT = "orange")
#' @param line.alpha (numeric) alpha value for the lines
#' 
#'      (DEFAULT = 0.7)
#' @param line.width (numeric) the width of the lines
#'      
#'      (DEFAULT = 1)
#' @param  ... aditional parameters for the ggplot
#' 
#' @examples 
#' data(iris)
#' model = lm(iris$Sepal.Width ~ iris$Petal.Width)
#' ggplot_residuals(model, lines=TRUE)
#' 
#' ggplot_residuals(model, points=FALSE, lines=TRUE, line.width=7, 
#' line.alpha=0.2, line.col="red")
#' 
#' @author Ronny Restrepo
#' @export
ggplot_residuals <- function(model, points=TRUE, lines=FALSE, point.col="blue", 
                             point.alpha=0.4, point.size=6, point.border=1, 
                             line.col="orange", line.alpha=0.7, line.width=1, 
                             ...){
    # TODO: BUG: occasionally comes up with error 
    #   Error in data.frame(x = c(4.18333804327995, 1.81955364346504, 5.80704394401982,  : 
    #   arguments imply differing number of rows: 100, 48
    # 
    # This occurs with 
    # library(UsingR)
    # data(diamond)
    # diamond.model = lm(diamond$price ~ diamond$carat)
    # ggplot_residuals(diamond.model)
    #
    # But if i name it just model instead of diamond.model it is fine. 
    #
    # TODO: consider using coef(model)
    # TODO: Add option for plotting confidence interval for regression line.
    # TODO: Add option for plotting confidence interval for prediction
    # TODO: Option for plotting based on a single value (to be interpreted as a 
    #       horizontal line), and x values instead of a model object. 
    require("ggplot2")
    
    g = ggplot(model$model, aes(x=model$model[,2], y=resid(model)), ...)
    
    # Draw a solid line across y = 0
    g = g + geom_hline(y=0)      
    
    # Draw the residuals as points
    if (points){
        if (point.border < 0){point.border = 0}
        g = g + geom_point(size=point.size+point.border, colour="black", 
                           alpha=point.alpha)
        g = g + geom_point(size=point.size, colour=point.col, 
                           alpha=point.alpha)
    }    
    
    # Draw the residuals as vertical lines
    if (lines){
        g = g + geom_segment(aes(x=model$model[,2], y=0, xend=model$model[,2], 
                                 yend=resid(model)), 
                             color=line.col, size=line.width, alpha=line.alpha)
    }
    return(g)
}






# ==============================================================================
#                                                                   GGPLOT_MODEL
# ==============================================================================
#' ggplot_model
#' 
#' Create a plot of some particular model object. You can chose to plot the 
#' observed values and the residuals as well. 
#' 
#' Currently only supports linear models, but support will be expanded to any 
#' arbitrary model in the future.  
#' 
#' @param  model (model object) an object created from a modelling function such 
#'          as lm()
#' @param points (logical) Should it show the observed data points?
#'        
#'      (DEFAULT = TRUE)
#' @param residuals (logical) Should it show the residuals as vertical lines? 
#' 
#'      (DEFAULT = FALSE)
#' @param model.color (string) The color of the model line/curve
#' 
#'      (DEFAULT = "red") 
#' @param model.width (numeric) width of the model line/curve 
#' 
#'       (DEFAULT = 1) 
#' @param model.alpha (numeric) alpha of the model line/curve
#' 
#'       (DEFAULT = 1) 
#' @param point.col (string) The color for the points
#' 
#'      (DEFAULT = "blue")
#' @param point.size (int) The size of the points
#' 
#'      (DEFAULT = 4)
#' @param point.border (int) size of border of points 
#' 
#'      (DEFAULT = 1)
#' @param resid.col (string) the color for the residual lines
#' 
#'      (DEFAULT = "red")
#' @param resid.alpha (numeric) alpha for the residual lines 
#'      
#'      (DEFAULT = 0.7)
#' @param resid.width (numeric) width of the residual lines
#' 
#'      (DEFAULT = 1)
#' @param  ... aditional parameters for the ggplot
#' 
#' @examples 
#' data(iris)
#' model = lm(iris$Petal.Length ~ iris$Sepal.Length)
#' ggplot_model(model, points=TRUE, residuals=TRUE, point.col="green")
#' 
#' ggplot_model(model, points=FALSE, residuals=TRUE, point.size=4, resid.width=5, resid.alpha=0.2, resid.col="red", point.border=2)
#' 
#'  
#' @author Ronny Restrepo
#' @export
ggplot_model <- function(model, points=TRUE, residuals=FALSE, 
                         model.color="red", model.width=1, model.alpha=1, 
                         point.col="blue", point.alpha=0.4, point.size=6, 
                         point.border=1, 
                         resid.col="orange", resid.alpha=0.7, resid.width=1, 
                         ...){
    # TODO: Add option for plotting confidence interval for regression line.
    # TODO: Add option for plotting confidence interval for prediction
    require("ggplot2")
    
    g = ggplot(model$model, aes(x=model$model[,2], y=model$model[,1]), ...)
    
    # Draw the scatter points
    if (points){
        if (point.border < 0){point.border = 0}
        g = g + geom_point(size=point.size+point.border, colour="black", 
                           alpha=point.alpha)
        g = g + geom_point(size=point.size, colour=point.col, 
                           alpha=point.alpha)
    }
    
    # Draw the residuals as vertical lines
    if (residuals){
        g = g + geom_segment(aes(x=model$model[,2], y=predict(model), 
                                 xend=model$model[,2], 
                                 yend=model$model[,1]), 
                             color=resid.col, size=resid.width, alpha=resid.alpha)
    }
    
    # Draw the model
    g = g + geom_lm_line(model, color=model.color, size=model.width, alpha=model.alpha)
    
    return(g)
}



# ==============================================================================
#                                                                   GEOM_LM_LINE
# ==============================================================================
#' geom_lm_line
#' 
#' Convenience function for ggplot to play friendly with lm models 
#' 
#' @param model (lm object) the linear model object to use
#' @param x ????
#' @param ... other arguments to be passed on to geom_abline()
#' 
#' @author Shenqi
#' @source https://gist.github.com/shenqi/29b42a6be83ec9e0517d
geom_lm_line <- function(model, x=NULL, ...){
    co <- model$coefficients
    stopifnot(is.numeric(co[[1]]))
    return(geom_abline(intercept=co[[1]], slope=ifelse(is.null(x), co[[2]], co[[x]]), ...))
}

