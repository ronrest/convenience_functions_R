library(ggplot2)

# ==============================================================================
#                                                                GGRESIDUAL_PLOT
# ==============================================================================
#' ggresidual_plot
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
