# TODO: implement plotting of residuals like in ggplot_residuals()


# =====================================
# RESET PAR TO FACTORY DEFAULTS
# =====================================
# resetPar <- function() {
#     dev.new()
#     op <- par(no.readonly = TRUE)
#     dev.off()
#     op
# }
# =====================================


#===============================================================================
#                                                                    PLOT.MODELS
#===============================================================================
#' @title plot_models
#' @description Plots multiple models fitted to the same data. By default it 
#'          overlaps each model over the others. But you can specify to have 
#'          each in a separate cell in a grid plot. 
#' @details By default the plot of each model will be the same color. But you 
#'          can specify the colour of each individualy. 
#'          
#'          Alternatively, a feature that will be added is to have all plots 
#'          the same color except for a master model (needs to be placed as the 
#'          first model in the list of models) 
#'          , or to have a master and a set of slaves (where
#'          one is brightly colored, and the others are grey)
#'          
#' @param models a list of model objects
#' @param scatter (logical) 
#' 
#'      TRUE = plots scatterplot of the datapoints used in the training set. 
#'      
#'      FALSE =  only plots the models without any of the original data points.
#' @param scatterColor the color for the scatter plot of the original data. 
#' 
#'      See here for a chart of possible colors: 
#'      \url{http://research.stowers-institute.org/efg/R/Color/Chart/}
#' @param modelColor either a single color for all models to be. Or a vector 
#'      of colors to cycle through. 
#' @param modelWidth (numeric) width of the model lines. You can wither 
#'      pass a single value in which case all models will be the same width, 
#'      or you can specify a vector of numerics to  specify the width of each 
#'      individual model. 
#' 
#' @author Ronny Restrepo
#' @examples
#' library(ElemStatLearn)
#' data(ozone,package="ElemStatLearn")
#' 
#' # create models
#' model1 = loess(temperature ~ ozone, data=ozone,span=0.15)
#' model2 = lm(temperature ~ ozone, data=ozone)
#' model3 = loess(temperature ~ ozone, data=ozone, span=0.2)
#' model4 = loess(temperature ~ ozone, data=ozone, span=1)
#' model5 = loess(temperature ~ ozone, data=ozone, span=20)
#' models = list(model1, model2, model3, model4, model5)
#'
#' plot_models(models)
#' 
#' # Use different colors for each model
#' modcols = c("darkorchid2","chartreuse","brown2","aquamarine2","darkorange") 
#'
#' # Use different line widths for each model
#' plot_models(models, modelColor=modcols, modelWidth=c(8,2,4,8,3))
#' 
#' @seealso \code{\link{plot.cols}}
#' @keywords plot, plot_models, plotting, modelling, models, model
#' @export plot_models 
#===============================================================================
plot_models <- function(models, scatter=TRUE, scatterColor="lightgray", 
                        modelColor="darkorange", modelWidth=2){
    #TODO: check that the models is a list, if not, then proceed with just the 
    #      one model (perhaps place it inside a list to make it compatible 
    #      with the rest of the code)
    # TODO: FIx axis labels to give useful labels. Maybe add a main title too. 
    # TODO: find if there is a way of setting alpha transperancy for points like
    #       there is in ggplot2
    
    # Make sure there are enough color and width elements for each model
    modelColor = rep_len(modelColor, length(models))
    modelWidth = rep_len(modelWidth, length(models))
    
    # Get x and y points of original data
    xy = model.frame(models[[1]])
    
    # Prepare the Cavas, and plot scatter plot if requested
    scatterType = ifelse(scatter, "p", "n")
    plot(xy[,2],xy[,1], col=scatterColor, pch=19, type=scatterType)
    
    # Prepare x axis values for Model
    model.x = data.frame(seq(min(xy[,2]), max(xy[,2]), length.out=100))
    names(model.x) = names(xy)[2]
    
    # Plot the models
    for (i in 1:length(models)){
        model.y = predict(models[[i]], newdata=model.x)
        lines(as.vector(model.x[,1]), 
              as.vector(model.y), 
              type="l", 
              col=modelColor[i], 
              lwd=modelWidth[i])
    }
}

