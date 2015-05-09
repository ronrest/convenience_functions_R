#' lm2
#' 
#' Creates a list of information needed for creating a linear model, along with 
#' other information that is useful in evaluating how good that model is.  
#' 
#' @param x (vector of numerics) The independent variables 
#' @param y (vector of numerics) The dependent variables
#' @param print.summary (boolean) Should it print a summary? 
#'          (DEFAULT = TRUE) 
#' @return a list of values:
#'      $n
#'      $mean.x
#'      $mean.y
#'      $cor
#'      $rot_significance
#'      $rot_is_significant
#'      $slope
#'      $intercept
#'      $SSE_using_mean
#'      $SSE_using_model
#'      $MSE_using_mean
#'      $MSE_using_model
#' @examples
#' lm2(x, y)
#' lm2(x, y, print.summary=FALSE)
#' 
#' # Plot a scatterplot and the linear model
#' model = lm2(x,y)
#' plot(x, y)
#' abline(model$intercept, model$slope)
#' 
#' @author Ronny Restrepo
#' @export
lm2 <- function(x, y, print.summary=TRUE){
    # TODO: make this actually return a class, with methods, instead of a list 
    #       One of the methods should be to print the summary.
    # TODO: Include the option to only print out selected items from the summary
    # TODO: Include option to return the string of the summary instead of 
    #       printing directly.
    # TODO: include option to return an HTML formatted version of summary.
    info = list()      # Store all the information in a list
    colspan = 45       # Used to line up printed values (fill amount in printkv)
    
    # Number of items information
    info$n = length(x)
    
    # Means of each variable
    mean.x = mean(x)
    mean.y = mean(y)
    info$mean.x = mean.x
    info$mean.y = mean.y
    
    # Correlation and Covariance Information
    info$cor = cor(x, y)
    
    # Info regarding significance of correlation using rule of thumb
    info$rot_significance = 2/sqrt(info$n)
    info$rot_is_significant = abs(info$cor) > info$rot_significance
    
    # Slope and intercept information
    info$slope = sum((x - mean.x)*(y - mean.y)) / sum((x - mean.x)^2)
    info$intercept = mean.y - (info$slope * mean.x)
    
    # Sum of Squares information 
    info$SSE_using_mean = sum((y - mean.y)^2)
    info$SSE_using_model = sum(((y - mean.y) - (x - mean.x)*info$slope)^2)
    
    # Mean Squared Error information 
    info$MSE_using_mean = info$SSE_using_mean / info$n
    info$MSE_using_model = info$SSE_using_model / info$n
    
    if (print.summary){
        print("===============================================================")
        print("                          MODEL SUMMARY                        ")
        print("===============================================================")
        print("Is assuming there are no NAs in the data fed in, and that both ")
        print("dependent and independent data is same length                  ")
        print("_______________________________________________________________")
        printkv("Sample Size", info$n, fill=colspan, fill_char=".")
        printkv("Mean of independent variable", info$mean.x, 
                fill=colspan, fill_char=".")
        printkv("Mean of dependent variable", info$mean.y, 
                fill=colspan, fill_char=".")
        printkv("Correlation (Pearson, using 'everything')", info$cor, 
                fill=colspan, fill_char=".")
        printkv("Rule of Thumb Significance for correlation", 
                info$rot_significance, fill=colspan, fill_char=".")
        printkv("Is this Significant? ", info$rot_is_significant, 
                fill=colspan, fill_char=".")
        printkv("Slope", info$slope, fill=colspan, fill_char=".")
        printkv("Intercept", info$intercept, fill=colspan, fill_char=".")
        printkv("Sum of Squared Errors (using mean)", info$SSE_using_mean, 
                fill=colspan, fill_char=".")
        printkv("Sum of Squared Errors (using model)", info$SSE_using_model, 
                fill=colspan, fill_char=".")
        printkv("Mean Squared Errors (using mean)", info$MSE_using_mean, 
                fill=colspan, fill_char=".")
        printkv("Mean Squared Errors (using model)", info$MSE_using_model, 
                fill=colspan, fill_char=".")
        print("_______________________________________________________________")
    }
    invisible(info)
}





