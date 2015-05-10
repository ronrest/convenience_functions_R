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
#' 
#'      $n
#'      
#'      $mean.x
#'      
#'      $mean.y
#'      
#'      $sd.x
#'      
#'      $sd.y
#'      
#'      $cor
#'      
#'      $rot_significance
#'      
#'      $rot_is_significant
#'      
#'      $slope
#'      
#'      $intercept
#'      
#'      $SST                # Total Sum of Squared Errors (using mean of y)
#'      
#'      $SSE                # Sum of Squared Errors in the Model
#'      
#'      $SSR                # Sum of Squared Errors due to Regression
#'      
#'      $MST                # Total Mean Squared Errors (using mean of y)
#'      
#'      $MSE                # Mean Squared Errors in the model
#'      
#'      $cod                # Coefficient of Determination
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
    
    # Standard Deviations of each variable
    info$sd.x = sd(x)
    info$sd.y = sd(y)
    
    
    # Correlation and Covariance Information
    info$cor = cor(x, y)
    
    # Info regarding significance of correlation using rule of thumb
    info$rot_significance = 2/sqrt(info$n)
    info$rot_is_significant = abs(info$cor) > info$rot_significance
    
    # Slope and intercept information
    #info$slope = sum((x - mean.x)*(y - mean.y)) / sum((x - mean.x)^2)
    #info$intercept = mean.y - (info$slope * mean.x)
    # Use an alternative formula that makes use of already calculated values. 
    info$slope = info$cor *  info$sd.y / info$sd.x
    info$intercept = info$mean.y - info$slope * info$mean.x
    
    # Sum of Squares information 
    info$SST = sum((y - mean.y)^2)  # Total Sum of Squares
    info$SSE = sum(((y - mean.y) - (x - mean.x)*info$slope)^2) # SSE of regression model
    info$SSR = info$SST - info$SSE  # Sum of Squares due to regression
    
    # Mean Squared Error information 
    info$MST = info$SST / info$n
    info$MSE = info$SSE / info$n
    
    # Coefficient of Determination
    info$cod = info$SSR / info$SST 
    
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
        printkv("SD of independent variable", info$sd.x, 
                fill=colspan, fill_char=".")
        printkv("SD of dependent variable", info$sd.y, 
                fill=colspan, fill_char=".")
        printkv("Correlation (Pearson, using 'everything')", info$cor, 
                fill=colspan, fill_char=".")
        printkv("Rule of Thumb Significance for correlation", 
                info$rot_significance, fill=colspan, fill_char=".")
        printkv("Is this Significant? ", info$rot_is_significant, 
                fill=colspan, fill_char=".")
        printkv("Slope", info$slope, fill=colspan, fill_char=".")
        printkv("Intercept", info$intercept, fill=colspan, fill_char=".")
        printkv("Total Sum of Squared Errors (using mean of y)", info$SST, 
                fill=colspan, fill_char=".")
        printkv("Sum of Squared Errors (in the model)", info$SSE, 
                fill=colspan, fill_char=".")
        printkv("Sum of Squared Errors (due to regression)", info$SSR, 
                fill=colspan, fill_char=".")
        printkv("Total Mean Squared Errors (using mean)", info$MST, 
                fill=colspan, fill_char=".")
        printkv("Mean Squared Errors in the model", info$MSE, 
                fill=colspan, fill_char=".")
        printkv("Coefficient of Determination", info$cod, 
                fill=colspan, fill_char=".")
        print("_______________________________________________________________")
    }
    invisible(info)
}





