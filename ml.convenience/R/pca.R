# ==============================================================================
#                                                                            PCA 
# ==============================================================================
#' pca
#' 
#' Principle Component Analysis. Compares all the features (columns) for some 
#' dataset, and calculates which pairs of features are highly correlated with 
#' each other. You can specify what threshold to use as your definition of 
#' "highly correlated". You can also plot all the highly correlated pairs.
#' 
#' @param x (dataframe) The dataframe of input features 
#' @param threshold (numeric) The correlation value that you wish to use as the 
#'        cutoff. Feature pairs with a correlation value above this threshold 
#'        will be returned.   
#'        
#'        (DEFAULT = 0.8) 
#' @param plot (logical) Should it print the highly correlated pairs?
#'          
#'        (DEFAULT = FALSE)
#' @return A matrix, where each row contains a pair of column indices from x 
#'         that are highly correlated with each other.   
#'
#' @examples
#' library(kernlab); 
#' data(spam)
#' pca(spam[,-68], threshold=0.7, plot=TRUE)
#' 
#' @author Ronny Restrepo
#' @export
pca <- function(x, threshold=0.8, plot=FALSE){
    
    # Take the correlations of all columns
    correlates <- abs(cor(x)) 
    
    # Every element has a correlation of 1 with itself (the values along the 
    # diagonal) so we want to remove those from our attention by converting to 
    # zeros. 
    diag(correlates) <- 0
    
    # We are looking for features that are highly correlated, so we will look for 
    # indices that have correaltion higher than the threshold. 
    highCorrelates = which(correlates > threshold, arr.ind=T)
    
    # Extract just the unique pairs (remove duplicates)
    a <- matrix(0,0,2)
    for (i in 1:nrow(highCorrelates)){
        a <- rbind(a, sort(highCorrelates[i,]))
    }
    colnames(a) = c("cola", "colb")
    highCorrelates = unique(a)
    
    
    if (plot){
        plot.new()
        
        # Take backup of current drawing parameters
        BU_par_grids = par("mfrow")
        BU_par_mar = par("mar")
        
        # Set up new drawing parameters to draw multiple plots in a grid
        cell_dims = ceiling(sqrt(nrow(highCorrelates)))
        par(mar = c(4,4,1,1))
        par(mfrow=c(cell_dims, cell_dims))
        
        # Plot each pair of highly correlated features
        for (i in 1:nrow(highCorrelates)){
            plot(x[,highCorrelates[i,1]], x[,highCorrelates[i,2]], 
                 xlab=names(x)[highCorrelates[i,1]], 
                 ylab=names(x)[highCorrelates[i,2]], 
            )
        }
        
        # restore previous drawing parameters
        par(mfrow = BU_par_grids)
        par(mar = BU_par_mar)
    }
    return(highCorrelates)   
}