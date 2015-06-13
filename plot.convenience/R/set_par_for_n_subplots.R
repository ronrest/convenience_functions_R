#===============================================================================
#                                                         SET_PAR_FOR_N_SUBPLOTS
#===============================================================================
#' Helper function to set global parameters of par() that are appropriate for 
#' plotting n number of cells. 
#' 
#' You secify n, and it automatically calculates what size the grid, and other 
#' plotting parameters to generate a nice layout.  
#===============================================================================
set_par_for_n_subplots <- function(n){
    # Calculate number of cells per row/column to plot
    if (n == 1){
        ngridRows = 1
        ngridCols = 1
    } else if (n == 2){
        ngridRows = 2
        ngridCols = 1
    } else {
        ngridCols = ceiling(sqrt(n))
        ngridRows = ceiling(n/ngridCols)
    }
    par(mfrow=c(ngridRows,ngridCols), mar=c(1, 1, 2, 0.5), 
        mgp = c(1.5, 0.3, 0), tck = -0.01, oma=c(1, 1, 0.5, 0.5))
}

# DEFAULTS
#par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1), 
#            mgp = c(3, 1, 0), tck = NA, oma=c(0, 0, 0, 0))