# A subset of functions belonging to the stat.convenience package that deal with 
# common or useful dataframe operations. 

require("fancyprint")

#' na.summary
#' 
#' Prints a summary of NAs that exist in the dataframe. 
#' 
#' @param df (dataframe) The dataframe you want to check for NAs
#' @param lineup (boolean) Try to line things up nicely. By default it 
#'        is set to TRUE, but if you have one column name that is much longer 
#'        than all the others it may cause the values to be pushed too far to 
#'        the right and may make it look ugly. 
#'        FALSE = disables this automatic lining up feature.
#'        (DEFAULT = TRUE)
#' @examples
#' a = c(1,NA,3,4,5,6,7,8,9,10)
#' bravo = c(1,2,NA,NA,5,6,7,8,9,10)
#' charlie = c(1,2,3,4,5,6,NA,NA,NA,10)
#' delta = c(NA,2,3,4,NA,6,7,8,9,NA)
#' df = data.frame(a,bravo,charlie,delta)
#' na.summary(df)
#' 
#' [1] "=========================================="
#' [1] "             SUMMARY OF NAS               "
#' [1] "=========================================="
#' [1] "Column name: Number of NAs: Indices of NAs"
#' [1] "                                          "
#' [1] "a      : 1: 2"
#' [1] "bravo  : 2: (3, 4)"
#' [1] "charlie: 3: (7, 8, 9)"
#' [1] "delta  : 3: (1, 5, 10)"
#' [1] "=========================================="
#' 
#' @author Ronny Restrepo
#' @export
na.summary <- function(df, lineup=TRUE){
    # TODO: have option to cap the number of indices to show, eg just the first 
    #       5 indices, and show if it has printed all, or if there is more using
    #       ... dots. 
    
    #n.rows = nrow(df)
    col.names = names(df)
    
    if (lineup){
        # Find the length of longest column name. Used to line things up. 
        lineup.space = max(nchar(col.names))    
    } else {
        lineup.space = 0
    }
    
    # Logicals Matrix of all the elements that are NAs
    na.all = is.na(df)
    
    # print out the summary
    print("==========================================")
    print("             SUMMARY OF NAS               ")
    print("==========================================")
    print("Column name: Number of NAs: Indices of NAs")
    print("                                          ")
    for (col in col.names){
        n.na = sum(na.all[,col])
        n.indices = which(is.na(df[,col]))
        val = strkv(n.na, n.indices)
        printkv(col, val, fill=lineup.space)
    }
    print("==========================================")
    
}



