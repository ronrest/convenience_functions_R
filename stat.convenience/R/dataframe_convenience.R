# A subset of functions belonging to the stat.convenience package that deal with 
# common or useful dataframe operations. 

# TODO: Add a function na.mean() which returns a copy of the dataframe but with 
#       NAs replaced with the mean of that column. 

require("fancyprint")

# ==============================================================================
#                                                                     NA.SUMMARY
# ==============================================================================
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

# ==============================================================================
#                                                                         NO.NAS
# ==============================================================================
#' no.nas
#' 
#' Returns a copy of a dataframe which has all rows containing NAs removed. 
#' 
#' @param df (dataframe) The dataframe you want to clean up. 
#' @examples
#' # Create a dataframe
#' a = c(11,NA,13,14,15,16,17,18,19)
#' b = c(21,22,NA,24,25,26,27,28,29)
#' c = c(31,32,33,34,35,36,NA,NA,NA)
#' d = c(NA,42,43,44,45,46,47,48,49)
#' df = data.frame(a,b,c,d)
#' 
#' # return only the rows with no NAs in it
#' no.nas(df)
#' 
#' # Result is
#' # a  b  c  d
#' # 4 14 24 34 44
#' # 5 15 25 35 45
#' # 6 16 26 36 46
#' 
#' @author Ronny Restrepo
#' @export
no.nas <- function(df){
    return(df[complete.cases(df),])
}


# ==============================================================================
#                                                                 FILTER.COLUMNS
# ==============================================================================
#' filter.columns
#' 
#' Returns a copy of a dataframe which filters for, or filters out, only the 
#' columns that fit some regular expression pattern. You can chose if the 
#' columns that match the pattern should be the ones returned, or if they are 
#' the ones that should be excluded. 
#' 
#' @param x (dataframe) The dataframe you want to filter.
#' @param pattern (string) A regular expression.
#' @param exclude (logical) Should the columns matching the pattern be filtered 
#'        out? If TRUE, then it returns all columns except the ones matching the 
#'        pattern.. If FALSE, then it returns only the columns that match.
#'        
#'         (DEFAULT = FALSE) 
#' @param ...  aditional parameters to be passed on to grep()
#' @examples
#' 
#' # Returns only the columns that start with "Petal"
#' data(iris)
#' filter.columns(iris, "^Petal")
#' 
#' # Filters out the columns that start with "Petal"
#' filter.columns(iris, "^Petal", exclude=TRUE)
#' 
#' @author Ronny Restrepo
#' @export
filter.columns <- function(x, pattern, exclude=FALSE, ...){
    # Columns matching the pattern
    cols = grep(pattern, names(x), ...) 
    
    # decide if to exclude those columns, or the keep them
    if (exclude){
        return(x[, -cols])
    } else {
        return(x[, cols])
    }
}

