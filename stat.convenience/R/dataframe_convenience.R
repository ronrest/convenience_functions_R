# A subset of functions belonging to the stat.convenience package that deal with 
# common or useful dataframe operations. 

# TODO: Add a function na.mean() which returns a copy of the dataframe but with 
#       NAs replaced with the mean of that column. 

library("fancyprint")

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
    
    require("fancyprint")
    
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
#' Returns a copy of a dataframe which has all rows (or all columns) containing 
#' NAs removed. You specify if its the columns or rows that you want to remove. 
#' 
#' By default, it removes rows containing NAs. But you can remove the columns 
#' containing NAs by setting byCols=TRUE
#' 
#' @param df (dataframe) The dataframe you want to clean up. 
#' @param byCols (logical) Remove cols containing the NAs? 
#' 
#'      - if TRUE, then it removes the columns containing NAs. 
#'      
#'      - If FALSE, then it removes the rows containing the NAs (DEFAULT)
#' @examples
#' # Create a dataframe
#' a = c(11,12,13,14,15,NA)
#' b = c(21,22,23,NA,25,26)
#' d = c(NA,32,33,34,35,36)
#' e = c(41,42,43,44,45,46)
#' f = c(51,52,53,54,55,56)
#' df = data.frame(a,b,d,e,f)
#' 
#' # Return just the rows with no NAs 
#' no.nas(df)
#' #    a  b  d  e  f
#' # 2 12 22 32 42 52
#' # 3 13 23 33 43 53
#' # 5 15 25 35 45 55
#' 
#' # Return just the columns with no NAs 
#' no.nas(df, byCols=TRUE)
#' #    e  f
#' # 1 41 51
#' # 2 42 52
#' # 3 43 53
#' # 4 44 54
#' # 5 45 55
#' # 6 46 56
#' 
#' @author Ronny Restrepo
#' @export
no.nas <- function(df, byCols=FALSE){
    if (!byCols){
        return(df[complete.cases(df),])
    }
    else {
        cols = sapply(1:ncol(df), function(i) !any(is.na(df[,i])))
        return(df[,cols])
    }
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

