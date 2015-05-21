# A subset of functions belonging to the stat.convenience package that deal with 
# common or useful dataframe operations. 

# TODO: Add a function na.mean() which returns a copy of the dataframe but with 
#       NAs replaced with the mean of that column. 

# TODO: add a function na.thresh() which is similar to no.nas() but which only 
#       removes rows/cols when there are more than some threshold of NAs. 

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
#' @param limit (integer) max number of NA indices to print out
#' 
#'         (DEFAULT = 8) 
#' @param only.nas (logical) Only return summaries for columns containing NAs?
#' 
#'          - if FALSE, then it gives summary of all columns. 
#'          
#'          - if TRUE, then it only prints out sumary for those columns 
#'            containing at least one NA. 
#'          
#'          (DEFAULT=FALSE)
#'          
#' @param no.nas (logical) prints out only the columns that dont contain any NAs
#'  
#'          - If TRUE, then it only shows the columns that dont contain any NAs. 
#'            This overrides the `only.nas` argument. 
#'          
#'          (DEFAULT = FALSE)
#' @return silently returns a vector of strings representing the columns that 
#'         contain NAs (or the columns that dont contain any NAs if no.nas=TRUE) 
#' @examples
#' a = c(1,2,3,4,5,6,7,8,9,10)
#' bravo = c(1,2,NA,NA,5,6,7,8,9,10)
#' charlie = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
#' delta = c(NA,2,3,4,NA,6,7,8,9,NA)
#' df = data.frame(a,bravo,charlie,delta)
#' na.summary(df)
#' #[1] "=========================================="
#' #[1] "             SUMMARY OF NAS               "
#' #[1] "=========================================="
#' #[1] "Column name: Number of NAs: Indices of NAs"
#' #[1] "                                          "
#' #[1] "a      : 0: "
#' #[1] "bravo  : 2: (3, 4)"
#' #[1] "charlie: 10: (1, 2, 3, 4, 5, 6, 7, 8, ...)"
#' #[1] "delta  : 3: (1, 5, 10)"
#' #[1] "=========================================="
#' 
#' na.summary(df, only.nas=TRUE)
#' #[1] "=========================================="
#' #[1] "             SUMMARY OF NAS               "
#' #[1] "=========================================="
#' #[1] "Column name: Number of NAs: Indices of NAs"
#' #[1] "                                          "
#' #[1] "bravo  : 2: (3, 4)"
#' #[1] "charlie: 10: (1, 2, 3, 4, 5, 6, 7, 8, ...)"
#' #[1] "delta  : 3: (1, 5, 10)"
#' #[1] "=========================================="
#' 
#' na.summary(df, no.nas=TRUE)
#' #[1] "=========================================="
#' #[1] "             SUMMARY OF NAS               "
#' #[1] "=========================================="
#' #[1] "Columns not containing any NAs"
#' #[1] "                                          "
#' #[1] "a: 0"
#' #[1] "=========================================="
#' 
#' @author Ronny Restrepo
#' @export
na.summary <- function(df, lineup=TRUE, limit=8, only.nas=FALSE, no.nas=FALSE){
    # TODO: use colSums(is.na(df)) instead of doing for loops and summing 
    #       individual columns.
    
    # TODO: give option to return a vector of either colum indices, or column 
    #       names of the columns that have nas, or the ones with no nas. 
    
    # TODO: print out the number of columns that contained NAs in them. And 
    #       what proportion this is. 
    
    require("fancyprint")
    
    #n.rows = nrow(df)
    col.names = names(df)
    
    if (lineup){
        # Find the length of longest column name. Used to line things up. 
        lineup.space = max(nchar(col.names))    
    } else {
        lineup.space = 0
    }
    
    # keep track of all the columns containing NAs (or no NAs if no.nas=TRUE)
    cols.with.nas = c() 
    
    # Logicals Matrix of all the elements that are NAs
    na.all = is.na(df)
    
    # print out the summary
    print("==========================================")
    print("             SUMMARY OF NAS               ")
    print("==========================================")
    if (no.nas){
        print("Columns not containing any NAs")
    } else {
        print("Column name: Number of NAs: Indices of NAs")
    }
    print("                                          ")
    for (col in col.names){
        n.na = sum(na.all[,col])
        
        # Append to cols.with.nas if it contains NAs (or no NAs if no.nas=TRUE)
        if (!no.nas & n.na > 0){cols.with.nas = c(cols.with.nas, col)}
        else if (no.nas & n.na == 0){cols.with.nas = c(cols.with.nas, col)}
        
        # If no.nas is set to TRUE, then only print out the columns with no NAs
        if (no.nas){
            if (n.na == 0){
                printkv(col, 0)
            }
        }
        # If only.nas is TRUE, and there are no NAs, then skip, otherwise print 
        # out the summary for that column 
        else if ((n.na > 0) | (!only.nas)){
            n.indices = head(which(is.na(df[,col])), limit)
            if (n.na > limit){
                n.indices = c(n.indices, "...")
            } 
            val = strkv(n.na, n.indices)
            printkv(col, val, fill=lineup.space)    
        }
    }
    print("==========================================")
    invisible(cols.with.nas)
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
#' columns whose names fit some criteria. That criteria could be a regular 
#' expression pattern, or it could be a vector of explicit column names. You can 
#' chose if the columns that match the criteria should be the ones returned, or 
#' if they are the ones that should be excluded. 
#' 
#' @param x (dataframe) The dataframe you want to filter.
#' @param pattern 
#' 
#'          - (string) A regular expression (if method="regex")
#'          
#'          - (vector of strings) the names of columns to filter 
#'            (if method="list")
#'          
#' @param exclude (logical) Should the columns matching the criteria be filtered 
#'        out? 
#'        
#'        - If TRUE, then it returns all columns except the ones matching the 
#'        criteria 
#'        
#'        - If FALSE, then it returns only the columns that match.
#'        
#'         (DEFAULT = FALSE) 
#' @param method (string) the type of criteria to be used. 
#'          
#'          - "regex" (DEFAULT) use a regular expression pattern
#'          
#'          - "list" when you want to provide a vector of column names to filter
#'  
#' @param ...  aditional parameters to be passed on to grep()
#' @return a copy of the dataframe with columns filtered out. 
#' @examples
#' 
#' # return only the columns with "p" in their names
#' filter.columns(mtcars, "p")
#' 
#' # return only the columns WITHOUT "p" in their names
#' filter.columns(mtcars, "p", exclude=TRUE)
#' 
#' # return only the columns whose names are in the filter vector below
#' filter = c("mpg", "cyl", "gear")
#' filter.columns(mtcars, filter, method="list")
#' 
#' # return only the columns whose names are NOT in the filter vector below
#' filter.columns(mtcars, filter, method="list", exclude=TRUE)
#' 
#' @author Ronny Restrepo
#' @export
filter.columns <- function(x, pattern=NA, exclude=FALSE, method="regex", ...){
    if (method=="regex"){
        # Columns matching the pattern
        cols = grep(pattern, names(x), ...) 
        
        # decide if to exclude those columns, or the keep them
        if (exclude){
            return(x[, -cols])
        } else {
            return(x[, cols])
        }
    } else if (method == "list"){
        print(pattern)
        if (any(is.na(pattern))){
            warning("You must have ONLY string values for pattern, when using",
                    "method='list'. Returning NA")
            return(NA)
        }
        if (exclude){
            return(x[, setdiff(names(x), pattern)])
        } else {
            return(x[,pattern])
        }
    }
}



