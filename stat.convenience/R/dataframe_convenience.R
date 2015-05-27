# A subset of functions belonging to the stat.convenience package that deal with 
# common or useful dataframe operations. 

# TODO: Add a function assumption.violators() which is somewhat similar to 
#       na.summary() but looks for data that violates certain assumptions. 
#       eg, if the values in columns are between 0-1, or that they are all 
#       positive numbers, or that they are all integers, or they are all 
#       strings that have a certain regex pattern, or they are all strings 
#       that fall into only a preselected set of values.... etc.  
#       
#       Perhaps a new kind of object should be used for this kind of task. 
#       - eg, you create an "assumption" object. And then feed that assumption
#         object to the assumption.violators() function, and it tells you
#         which columns violate that assumption. 
#         - eg,  
#               zero_to_one = assumtion(type=numeric, min=0, max=1)
#               assumption.violators(myDF, asp=zero_to_one) 
#           
#       Since different collumns have different assumptions, you should 
#       be able to specify what columns you want to test for some particular 
#       assumption (either by column name, or by index number).
#       
#       The assumption object should also work along the lines of: 
#               zero_to_one = assumtion(type=numeric, min=0, max=1)
#               positiveFloats = assumption(type=numeric, min=0, max=inf)
#               negativeFloats = assumption(type=numeric, min=-inf, max=0)
#               integer_asp = assumption(type=integer)
#               factors_occup_asp = assumption(type=factor, levels=c("teacher", 
#                                              "doctor", "firefighter"))
#               
#       Also consider testing if the variables are normally distributed within 
#       some acceptable threshold (eg by using skew/kurtosis/qplots to test).
#       - Or also to test that there are no crazy outliers. 
#
# TODO: create summary2() function, which prints out a summary that contains 
#       - similar kind of output as summary() but, printed out in similar 
#         way to my na.summary() function. 
#       - contains info about missing values. eg NA=34(23%)
#       - like str() it should also show data type
#       - for numeric values it should show min, max, median, mean, and perhaps 
#         skew, kurtosis.
#       - should also highlight if the values are within the range of 0-1, 
#         or if they are all positive, or all negative. 
#         - but you should be able to specify which of these values to include/
#           exclude to keep things pretty and uncluttered. 
#       - For integers, do same output as numeric. 
#       - For factors, it should return the number of factors, and a printout 
#         of the first few factors, and you should be able to specify how many 
#         to print out. 
#       - For strings, do similar to factors, plus, min length of strings, and 
#         max length of characters. 
#       - like na.summary() it should return a dataframe
#       - The printout should be grouped by class types, so all numerics
#         grouped togeerher, all strings grouped together, factors, etc...
#         This allows the columns of the output to remain compact. 
#
#
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
#' @param thresh (numerical) threshold value between 0 and 1, representing 
#'          proportion of NAs in the column. So iff you use thresh=0.3 it 
#'          returns NA information for all the columns that are composed of more 
#'          than 30% NAs 
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
#'            This overrides the `only.nas` and `thresh` arguments. 
#'          
#'          (DEFAULT = FALSE)
#' @param printit (logical) Should it print a pretty version of the summary? 
#' 
#'          (DEFAULT = TRUE)
#' @return silently returns a dataframe with the following columns:
#' 
#'          $colName the names of the columns of interest
#'          
#'          $total the total number of NAs in that column
#'          
#'          $proportion the proportion of NAs in that column 
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
na.summary <- function(df, lineup=TRUE, limit=8, thresh=NA, only.nas=FALSE, 
                          no.nas=FALSE, printit=TRUE){
    
    # TODO: Detect other common forms of missing values. eg:
    #       - "#DIV/0!"
    #       - "-"
    #       - "none", "NONE", "None"
    #       - "missing", "MISSING"
    
    # TODO: give option to return a vector of either colum indices, or column 
    #       names of the columns that have nas, or the ones with no nas. 
    
    # TODO: print out the number of columns that contained NAs in them. And 
    #       what proportion this is. 
    
    # TODO: allign total nas to the right, so digits, tens, hundreds, etc line up
    
    # TODO: reimplement the feature to show the indices of NAs per column
    
    require("fancyprint")
        
    all.nas = is.na(df)                 # All elements that are NAs in df
    totals = colSums(all.nas)           # Total number of NAs for each column
    proportions = colMeans(all.nas)     # Proportion of NAs for each column
    
    # Determine if to keep only the ones with no NAs, only with NAs, or all
    if (no.nas) {
        filter = names(totals[totals == 0])
    }else if (!is.na(thresh)){
        filter = names(proportions[proportions > thresh])    
    }else if (only.nas){
        filter =  names(totals[totals > 0])
    } else {
        filter = names(totals)
    }
    
    # Create dataframe of the relevant columns
    filtered.nadf = data.frame(colName=filter, total=totals[filter], 
                               proportion=proportions[filter], row.names=NULL, 
                               stringsAsFactors=FALSE)
    #nadf
    
    # print out the summary
    if (printit){
        col.names = names(filtered.nadf)
        # ----------------------------------------------------------- Headings
        print("==========================================")
        print("             SUMMARY OF NAS               ")
        print("==========================================")
        if (no.nas){
            print("Columns not containing any NAs")
        } else {
            print("Col name: Num NAs (percent): Indices of NAs")
        }
        print("                                          ")
        
        # --------------------------------------------------- Line up settings
        if (lineup){
            # Find the length of longest column name. Used to line things up. 
            lineup.space = max(nchar(filtered.nadf$colName))    
        } else {
            lineup.space = 0
        }
        
        # -------------------------------------------- Print each columns info
        library(fancyprint)
        for (i in 1:nrow(filtered.nadf)){
            row = filtered.nadf[i,]
            percent = paste("(",round(row$prop*100, digits=2), "%", ")",sep="")
            val = strkv(row$total, percent, fill=4, sep="")
                        
            # Handle how many indices of the NAs to show per column
            #if (!only.nas){
            #    n.indices = head(which(is.na(df[,col])), limit)
            #    if (row$total > limit){
            #        n.indices = c(n.indices, "...")
            #    } 
            #    val = strkv(n.na, n.indices)
            #    
            #}
            printkv(row$colName, val, fill=lineup.space)
        }
        print("==========================================")
    }
    invisible(filtered.nadf)
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
    #TODO: option to use a threshold value, where it only filters out row/column
    #      if there is more than a certain proportion of them for that row/column
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
    # TODO: add another method "na", with pattern=0.9 where if a column contains
    #       more than 90% NAs in it, then it gets filtered. 
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



