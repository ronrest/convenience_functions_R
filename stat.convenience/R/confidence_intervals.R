# ==============================================================================
#                                                                         CBINOM
# ==============================================================================
#' cbinom
#' 
#' Confidence region for a binomial Distribution. 
#' 
#' @param size int. number of trials for the binomial distribution.
#' @param prob numeric. probability of success per trial
#' @param type string. type of hypothesis test taken. 
#'        
#'        - for two tailed test use "equal" (Default)  or "="
#'        
#'        - for one-tailed lower tail test use "less" or "<" 
#'        
#'        - for one-tailed upper tail test use "more" or "greater" or "higher" 
#'          or ">" 
#' @param conf confidence interval used
#' @return a vector with two values representing the lower and upper points that 
#'         fit within your confidence interval.
#' @examples
#' cbinom(size=100, p=0.7, type="equal", conf=0.99)
#' cbinom(size=15, p=0.9, type="less", conf=0.95)
#' cbinom(size=30, p=0.4, type="more", conf=0.90)
#' @export
cbinom <- function(size=1, prob=0.5, type="equal", conf=0.95){
    # Account for the different types of cutoff quantiles
    alpha = 1 - conf
    if ((type == "less") | (type == "<")){
        p_lower = alpha
        p_upper = 1.0
    } else if ((type == "more") | (type == "greater") | (type == "higher") 
               | (type == ">")){
        p_lower = 0.0
        p_upper = conf
    } else if ((type == "equal") | (type == "=")){
        p_lower= alpha/2
        p_upper = 1 - (alpha/2)
    }
    
    # calculate the cutoff points
    cutoff_lower = qbinom(p_lower, size=size, prob=prob, lower.tail=TRUE)
    cutoff_upper = qbinom(p_upper, size=size, prob=prob, lower.tail=TRUE)
    
    return(c(cutoff_lower, cutoff_upper))
}



# ==============================================================================
#                                                                             CT
# ==============================================================================
#' ct
#' 
#' Confidence region for a T Distribution. 
#' 
#' @param df (int) degrees of freedom.
#' @param type string. type of hypothesis test taken. 
#'        
#'        - for two tailed test use "equal" (Default)  or "="
#'        
#'        - for one-tailed lower tail test use "less" or "<" 
#'        
#'        - for one-tailed upper tail test use "more" or "greater" or "higher" 
#'          or ">" 
#' @param conf confidence interval used
#' @return a vector with two values representing the lower and upper points that 
#'         fit within your confidence interval.
#' @examples
#' ct(df=20, type="equal", conf=0.99)
#' ct(df=20, type="=", conf=0.90)
#' ct(df=20, type="less", conf=0.95)
#' ct(df=20, type="<", conf=0.90)
#' ct(df=20, type="more", conf=0.90)
#' ct(df=20, type=">", conf=0.90)
#' @export
ct <- function(df=10, type="equal", conf=0.95){
    # Account for the different types of cutoff quantiles
    alpha = 1 - conf
    if ((type == "less") | (type == "<")){
        p_lower = alpha
        p_upper = 1.0
    } else if ((type == "more") | (type == "greater") | (type == "higher") 
               | (type == ">")){
        p_lower = 0.0
        p_upper = conf
    } else if ((type == "equal") | (type == "=")){
        p_lower= alpha/2
        p_upper = 1 - (alpha/2)
    }
    
    # calculate the cutoff points
    cutoff_lower = qt(p_lower, df=df, lower.tail=TRUE)
    cutoff_upper = qt(p_upper, df=df, lower.tail=TRUE)
    
    return(c(cutoff_lower, cutoff_upper))
}
