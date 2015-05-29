#library(Hmisc)  # Used for floating point matches
#TODO: Find a better alternative to Hmisc, that doesnt rely on so many other 
#      secondary libraries which slow load time.
#      find.matches() requires Hmisc


shade_after <- function(x, y, boundary, ...){
    # Shades area under the curve from boundary to values of x that are higher.
    # TODO: BUG: Only printing graph with dots, specifying lines not working
    upper <- x[length(x)]
    shade_between(x, y, boundary, upper)
}

shade_before <- function(x, y, boundary, ...){
    # Shades area under the curve from boundary to values of x that are lower.
    # TODO: BUG: Only printing graph with dots, specifying lines not working
    lower <- x[1]
    shade_between(x, y, lower, boundary)
}









