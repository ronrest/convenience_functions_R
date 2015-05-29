shade_after <- function(x, y, boundary, ...){
    # Shades area under the curve from boundary to values of x that are higher.
    # TODO: BUG: Only printing graph with dots, specifying lines not working
    upper <- x[length(x)]
    shade_between(x, y, boundary, upper)
}
