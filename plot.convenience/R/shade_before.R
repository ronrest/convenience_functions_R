shade_before <- function(x, y, boundary, ...){
    # Shades area under the curve from boundary to values of x that are lower.
    # TODO: BUG: Only printing graph with dots, specifying lines not working
    lower <- x[1]
    shade_between(x, y, lower, boundary)
}
