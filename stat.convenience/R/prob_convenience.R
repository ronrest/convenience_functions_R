#library("plot.convenience")
library("fancyprint")

#' coinflipper
#' 
#' simulates coin flips
#' 
#' @param n.flips
#' @param n.success
#' @param p 
#' 
#' @export
coinflipper <- function(n.flips=1, n.success=1, p=0.5){
    # Flips a coin multiple times, and it:
    # - plots a distribution plot for n number of flips
    # - tells you how many possible ways there are to get exactly n successes
    # - tells you probability of getting exactly n number of successes.
    # TODO: I had to comment out the call to plot distribution to avoid 
    #       circularities in dependencies. Find a way to deal with this.
    #plot_distribution(dist="binomial", n=n.flips, p=p)
    total.outcomes = 2^n.flips
    p.satisfy = dbinom(3, size=5, p=0.5)
    n.satisfy = p.satisfy * total.outcomes
    
    #TODO: include print out of getting n or more, n or less successes. 
    fill = 60
    print("===================================================================")
    print(sprintf("Flipping a coin %d times with p=%0.2f", n.flips, p))
    print("===================================================================")
    printkv("Total number of possibilities : ", total.outcomes, sep=" ", fill=fill, fill_char="_")
    print("")
    printkv(sprintf("Possible ways of getting exactly %d successes: ", n.success), n.satisfy, sep=" ", fill=fill, fill_char="_")
    printkv(sprintf("Probability of getting exactly %d successes: ", n.success), p.satisfy, sep=" ", fill=fill, fill_char="_")
    print("")
    print("===================================================================")
}

# 
# #source("~/programming/R/projects/convenience/prob_convenience.R")
# #coinflipper(n.flips=9, n.success=2, p=0.5)
# 
