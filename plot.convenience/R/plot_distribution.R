#' plot_distribution
#' 
#' Plots what a particular distribution looks like given parameters such as 
#' - mean and standard deviation (for normal distribution).
#' - size and p (for binomial distribution)
#' - df (for t distribution)
#' - df and df2 (for f distribution)
#' - mean (for poisson distribution)
#' 
#' @param dist "normal" "poisson" "binomial" "t" "f" "exp" determines the   
#'             distribution to use.
#' @param  mean numeric. The mean of the distribution. If using poisson, this 
#'             is ths lambda value.
#'             DEFAULT = 0 if using normal distribution. 
#'             DEFAULT = 1 if using poisson distribution. 
#' @param  sd  Standard deviation. 
#'             DEFAULT = 1
#' @param  n   int. Used when dealing with binomial distribution. The number 
#'             of times we run the binomial event, eg flip a coin.
#'             DEFAULT: 1
#' @param  p   numeric. probability of success when using binomial 
#'             deistribution
#'             DEFAULT: 0.5
#' @param  df  Degrees of Freedom when using t distribution. 
#'             Or the first degrees of freedom when using f distribution
#'             DEFAULT = 1   (if "t" distribution chosen)
#'             DEFAULT = 10  (if "f" distribution chosen)
#' @param  df2 Second degrees of freedom when using f distribution
#'             DEFAULT = 100
#' @param rate Used for exponential Distribution
#'             DEFAULT = 1
#' @param  res integer. Resolution of the plot (measured as the number of 
#'             data points along the x axis)
#'             Not implemented for Poisson distribution yet.
#'             DEFAULT = 100 if using normal distribution
#' @param return.df should it return a dataframe of the x and y values? 
#'                  DEFAULT = FALSE
#' @param primary   boolean. Whether to plot as primary plot using plot() or 
#'                  append to an exisitng plot using points()
#'                  DEFAULT = TRUE
#' @param show.mean Show a vertical line highlihgting mean of the distribution?
#'                  DEFAULT = TRUE
#' @param  p.lower  numeric. A quantile used to calculate the lower end of the 
#'                  x axis to plot. eg, if a value of 0.25 is used, then the x  
#'                  axis will start at the 25th percentile of the distribution.
#'                  DEFAULT = 0.0001 
#' @param  p.upper  numeric. quantile used to calculate the upper end of the 
#'                  x axis to plot.
#'                  DEFAULT = 0.9999
#' @param ...       other parameters to pass onto the plot
#' 
#' @author Ronny Restrepo
#' @examples
#' plot_distribution("normal", mean=50, sd=15)
#' plot_distribution("binomial", n=15, p=0.9)
#' plot_distribution("t", df=12)
#' plot_distribution("exp", rate=0.2)
#' plot_distribution("f", df=10, df2=100)
#' plot_distribution("poisson", mean=50)
#' 
#' @export
plot_distribution <- function(dist="normal", mean=NA, sd=NA, n=NA, p=NA, df=NA, 
                              df2=NA, rate=NA, res=100, return.df=FALSE, 
                              primary=TRUE, p.lower=0.0001, p.upper=0.9999, ...){
    
    #===========================================================================
    # TODO: give option to centre t-test at mean (instead of just 0) and 
    #       calculate how the variance should be plotted at that scale. 
    # TODO: scale the resolution of poisson distributions (and perhaps binomial) 
    #       so that there isnt a ridiculous amount of bars for large numbers. 
    # TODO: add option "lambda" for poisson distribution. It makes using poisson 
    #       more intuitive. You can still use mean if you like, but if you 
    #       include both, then lambda takes presedence. 
    # TODO: Check the data types of the inputs
    # TODO: Add option to draw exponential distribution using either lambda OR 
    #       the mean (where mean = 1/lambda) or sd (sd=1/lambda). But give
    #       preference in this order (lambda, mean, sd) if more than one of 
    #       those arguments is given.  
    # TODO: currently the vertical line is plotting median not mean
    # TODO: implement show.mean option
    #-------------------------------------------------------------------------
    #                                               Handle Normal Distribution
    #-------------------------------------------------------------------------
    if (dist=="normal"){
        if (is.na(mean)){ mean = 0} 
        if (is.na(sd)){ sd = 1 }
        x_min = qnorm(p.lower, mean=mean, sd=sd)
        x_max = qnorm(p.upper, mean=mean, sd=sd)
        x = seq(x_min, x_max, by=(x_max-x_min)/res)
        y = dnorm(x, mean=mean, sd=sd)
        title = sprintf("Normal Distribution with\n mean=%.2f and sd=%.2f", mean, sd)
        if (primary){
            plot(x,y, type="l", main=title, ...)
        } else {
            points(x,y, type="l", ...)
        }
        abline(v=qnorm(0.5, mean=mean, sd=sd), col="red")
    } 
    #-------------------------------------------------------------------------
    #                                                    Handle t Distribution
    #-------------------------------------------------------------------------
    else if (dist=="t"){
        if (is.na(df)){ df = 1}
        x_min = qt(p.lower, df=df)
        x_max = qt(p.upper, df=df)
        x = seq(x_min, x_max, by=(x_max-x_min)/res)
        y = dt(x, df=df)
        title = sprintf("t Distribution with\n df = %.2f", df)
        if (primary){
            plot(x,y, type="l", main=title, ...)
        } else {
            points(x,y, type="l", ...)
        }
        abline(v=qt(0.5, df=df), col="red")
    } 
    #-------------------------------------------------------------------------
    #                                                  Handle exp Distribution
    #-------------------------------------------------------------------------
    else if (dist=="exp"){
        if (is.na(rate)){ rate = 1}
        x_min = qexp(p.lower, rate=rate)
        x_max = qexp(p.upper, rate=rate)
        x = seq(x_min, x_max, by=(x_max-x_min)/res)
        y = dexp(x, rate=rate)
        title = sprintf("Exponential Distribution with\n rate = %.2f", rate)
        if (primary){
            plot(x,y, type="l", main=title, ...)
        } else {
            points(x,y, type="l", ...)
        }
        abline(v=qexp(0.5, rate=rate), col="red")
    } 
    #-------------------------------------------------------------------------
    #                                                    Handle f Distribution
    #-------------------------------------------------------------------------
    else if (dist=="f"){
        if (is.na(df)){ df = 10}
        if (is.na(df2)){ df2 = 100}
        x_min = qf(p.lower, df1=df, df2=df2)
        x_max = qf(p.upper, df1=df, df2=df2)
        x = seq(x_min, x_max, by=(x_max-x_min)/res)
        y = df(x, df1=df, df2=df2)
        title = sprintf("f Distribution with\n df1 = %.2f and df2 = %.2f", df, df2)
        if (primary){
            plot(x,y, type="l", main=title, ...)
        } else {
            points(x,y, type="l", ...)
        }
        abline(v=qf(0.5, df1=df, df2=df2), col="red")
    }
    #-------------------------------------------------------------------------
    #                                              Handle Poisson Distribution
    #-------------------------------------------------------------------------
    else if (dist=="poisson"){
        if (is.na(mean)){ mean = 1}
        mean = round(mean)                     # removes any fractional components
        x_min = qpois(p.lower, lambda=mean)
        x_max = qpois(p.upper, lambda=mean)
        x = x_min:x_max
        y = dpois(x, lambda=mean)
        title = sprintf("Poisson Distribution with\n lambda=%d", mean)
        if (!primary){
            warning("Secondary plotting is not yet implemented for poisson ",
                    "distribution")
        } else {
            barplot(y, names.arg=x, main=title, ...)    
        }
        # TODO: Find an alternative to abline that actually places a vertical 
        #       line in the correct position when using in conjunction with 
        #       barplot.
        #abline(v=qpois(0.5, lambda=mean), col="red", lwd="5")
    }
    #-------------------------------------------------------------------------
    #                                             Handle Binomial Distribution
    #-------------------------------------------------------------------------
    else if (dist=="binomial"){
        #TODO: check the data types of the inputs
        if (is.na(n)){ n = 1}
        if (is.na(p)){ p = 0.5}
        x_min = qbinom(p.lower, size=n, prob=p)
        x_max = qbinom(p.upper, size=n, prob=p)
        x = x_min:x_max
        y = dbinom(x, n,prob=p)
        title = sprintf("Binomial Distribution with\n n=%d and p=%.3f", n, p)
        if (!primary){
            warning("Secondary plotting is not yet implemented for binomial ",
                    "distribution")
        } else {
            barplot(y, names.arg=x, main=title, ...)
        }
        # TODO: Find an alternative to abline that actually places a vertical 
        #       line in the correct position when using in conjunction with 
        #       barplot.
        #abline(v=qpois(0.5, lambda=mean), col="red", lwd="5")
    }
    #-------------------------------------------------------------------------
    #                                 Return the dataframe if it was requested
    #-------------------------------------------------------------------------
    if (return.df){
        return(data.frame(x, y))
    } 
    else return(NA)
}



# plot_sample_dist(n, mean=NA, sd=NA, overlay=TRUE){
#     # TODO: actually implement this function. 
#     # A funciton that draws a sample distribution plot. You can optionally 
#     # select to ovelray the plot of the population/sample that the sample 
#     # distribution comes from, so you can see how the variance has shrunk. 
#     # Optionally, you can also make it plot some confidence interval, either as 
#     # a percentage (eg you specify you want to draw the 95% confidence interval)
#     # or by range (eg, you specify that you are interested in values bewtween 
#     # x_min and x_max).
#     # 
#     print("TODO: still need to implement plot_sample_dist()")
#     #lower.tail = pnorm(lower.int, mean=mean, sd=SE)
#     #upper.tail = pnorm(upper.int, mean=mean, sd=SE, lower.tail=FALSE)
#     #estimated_prob = 1 - lower.tail - upper.tail     
#     
# }


#source("~/programming/R/projects/convenience/plot_convenience.R")


# Plot Common Distributions
#plot_distribution("normal", mean=100, sd=15)
#plot_distribution("poisson", mean=100)
#plot_distribution("binomial", n=3, p=0.5)
#plot_distribution("binomial", n=500, p=0.01)
#plot_distribution("normal", mean=100, sd=15, p.lower=0.25, p.upper=0.75)

#plot_distribution("t", df=3)
#plot_distribution("f", df=10, df2=100)
#plot_distribution("exp", rate=3)

#plot_distribution("normal", mean=5, sd=5, primary=TRUE, col="green")
#plot_distribution("exp", rate=0.1, primary=FALSE, col="pink")

#df = plot_distribution("normal", mean=100, sd=15, return.df=TRUE)
#shade_after(df$x, df$y, 120)
#shade_before(df$x, df$y, 120)

# Shades the tails before -5 and after 5 in a bell curve
#x = seq(-9,9,by=0.9)
#y = dnorm(x, mean=0, sd=3)
#shade_outside(x,y,-3, 2, type="o", lwd=2, shade.density=20)