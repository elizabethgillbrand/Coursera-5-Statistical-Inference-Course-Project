####Step 1: a single mean calculation with large sample size
x<-rexp(1000,.2)
hist(x, xlab="Sample size of 1000 with lambda=.2", main="Draws from an Exponential Distribution")
mean(x)

####Step 2: simulation with many mean calculations from smaller samples
library(ggplot2)
nosim <- 1000
cfunc <- function(x, n) sqrt(n) * (mean(x) - 5) / 5
dat <- data.frame(
        x = c(apply(matrix(rexp(nosim*10,.2), 
                           nosim), 1, cfunc, 10),
              apply(matrix(rexp(nosim*20,.2), 
                           nosim), 1, cfunc, 20),
              apply(matrix(rexp(nosim*40,.2), 
                           nosim), 1, cfunc, 40)
        ),
        size = factor(rep(c(10, 20, 40), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)