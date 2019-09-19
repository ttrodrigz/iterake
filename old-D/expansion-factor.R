n <- 20
N <- 35

a <- abs(rnorm(n, 1, 0.25))
a <- mpace::rescale_vector(a, "sum", length(a))
a

xpand <- function(wgt, N) {
    
    # expansion factor is the proportion of population to sample size
    n <- length(wgt)
    
    a * (N / n)
    
}


xpand(a, N)
xpand(a, N) %>% sum()
xpand(a, N) %>% mean()

# this works, wtf do we call it?
# what's the initial value if we have them provide N?
# should we just have them plug in an actual expansion factor?
    # we could then default to 1...
    # let them provide either? is.missing()'s?
    # datacamp says to include sensible defaults for all non-data params

