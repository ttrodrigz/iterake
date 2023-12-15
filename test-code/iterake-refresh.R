
# Overview ----------------------------------------------------------------

# This is my attempt at a syntactically simpler and hopefully faster
# version of the iterake function.

# This does not do any error checking or exception handling and does not
# incoporate the permutation version. But, I think it works!

# Packages ----------------------------------------------------------------

library(tidyverse)
library(collapse)


# Create some data --------------------------------------------------------

ss <- 20

set.seed(101)
tst <- tibble(
    x = factor(sample(letters[1:4], size = ss, replace = TRUE)),
    y = factor(sample(letters[5:7], size = ss, replace = TRUE)),
    z = factor(sample(letters[8:10], size = ss, replace = TRUE)),
    wgt = rep(1, times = ss)
)


# "Wants" -----------------------------------------------------------------

want <- list(
    x = c("a" = 1/4, "b" = 1/4, "c" = 1/4, "d" = 1/4),
    y = c("e" = 0.3, "f" = 0.3, "g" = 0.4),
    z = c("h" = 0.25, "i" = 0.44, "j" = 0.31)
)


# Initial values ----------------------------------------------------------

tmp <- tst
threshold <- 0.0000001
cap <- 3
counter <- 0

wt.cats <- names(want)

have <- 
    tst |> 
    fselect(wt.cats) |> 
    lapply(qtab, w = tst$wgt, dnn = NULL) |> 
    lapply(prop.table)


delta <- sum(map2_dbl(
    .x = want,
    .y = have,
    .f = \(w, h) sum(abs(w - h))
))


# Do stuff ----------------------------------------------------------------

while (delta >= threshold) {
    
    counter <- counter + 1
    
    # Create the weights
    for (i in seq_along(wt.cats)) {
        
        cat.now <- wt.cats[[i]]
        
        # tibble up the wants - eventually this should happen outside
        # of the loop at the initial phases of the function
        want.df <- enframe(
            x = want[[cat.now]],
            name = cat.now,
            value = "want"
        )
        
        tmp <-
            
            tmp |> 
            
            # sum of the weights by group to get the "haves"
            # `fsum()` is faster than `sum()` especially when used
            # in the context of `fgroup_by()` and `fmutate()`
            fgroup_by(cat.now) |> 
            fmutate(have = fsum(wgt) / ss) |> 
            fungroup() |> 
            
            # join in the "wants"
            join(
                y = want.df,
                on = cat.now,
                verbose = FALSE
            ) |> 
            
            # weight factor is the "wants" / "haves"
            # w / h will result in Inf if the denominator is 0
            # `replace_inf()` is faster and uses less memory than
            # using an `ifelse()`
            fmutate(wgt_fct = replace_inf(want / have, value = 0)) |> 
            
            # create the new weight by multiplying the old by the
            # newly created weighting factor
            # `pmin()` faster and uses less memory than `ifelse()`
            fmutate(wgt = pmin(cap, wgt * wgt_fct)) |> 
            
            # grab columns needed
            fselect(wt.cats, "wgt")
        
    }
    
    # Check the deltas
    have <- 
        tmp |> 
        fselect(wt.cats) |> 
        lapply(qtab, w = tmp$wgt, dnn = NULL) |> 
        lapply(prop.table)
    
    delta <- sum(map2_dbl(
        .x = want,
        .y = have,
        .f = \(w, h) sum(abs(w - h))
    ))
    
    
    
}

# Show results
want
have
delta
counter
