
# Overview ----------------------------------------------------------------

# This is my attempt at a syntactically simpler and hopefully faster
# version of the iterake function.

# This does not do any error checking or exception handling and does not
# incorporate the permutation version. But, I think it works!

# Packages ----------------------------------------------------------------

library(tidyverse)
library(collapse)
library(cli)


# Create some data --------------------------------------------------------

ss <- 100

set.seed(101)
tst <- tibble(
    x = factor(sample(letters[1:4], size = ss, replace = TRUE)),
    y = factor(sample(letters[5:7], size = ss, replace = TRUE)),
    z = factor(sample(letters[8:10], size = ss, replace = TRUE))
)


# "Wants" -----------------------------------------------------------------

want <- list(
    x = c("a" = 1/4, "b" = 1/4, "c" = 1/4, "d" = 1/4),
    y = c("e" = 0.3, "f" = 0.3, "g" = 0.4),
    z = c("h" = 0.25, "i" = 0.44, "j" = 0.31)
)

want.tibbles <- want

for (i in seq_along(want)) {
    
    want.tibbles[[i]] <- enframe(
        x = want.tibbles[[i]],
        name = names(want.tibbles[i]),
        value = "want"
    )
    
}


control_iterake <- function(
        threshold = 1e-10,
        max_weight = 3,
        max_iter = 50,
        max_stuck = 5
) {
    list(
        threshold = threshold,
        max_weight = max_weight,
        max_iter = max_iter,
        max_stuck = max_stuck
    )
}

# Do stuff ----------------------------------------------------------------

#' TODO DEVON: permute, missing value handling, make sure new code does the same
#' thing as the old code.

iterake <- function(
        x,
        targets,
        permute = FALSE,
        control = control_iterake()
) {
    
    # Initialize things
    tmp <- fmutate(x, ...wgt... = 1)
    rep.counter   <- 0
    stuck.counter <- 0
    delta.log     <- numeric()
    wt.cats       <- names(targets)
    
    # Stuff from the control
    threshold  <- control$threshold
    max.weight <- control$max_weight
    max.iter   <- control$max_iter
    max.stuck  <- control$max_stuck
    
    # Unpack the wants for faster calculation of the deltas
    want <- map(targets, \(x) pull(x, want, name = 1))
    
    # Calculate the initial "haves"
    have <- 
        tmp |> 
        fselect(wt.cats) |> 
        lapply(qtab, w = tmp[["...wgt..."]], dnn = NULL) |> 
        lapply(prop.table)
    
    # Calculate the sum of the absolute difference of the "wants" and "haves"
    delta <- sum(map2_dbl(
        .x = want,
        .y = have,
        .f = \(w, h) sum(abs(w - h))
    ))
    
    while (delta >= threshold) {
        
        # Increment the counter
        rep.counter <- rep.counter + 1
        
        # Create the weights
        for (i in seq_along(wt.cats)) {
            
            cat.now <- wt.cats[[i]]
            
            want.now <- want.tibbles[[cat.now]]
            
            tmp <-
                
                tmp |> 
                
                # sum of the weights by group to get the "haves"
                # `fsum()` is faster than `sum()` especially when used
                # in the context of `fgroup_by()` and `fmutate()`
                fgroup_by(cat.now) |> 
                fmutate(have = fsum(...wgt...) / ss) |> 
                fungroup() |> 
                
                # join in the "wants"
                join(
                    y = want.now,
                    on = cat.now,
                    verbose = FALSE,
                    how = "left"
                ) |> 
                
                # weight factor is the "wants" / "haves"
                # w / h will result in Inf if the denominator is 0
                # `replace_inf()` is faster and uses less memory than
                # using an `ifelse()`
                fmutate(wgt_fct = replace_inf(want / have, value = 0)) |> 
                
                # create the new weight by multiplying the old by the
                # newly created weighting factor
                # `pmin()` faster and uses less memory than `ifelse()`
                fmutate(...wgt... = pmin(max.weight, ...wgt... * wgt_fct)) |> 
                
                # grab columns needed
                fselect(wt.cats, "...wgt...")
            
        }
        
        # Re-calculate the "haves"
        have <- 
            tmp |> 
            fselect(wt.cats) |> 
            lapply(qtab, w = tmp[["...wgt..."]], dnn = NULL) |> 
            lapply(prop.table)
        
        # Calculate the sum of the absolute difference of the "wants" and "haves"
        delta <- sum(map2_dbl(
            .x = want,
            .y = have,
            .f = \(w, h) sum(abs(w - h))
        ))
        
        
        # In some situations the algorithm will get stuck where the deltas will
        # actually increase from the prior iteration and will fluctuate back and
        # forth. This keeps a counter of that issue and will break the loop if it
        # reaches the specified limit.
        
        if (rep.counter > 1) {
            
            if (delta > tail(delta.log, 1)) {
                
                stuck.counter <- stuck.counter + 1
                
                if (stuck.counter == max.stuck) {
                    break
                }
            }
        }
        
        
        # Keep a log of the deltas.
        delta.log <- c(delta.log, delta)
        
        if (rep.counter == max.iter) {
            break
        }
        
        
    }
    
    res <- pull(tmp, ...wgt...)
    
    
    cat_line("Delta")
    print(delta)

    cat_line("Counter")
    print(rep.counter)

    cat_line("Stuck Counter")
    print(stuck.counter)

    cat_line("Delta Log")
    print(delta.log)

    cat_line("Weights")
    print(res)

    # list(
    #     "delta" = delta.log,
    #     "counter" = rep.counter,
    #     "stuck_counter" = stuck.counter,
    #     "results" = res
    # )
}

iterake(
    x = tst,
    targets = want.tibbles
)



# Ideas -------------------------------------------------------------------

#' Here are ideas for functions to use downstream of iterake(), all of 
#' these will use the output of iterake() as its primary input with some
#' additional parameters.
#' 
#' 1. append_weights(x, weight_name = "weight"): creates a tibble of the
#'    original data plus the column of weights appended with its specified
#'    name.
#' 2. extract_weights(x, weight_name = "weight"): creates a 1-column tibble
#'    of just the created weights with the specified weight name.
#' 3. weight_stats(x): creates a 1-row tibble of the summary statistics of
#'    the created weights...
#'    - Unweighted base
#'    - Weighted base
#'    - Effective base
#'    - Quantiles, mean, standard deviation of the weights
#'    - Efficiency
#' 4. iterake_stats(x): creates a 1-row tibble of the statistics of the
#'    algorithm such as number of iterations, final convergence, number of
#'    time stuck, time it took, etc.
#' 5. summary(x, digits = 3): use this generic to nicely print out what weight_stats()
#'    and iterake_stats() generates.
#' 6. print(x, digits = 3): use this generic to print a simple print message of what
#'    took place.
#' 7. autoplot(x, what = c("hist", "compare"), binwidth = 0.1): use this 
#'    generic from ggplot2 to plot the histogram of the weights ("hist"), 
#'    or the weighted and unweighted props compared to the targets 
#'    for "compare". Note: there can/should also be an autoplot() function
#'    for the output of universe() that will plot the actual vs targets but
#'    it will just be unweighted.
#' 8. Exported functions on vectors:
#'    - unweighted_base()
#'    - weighted_base()
#'    - effective_base()
#'    - weighting_efficiency()