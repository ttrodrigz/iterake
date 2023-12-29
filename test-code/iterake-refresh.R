
# Overview ----------------------------------------------------------------

# This is my attempt at a syntactically simpler and hopefully faster
# version of the iterake function.

# This does not do any error checking or exception handling and does not
# incorporate the permutation version. But, I think it works!

# Packages ----------------------------------------------------------------

library(tidyverse)
library(collapse)
library(cli)
library(arrangements)

library(progress)
library(glue)
library(crayon)
library(scales)

library(haven)
library(iterake)


# Create some data --------------------------------------------------------

# ss <- 100
# 
# set.seed(101)
# tst <- tibble(
#     x = factor(sample(letters[1:4], size = ss, replace = TRUE)),
#     y = factor(sample(letters[5:7], size = ss, replace = TRUE)),
#     z = factor(sample(letters[8:10], size = ss, replace = TRUE))
# )
# 
# 
# # "Wants" -----------------------------------------------------------------
# 
# want <- list(
#     x = c("a" = 1/4, "b" = 1/4, "c" = 1/4, "d" = 1/4),
#     y = c("e" = 0.3, "f" = 0.3, "g" = 0.4),
#     z = c("h" = 0.25, "i" = 0.44, "j" = 0.31)
# )
# 
# want.tibbles <- want
# 
# for (i in seq_along(want)) {
#     
#     want.tibbles[[i]] <- enframe(
#         x = want.tibbles[[i]],
#         name = names(want.tibbles[i]),
#         value = "want"
#     )
#     
# }

tst <- 
    read_sav("test-code/dummydata.sav") %>%
    # this isn't needed if only wave of interest is present anyway
    filter(wave == 32) %>%
    mutate(elig = as.numeric(elig),
           new_age = case_when(age < 65 ~ 1,
                               age >= 65 ~ 2,
                               TRUE ~ NA_real_ ),
           new_juris = case_when(juris %in% c(1, 2, 3, 4, 6, 9) ~ 1,
                                 juris %in% c(7, 8) ~ 2,
                                 juris == 5 ~ 3,
                                 TRUE ~ NA_real_ )
    )

want <- list(
    elig = c("1" = 0.524, "2" = 0.476),
    new_age = c("1" = 0.468, "2" = 0.532),
    new_juris = c("1" = 0.141, "2" = 0.508, "3" = 0.351)
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

# part of the old way
mod <- universe(data = tst,
                
                category(name = "elig",
                         buckets = c(1, 2),
                         targets = c(0.524, 0.476)),
                
                category(name = "new_age",
                         buckets = c(1, 2),
                         targets = c(0.468, 0.532)),
                
                category(name = "new_juris",
                         buckets = c(1, 2, 3),
                         targets = c(0.141, 0.508, 0.351))
                
)


# Do stuff ----------------------------------------------------------------

#' TODO DEVON: permute, missing value handling, make sure new code does the same
#' thing as the old code.

# calling it iterake2 to test it against iterake
iterake2 <- function(
        x,
        targets,
        permute = FALSE,
        control = control_iterake()
) {

    # Initialize things
    tmp.base        <- fmutate(x, ...wgt... = 1)
    wt.cats         <- names(targets)
    tmp.keep        <- NULL
    winner          <- NULL
    rep.ct.keep     <- 0
    stk.ct.keep     <- 0
    stuck.delta     <- 0
    delta.keep      <- 0
    delta.log.keep  <- numeric()
    
    # set up permutation list
    order.list <-
        permutations(wt.cats) |>
        as_tibble(.name_repair = "minimal") |>
        set_names(paste0("X", 1:length(wt.cats)))
    
    # This is used in the progress bar - not sure if this is still wanted
    n.permutes <- nrow(order.list)
    
    # only look at the first row (original order) if not permuting
    # all of the progress bar stuff related to permuting as well
    # as the cat outputs can all be scrapped/replaced, just tossed
    # it all in for now
    if (!permute) {
        
        # only keep first row (original order)
        order.list <- ss(order.list, 1)
        
    } else {

        cat("\nTesting a total of", n.permutes, "orderings.\n")
        
        progbar <- progress_bar$new(
            format = "[:bar] :percent eta: :eta",
            clear = FALSE,
            total = n.permutes
        )
        
    }
    
    # Stuff from the control
    # error checking for these could probably be handled by the 
    # control_iterake function
    threshold  <- control$threshold
    max.weight <- control$max_weight
    max.iter   <- control$max_iter
    max.stuck  <- control$max_stuck
    
    for (j in seq_along(order.list[[1]])) {
        
        if (permute) {
            
            progbar$tick()
        }

        # set up for new outer outer loop
        wt.cats.tmp <- unlist(ss(order.list, j), use.names = FALSE)
        
        # Initialize some more things
        rep.counter   <- 0
        stuck.counter <- 0
        delta.log     <- numeric()
        tmp           <- tmp.base
        
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
            for (i in seq_along(wt.cats.tmp)) {

                cat.now <- wt.cats.tmp[[i]]
                
                want.now <- want.tibbles[[cat.now]]

                tmp <-
                    
                    tmp |>
                    
                    # sum of the weights by group to get the "haves"
                    # `fsum()` is faster than `sum()` especially when used
                    # in the context of `fgroup_by()` and `fmutate()`
                    fgroup_by(cat.now) |> 
                    fmutate(have = fsum(...wgt...) / nrow(tmp.base)) |> 
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
                    # `replace_Inf()` is faster and uses less memory than
                    # using an `ifelse()`
                    fmutate(wgt_fct = replace_Inf(want / have, value = 0)) |> 
                    
                    # create the new weight by multiplying the old by the
                    # newly created weighting factor
                    # `pmin()` faster and uses less memory than `ifelse()`
                    fmutate(...wgt... = pmin(max.weight, ...wgt... * wgt_fct)) |> 
                    
                    # grab columns needed
                    fselect(wt.cats.tmp, "...wgt...")
                
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
                        # break may cause issues w/in outer permute loop
                        # unless does it only break one level out? meaning
                        # it breaks the do while and not the outer for loop?
                        stuck.delta <- delta
                        
                        break
                    }
                }
            }
            
            # Keep a log of the deltas.
            delta.log <- c(delta.log, delta)
            
            if (rep.counter == max.iter) {
                # same issue/thoughts as above
                break
            }
            
        }

        # now evaluate the run against previous if others
        # if tmp.keep is null, this auto-wins and should be kept
        if (is.null(tmp.keep)) {
            
            # data
            tmp.keep <- tmp
            
            # items for summary output
            rep.ct.keep    <- rep.counter
            stk.ct.keep    <- stuck.counter
            winner         <- wt.cats.tmp
            delta.keep     <- delta
            delta.log.keep <- delta.log
            
        } else {
            
            # grab the before and after weights
            wgt.old <- pull(tmp.keep, ...wgt...)
            wgt.new <- pull(tmp, ...wgt...)
            
            # calculate before and after effN
            effN.old <- (sum(wgt.old) ^ 2) / sum(wgt.old ^ 2)
            effN.new <- (sum(wgt.new) ^ 2) / sum(wgt.new ^ 2)
            
            # This is N, how much of a fractional person
            # should count as "better"? A hundredth? A thousandth?
            # going with ten-thousandth for now...
            effN.old <- round(effN.old, 4)
            effN.new <- round(effN.new, 4)
            
            # if new is better, keep it
            if (effN.new > effN.old) {
                
                # data
                tmp.keep <- tmp
                
                # items for summary output
                rep.ct.keep    <- rep.counter
                stk.ct.keep    <- stuck.counter
                winner         <- wt.cats.tmp
                delta.keep     <- delta
                delta.log.keep <- delta.log
                
            }
            
        }
        
    }
    
    if (is.null(tmp.keep)) {
        
        out.bad <- red $ bold
        
        title1 <- 'iterake summary'
        num.dashes <- nchar(title1) + 4
        rem.dashes <- 80 - num.dashes
        
        cat('\n-- ' %+% 
                bold(title1) %+% 
                ' ' %+%
                paste(rep('-', times = rem.dashes), collapse = "") %+%
                '\n')
        cat(' Convergence: ' %+% red('Failed') %+% '\n')
        cat('  Iterations: ' %+% paste0(max.iter) %+% '\n\n')
        cat('Unweighted N: ' %+% paste0(nrow(tmp.base)) %+% '\n')
        cat(' Effective N: ' %+% '--\n')
        cat('  Weighted N: ' %+% '--\n')
        cat('  Efficiency: ' %+% '--\n')
        cat('        Loss: ' %+% '--\n')
        
    } else {
        
        # calculate stats
        wgt        <- pull(tmp.keep, ...wgt...)
        uwgt.n     <- nrow(tmp.keep)
        wgt.n      <- sum(wgt)
        eff.n      <- (sum(wgt) ^ 2) / sum(wgt ^ 2)
        loss       <- round((uwgt.n / eff.n) - 1, 3)
        efficiency <- (eff.n / uwgt.n)
        
        # output message
        out.good <- green $ bold
        title1 <- 'iterake summary'
        num.dashes <- nchar(title1) + 4
        rem.dashes <- 80 - num.dashes
        
        cat('\n-- ' %+% 
                bold(title1) %+% 
                ' ' %+%
                paste(rep('-', times = rem.dashes), collapse = "") %+%
                '\n')
        
        cat(' Convergence: ' %+% green('Success') %+% '\n')
        cat('  Iterations: ' %+% paste0(rep.ct.keep) %+% '\n\n')
        cat('Unweighted N: ' %+% paste0(sprintf("%.2f", uwgt.n)) %+% '\n')
        cat(' Effective N: ' %+% paste0(round(eff.n,  2)) %+% '\n')
        cat('  Weighted N: ' %+% paste0(sprintf("%.2f", wgt.n)) %+% '\n')
        cat('  Efficiency: ' %+% paste0(percent(round(efficiency, 4))) %+% '\n')
        cat('        Loss: ' %+% paste0(loss) %+% '\n')
        
        if (permute) {
            cat('       Order: ' %+% paste(winner, collapse = " ") %+% '\n\n')    
        } else {
            cat('\n')
        }
            
        if (stuck.delta > 0) {
            cat(' NOTE: ' %+% 
                    paste0('Threshold met, stopped at difference of ' %+% 
                               paste0(
                                   formatC(stuck.delta, 
                                           format = "e", 
                                           digits = 3))) %+% 
                    ' between weighted sample and universe.\n\n')
        }
    }
    
    res <- pull(tmp.keep, ...wgt...)
    
    cat_line("Delta")
    print(delta.keep)

    cat_line("Counter")
    print(rep.ct.keep)

    cat_line("Stuck Counter")
    print(stk.ct.keep)

    cat_line("Delta Log")
    print(delta.log.keep)

    cat_line("Weights")
    print(res)

    # list(
    #     "delta" = delta.log,
    #     "counter" = rep.counter,
    #     "stuck_counter" = stuck.counter,
    #     "results" = res
    # )
}

# iterake(
#     x = tst,
#     targets = want.tibbles,
#     permute = TRUE
# )


bench::mark(
    "v1" = iterake(mod, threshold = 1e-15, permute = TRUE),
    "v2" = iterake2(x = tst, targets = want.tibbles, permute = TRUE),
    check = FALSE
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