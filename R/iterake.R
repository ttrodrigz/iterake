#' Iterative raking procedure
#' 
#' This function creates case weights using an iterative raking algorithm based
#' on targets from a known population (established with \code{universe()}).
#' The weights are appended as a new column in the data. If \code{iterake()} 
#' converges, the weighted marginal proportions of the sample will match those
#' set in \code{universe()}. Summary statistics of the weighting procedure are 
#' presented by default.
#' 
#' @param universe Output object created with \code{universe()} function.
#' @param permute Boolean indicating whether to test all possible orders of categories in \code{universe} 
#' and keep the most efficient (\code{TRUE}) or to test categories in the order listed in \code{universe} 
#' only (default, \code{FALSE}), optional. Note that when \code{TRUE} this will increase runtime by a 
#' factor of \code{(number of categories)!}.
#' @param control Output object created with \code{control_iterake()} function.
#' 
#' @importFrom arrangements permutations
#' @importFrom cli cli_progress_bar cli_progress_done cli_progress_update
#' @importFrom collapse fgroup_by fmax fmin fmutate fselect fsum fungroup join qtab replace_Inf ss
#' @importFrom dplyr pull
#' @importFrom purrr map map2_dbl set_names
#' @importFrom rlang abort
#' @importFrom stats setNames
#' @importFrom tibble as_tibble enframe tibble
#' @importFrom utils tail
#' 
#' @return A \code{list} that includes details of the run as well as the generated weights.
#' 
#' @export
iterake <- function(
        universe,
        permute = FALSE,
        control = control_iterake()
) {
    
    
    its.a.uni <- inherits(universe, "universe")
    
    if (!its.a.uni) {
        abort("Input to `universe` must be the output of `universe()`.")
    }
    
    its.a.con <- inherits(control, "control")
    
    if (!its.a.con) {
        abort("Input to `control` must be the output of `control_iterake()`.")
    }
    
    # Initialize things
    tmp.base        <- fmutate(universe$data$data, ...wgt... = 1)
    wt.cats         <- names(universe$categories)
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
    if (!permute) {
        
        # only keep first row (original order)
        order.list <- ss(order.list, 1)
        
    } else {
        
        # speed means rarely see this, but this can be removed
        options(cli.progress_show_after = 0)
        # set up a progress bar of sorts to show permutation progress
        cli_progress_bar(
            "Permutations", 
            total = n.permutes, 
            type = "tasks", 
            clear = FALSE
        )
        
    }
    
    # Stuff from the control
    # error checking for these is already handled by the control_iterake function
    threshold  <- control$threshold
    max.weight <- control$max_weight
    max.iter   <- control$max_iter
    max.stuck  <- control$max_stuck
    
    for (j in seq_along(order.list[[1]])) {
        
        # move the bar along if relevant
        if (permute) {
            cli_progress_update()
        }
        
        # set up for new outer outer loop
        wt.cats.tmp <- unlist(ss(order.list, j), use.names = FALSE)
        
        # Initialize some more things
        rep.counter   <- 0
        stuck.counter <- 0
        delta.log     <- numeric()
        tmp           <- tmp.base
        
        # Unpack the wants for faster calculation of the deltas
        want.tibbles <- universe$categories
        
        for (i in seq_along(want.tibbles)) {
            
            want.tibbles[[i]] <- enframe(
                x = setNames(want.tibbles[[i]]$targets, want.tibbles[[i]]$groups),
                name = names(want.tibbles[i]),
                value = "want"
            )
            
        }
        
        want <- map(want.tibbles, \(x) pull(x, want, name = 1))
        
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
            effN.old <- effective_ss(1, wgt.old)
            effN.new <- effective_ss(1, wgt.new)
            
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
    
    if (permute) {
        cli_progress_done()
    }
    
    if (is.null(tmp.keep)) {
        # summary is null to indicate failure so the print method knows which way to go
        res <- numeric()
        summary <- NULL
        
    } else {
        
        # calculate stats
        res     <- pull(tmp.keep, ...wgt...)
        summary <- tibble(
            "uwgt_n" = unweighted_ss(res),
            "wgt_n" = weighted_ss(1, res),
            "eff_n" = effective_ss(1, res),
            "loss" = (unweighted_ss(res) / effective_ss(1, res)) - 1,
            "efficiency" = (effective_ss(1, res) / unweighted_ss(res)),
            "min" = fmin(res),
            "max" = fmax(res)
        )
        
    }

    # remember downstream summary is null if it doesnt work
    out <- list(
        "universe" = universe,
        "control" = control,
        "delta_log" = delta.log.keep,
        "counter" = rep.ct.keep,
        "stuck_counter" = stk.ct.keep,
        "stuck_delta" = stuck.delta,
        "winner" = winner,
        "delta" = delta.keep,
        "permute" = permute,
        "results" = res,
        "summary" = summary
    )
    
    class(out) <- c(class(out), "iterake")
    
    out
}

#' Print method for iterake objects.
#' 
#' @method print iterake
#' 
#' @param x An `iterake` object.
#' @param digits Number of digits for printing proportions, default is 3.
#' @param ... Not currently used.
#' 
#' @importFrom crayon red green bold %+%
#' @importFrom scales percent
#'
#' @export
print.iterake <- function(x, digits = 3, ...) {

    success <- !is.null(x$summary)
    
    if (success) {
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
        cat('  Iterations: ' %+% paste0(x$counter) %+% '\n\n')
        cat('Unweighted N: ' %+% paste0(sprintf("%.2f", x$summary$uwgt_n)) %+% '\n')
        cat(' Effective N: ' %+% paste0(round(x$summary$eff_n,  2)) %+% '\n')
        cat('  Weighted N: ' %+% paste0(sprintf("%.2f", x$summary$wgt_n)) %+% '\n')
        cat('  Efficiency: ' %+% paste0(percent(round(x$summary$efficiency, 4))) %+% '\n')
        cat('        Loss: ' %+% paste0(round(x$summary$loss, digits)) %+% '\n')
        cat('       Order: ' %+% paste(x$winner, collapse = " ") %+% '\n\n')
        
        if (x$stuck_delta > 0) {
            cat(' NOTE: ' %+%
                    paste0('Threshold met, stopped at difference of ' %+%
                               paste0(
                                   formatC(x$stuck_delta,
                                           format = "e",
                                           digits = 3))) %+%
                    ' between weighted sample and universe.\n\n')
        }
        
    } else {
        # FAIL METHOD
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
        cat('  Iterations: ' %+% paste0(x$control$max_iter) %+% '\n\n')
        cat('Unweighted N: ' %+% paste0(nrow(x$universe$data$data)) %+% '\n')
        cat(' Effective N: ' %+% '--\n')
        cat('  Weighted N: ' %+% '--\n')
        cat('  Efficiency: ' %+% '--\n')
        cat('        Loss: ' %+% '--\n')
    }
    
    # this area should be moved to a custom print function
    # cat_line("Delta")
    # print(delta.keep)
    # 
    # cat_line("Counter")
    # print(rep.ct.keep)
    # 
    # cat_line("Stuck Counter")
    # print(stk.ct.keep)
    # 
    # cat_line("Delta Log")
    # print(delta.log.keep)
    # 
    # cat_line("Weights")
    # print(res)
}

utils::globalVariables(c("...wgt...", "wgt_fct"))