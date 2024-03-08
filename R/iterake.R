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
    stk.delta.keep  <- 0
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
    
    # Unpack the wants for faster calculation of the deltas
    want.tibbles <- universe$categories
    
    for (i in seq_along(want.tibbles)) {
        
        # Replace the targets with the targets_sum1 when necessary
        sum1 <- !is.null(want.tibbles[[i]]$targets_sum1)
        
        if (sum1) {
            want.tibbles[[i]]$targets <- want.tibbles[[i]]$targets_sum1
            want.tibbles[[i]]$targets_sum1 <- NULL
        }
        
        want.tibbles[[i]] <- enframe(
            x = setNames(want.tibbles[[i]]$targets, want.tibbles[[i]]$groups),
            name = names(want.tibbles[i]),
            value = "want"
        )
        
    }
    
    # get this prepped since itll be the same regardless of order
    want <- map(want.tibbles, \(x) pull(x, want, name = 1))
    
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
        stuck.delta   <- 0
        delta.log     <- numeric()
        tmp           <- tmp.base
        
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
                        
                        stuck.delta <- delta
                        
                        break
                    }
                }
            }
            
            # Keep a log of the deltas.
            delta.log <- c(delta.log, delta)
            
            if (rep.counter == max.iter) {break}
            
        }
        
        # now evaluate the run against previous if others
        # if tmp.keep is null, this auto-wins and should be kept
        if (is.null(tmp.keep)) {
            
            # data
            tmp.keep <- tmp
            
            # items for summary output
            rep.ct.keep    <- rep.counter
            stk.ct.keep    <- stuck.counter
            stk.delta.keep <- stuck.delta
            hit.max.iters  <- rep.counter == max.iter
            winner         <- wt.cats.tmp
            delta.keep     <- delta
            delta.log.keep <- delta.log
            
            
        } else {
            
            # grab the before and after weights
            wgt.old <- pull(tmp.keep, ...wgt...)
            wgt.new <- pull(tmp, ...wgt...)
            
            # calculate before and after effN
            effN.old <- sample_size(1, wgt.old, type = "e")
            effN.new <- sample_size(1, wgt.new, type = "e")
            
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
                stk.delta.keep <- stuck.delta
                hit.max.iters  <- rep.counter == max.iter
                winner         <- wt.cats.tmp
                delta.keep     <- delta
                delta.log.keep <- delta.log
                
            }
            
        }
        
    }
    
    if (permute) {
        cli_progress_done()
    }
    
    # max iteration
    if (hit.max.iters) {
        status <- "max iter"
    # max stuck
    } else if (stk.delta.keep > 0) {
        status <- "max stuck"
    # should be good
    } else {
        status <- "success"
    }
    
    res <- pull(tmp.keep, ...wgt...)
    stats <- weight_stats(res)

    # remember downstream to check status
    out <- list(
        "universe" = universe,
        "control" = control,
        "status" = status, 
        "delta_log" = delta.log.keep,
        "counter" = rep.ct.keep,
        "stuck_counter" = stk.ct.keep,
        "stuck_delta" = stuck.delta,
        "cat_order" = winner,
        "delta" = delta.keep,
        "permute" = permute,
        "results" = res,
        "stats" = stats
    )
    
    class(out) <- c(class(out), "iterake")
    
    out
}


#' Print method for iterake objects.
#' 
#' @method print iterake
#' 
#' @param x An `iterake` object.
#' @param ... Not currently used.
#' 
#' @importFrom cli col_green col_red cat_line cat_bullet cat_rule
#' @importFrom dplyr case_when
#' @importFrom scales percent number
#'
#' @export
print.iterake <- function(x, ...) {
    
    # Extract stuff --
    thresh    <- x$control$threshold
    delta     <- x$delta
    dt.diff   <- delta - thresh
    status    <- x$status
    iter      <- x$counter
    uss       <- x$stats$uwgt_n
    wss       <- x$stats$wgt_n
    ess       <- x$stats$eff_n
    eff       <- x$stats$efficiency
    loss      <- x$stats$loss
    avg       <- x$stats$mean
    cat.order <- x$cat_order
    
    # Start prettying the text --
    converged <- status == "success"
    converged_text <- ifelse(converged, "Converged", "Failed to converge")
    converged_text <- ifelse(converged, col_green(converged_text), col_red(converged_text))
    
    if (!converged) {
        reason <- case_when(
            status == "max iter"  ~ "Reached maximum iterations before convergence threshold was met.",
            status == "max stuck" ~ "Reached maximum stuck limit by failing to consistently improve.",
            .default = NA
        )
    }
    

    cat_rule("iterake")
    cat_line()
    cat_line("      Status: ", converged_text)
    cat_line("  Iterations: ", comma(iter, accuracy = 1))
    cat_line("Unweighted N: ", comma(uss, accuracy = 0.1))
    cat_line("  Weighted N: ", comma(wss, accuracy = 0.1))
    cat_line(" Effective N: ", comma(ess, accuracy = 0.1))
    cat_line("  Efficiency: ", percent(eff, 0.1))
    cat_line("        Loss: ", number(loss, 0.001))
    cat_line("        Mean: ", number(avg, 0.001))
    cat_line("       Order: ", paste(cat.order, collapse = ", "))
    
    if (!converged) {
        cat_line()
        cat_bullet(
            reason, 
            bullet = "info", 
            bullet_col = "cyan"
        )
        cat_bullet(
            "Stopped at a difference from threshold of approximately ",
            number(dt.diff, 0.0000001),
            ".",
            bullet = "info", 
            bullet_col = "cyan"
        )
    }

}


utils::globalVariables(c("...wgt...", "eff_n", "uwgt_n", "wgt_fct"))