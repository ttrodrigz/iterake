#' Iterative raking procedure
#' 
#' This function creates row-level weights using an iterative raking algorithm based
#' on targets from a known population (established with \code{universe()}).
#' The weights are appended as a new column in the data. If \code{iterake()} 
#' converges, the weighted marginal proportions of the sample will match those
#' set in \code{universe()}. Summary statistics of the weighting procedure are 
#' presented by default.
#' 
#' @param universe Output object created with \code{universe()} function.
#' @param wgt.name Name given to column of weights to be added to data, default is "weight", optional.
#' @param max.wgt Maximum value weights can take on, default is 3, optional. The capping 
#' takes place prior to applying expansion factor (if \code{N} is set in \code{universe()}.
#' @param threshold Value specifying minimum summed difference between weighted marginal 
#' proportions of sample and universe before algorithm quits, default is 1e-10, optional.
#' @param max.iter Value capping number of iterations for the procedure, default is 50, optional.
#' @param stuck.limit Value capping the number of times summed differences between sample 
#' and universe can oscillate between increasing and decreasing, default is 5, optional.
#' @param permute Boolean indicating whether to test all possible orders of categories in \code{universe} 
#' and keep the most efficient (\code{TRUE}) or to test categories in the order listed in \code{universe} 
#' only (default, \code{FALSE}), optional. Note that when \code{TRUE} this will increase runtime by a 
#' factor of \code{(number of categories)!}.
#' @param summary Whether or not to display summary output of the procedure, default is \code{TRUE}, optional.
#' 
#' @importFrom dplyr %>% slice progress_estimated
#' @importFrom data.table data.table setkey
#' @importFrom crayon red green bold %+%
#' @importFrom glue glue
#' @importFrom tibble as_tibble
#' @importFrom scales percent
#' @importFrom arrangements permutations
#' 
#' @return Data frame with the resulting weight variable appended to it.
#' 
#' @examples
#' data(demo_data)
#' 
#' iterake(
#'     universe = universe(
#'         data = demo_data,
#'         
#'         category(
#'             name = "Sex",
#'             buckets = factor(
#'                 x = levels(demo_data[["Sex"]]),
#'                 levels = levels(demo_data[["Sex"]])
#'             ),
#'             targets = c(0.4, 0.5),
#'             sum.1 = TRUE
#'         ),
#' 
#'         category(
#'             name = "BirthYear",
#'             buckets = c(1986:1990),
#'             targets = rep(0.2, times = 5)
#'         ),
#'     
#'         category(
#'             name = "EyeColor",
#'             buckets = c("brown", "green", "blue"),
#'             targets = c(0.8, 0.1, 0.1)
#'         ),
#'     
#'         category(
#'             name = "HomeOwner",
#'             buckets = c(TRUE, FALSE),
#'             targets = c(3/4, 1/4)
#'         )
#'     )
#' )
#' 
#' @export
iterake <- function(universe, wgt.name = "weight", max.wgt = 3, 
                    threshold = 1e-10, max.iter = 50, stuck.limit = 5, 
                    permute = FALSE, summary = TRUE) {
    
    # preliminary setup + checks ----------------------------------------------
    
    if (!("universe" %in% class(universe))) {
        stop("Input to `universe` must be output created by `universe()`.")
    }

    # do stuff to to_weight - make data.table and use rn as index
    to_weight <- 
        data.table(
            universe[["data"]], 
            keep.rownames = TRUE
        ) %>%
        .[, rn := as.numeric(rn)]
    
    N <- nrow(to_weight)
    wgt_cats <- names(universe[["universe"]])
    
    # expansion factor calc    
    x.factor <- ifelse(
        universe[["N"]] == 1, 
        1, 
        universe[["N"]] / nrow(to_weight)
    )
    
    # wgt.name
    if (any(
        length(wgt.name) > 1,
        !is.character(wgt.name)
    )) {
        
        stop("Input to `wgt.name` must be a single character string.")
    }
    
    if (wgt.name %in% names(universe[["data"]])) {
        stop(glue("Column name '{wgt.name}' already exists in the data. Please supply a different name to `wgt.name`."))
    }
    
    # max.wgt
    if (any(
        length(max.wgt) > 1,
        !is.numeric(max.wgt),
        max.wgt <= 1
    )) {
        
        stop("Input to `max.wgt` must be a single numeric value greater than 1.")
        
    }
    
    # threshold
    if (any(
        length(threshold) > 1,
        !is.numeric(threshold),
        threshold < 0
    )) {
        
        stop("Input to `threshold` must be a single positive numeric value.")
    }
    
    # max.iter
    if (any(
        length(max.iter) > 1,
        !is.numeric(max.iter),
        max.iter <= 0
    )) {
        
        stop("Input to `max.iter` must be a single numeric value greater than 0.")
        
    }
    
    # stuck.limit
    if (any(
        length(stuck.limit) > 1,
        !is.numeric(stuck.limit),
        stuck.limit < 1
    )) {
        
        stop("Input to `stuck.limit` must be a single numeric value greater than or equal to 1.")
        
    }

    # permute
    if (!is.logical(permute)) {
        stop("Input to `permute` requires TRUE/FALSE.")
    }

    # summary
    if (!is.logical(summary)) {
        stop("Input to `summary` requires TRUE/FALSE.")
    }
    
    # trim things and initialize wgt = 1
    to_weight_base <- 
        # grab rn and wgt_cats
        to_weight[, c("rn", wgt_cats), with = FALSE] %>%
        # start with initial wgt of 1
        # should there be some sort of duplicate "wgt" name detection here?
        .[, wgt := 1]
    
    to_weight_keep <- NULL
    count_keep <- 0
    winner <- NULL
    
    # raking ------------------------------------------------------------------
    
    # get permutations for categories in universe
    # thoughts on using gtools for this?
    # below is from arrangements
    order_list <-
        permutations(length(wgt_cats)) %>% 
        as_tibble(.name_repair = "minimal") %>%
        set_names(paste0("X", 1:length(wgt_cats))) 
    
    # This is used in the progress bar
    n_permutes <- nrow(order_list)
    
    if (!permute) {
        # only keep first row (original order)
        order_list <- slice(order_list, 1)
    }
    
    # Build output message and initialize progress bar
    if (permute) {
        
        cat("\nTesting a total of", n_permutes, "orderings.\n")
        progbar <- progress_estimated(n_permutes)
        
    }
    
    # now loop through things here
    for (j in seq_along(order_list[[1]])) {
        
        if (permute) {
            progbar$pause(0.1)$tick()$print()
        }
        
        # set up for new outer outer loop
        order_index <- slice(order_list, j)
        
        # initialize some things
        check <- 1
        count <- 0
        stuck_count <- 0
        stuck_check <- 0
        to_weight <- to_weight_base
        
        # do the loops until the threshold is reached
        while (check > threshold) {
            
            # iteration limit check
            if (count >= max.iter) {
                uwgt_n <- nrow(to_weight)
                to_weight <- NULL
                break
            }
            
            # store previous summed difference between targets and actuals
            prev_check <- check
            # reset/initialize check value
            check <- 0
            
            # loop through each variable in universe[["universe"]] list to generate weight
            for (i in seq_along(wgt_cats)) {
                
                # data.table versions of:
                # - data being weighted, i'th weighting column as key
                DT_data   <- data.table(to_weight, key = wgt_cats[ order_index[[i]] ])
                
                # - i'th dataset (targets) of universe
                # exclude the first column, not needed
                DT_design <- data.table(universe[["universe"]][[ order_index[[i]] ]])[, -1]
                
                # start with original data.table-ized object
                DT_merge <- 
                    
                    # original everything is getting merged to
                    DT_data[
                        
                        # DT object to be merged with the original
                        # ideal becuase it uses the same key for merging
                        # calcs the "haves" by the i'th weighting category
                        DT_data[, .(act_prop = sum(wgt) / N), by = c(wgt_cats[ order_index[[i]] ])] %>%
                            
                            # merge haves with wants
                            .[DT_design] %>%
                            
                            # calculate wants over haves by the i'th weighting category
                            .[, .(wgt_temp = ifelse(act_prop == 0, 0, targ_prop / act_prop)), 
                              by = c(wgt_cats[ order_index[[i]] ])]
                        ]
                
                # creates the weight factor, max out at the weight limit
                to_weight <- 
                    DT_merge[, wgt := ifelse(wgt * wgt_temp > max.wgt, max.wgt, wgt * wgt_temp)] %>%
                    
                    # temporary weight no longer needed
                    .[, wgt_temp := NULL]
                
            }
            
            # loop through each to calculate discrepencies
            for (i in seq_along(wgt_cats)) {
                
                DT_data   <- data.table(to_weight, key = wgt_cats[ order_index[[i]] ])
                
                # exclude the first and fourth column, not needed
                DT_design <- data.table(universe[["universe"]][[ order_index[[i]] ]])[, c(-1, -4)]
                
                sum_diffs <- 
                    # calcs the "haves" by the i'th weighting category
                    DT_data[, .(act_prop = sum(wgt) / N), by = c(wgt_cats[ order_index[[i]] ])] %>%
                    
                    # merge haves with wants
                    .[DT_design] %>%
                    
                    # calculate sum of abs(prop diffs)
                    .[, .(sum = sum(abs(targ_prop - act_prop)))] %>%
                    
                    # just return this sum
                    .[, sum]
                
                # check is the sum of whatever check already is + sum_diffs
                check <- check + sum_diffs
            }
            
            # check to see if summed difference increased from last iteration
            if (prev_check < check) {
                # if so, increment stuck counter
                stuck_count <- stuck_count + 1
                
                # ...and if stuck counter hits a threshold, force check to equal threshold to stop while loop
                if (stuck_count > stuck.limit) {
                    stuck_check <- check
                    check <- threshold
                }
            }
            
            # increment loop count
            count <- count + 1
        }
        
        # now that a run is complete, need to compare (if permute)
        # then set up what to keep and move on
        # how to decide what to keep? Perhaps effN?

        # only do something if to_weight isn't null
        if (!is.null(to_weight)) {
            
            # if _keep is null, this auto-wins and should be kept
            if (is.null(to_weight_keep)) {
                
                # data
                to_weight_keep <- to_weight
                
                # items for summary output
                count_keep <- count
                winner     <- order_index
                
            } else {
                
                # grab the before and after weights
                wgt_old <- to_weight_keep$wgt
                wgt_new <- to_weight$wgt
                
                # calculate before and after effN
                effN_old <- (sum(wgt_old) ^ 2) / sum(wgt_old ^ 2)
                effN_new <- (sum(wgt_new) ^ 2) / sum(wgt_new ^ 2)
                
                # make comparison on somewhat rounded values... based on threshold?
                # dig_base <- match(TRUE, round(threshold, 1:50) == threshold)
                
                # or just decide - this is N, how much of a fractional person
                # should count as "better"? A hundredth? A thousandth?
                # going with ten-thousandth for now...
                effN_old <- round(effN_old, 4)
                effN_new <- round(effN_new, 4)
                
                # if new is better, keep it
                if (effN_new > effN_old) {
                    
                    # data
                    to_weight_keep <- to_weight
                    
                    # items for summary output
                    count_keep <- count
                    winner <- order_index
                }
            }
            
        }

    }
    
    # return ------------------------------------------------------------------
    
    # rename the _keep items first
    to_weight <- to_weight_keep
    count <- count_keep
    
    if (is.null(to_weight)) {
        
        out_bad <- red $ bold
        out <- NULL
        
        title1 <- 'iterake summary'
        num_dashes <- nchar(title1) + 4
        rem_dashes <- 80 - num_dashes
        
        cat('\n-- ' %+% 
                bold(title1) %+% 
                ' ' %+%
                paste(rep('-', times = rem_dashes), collapse = "") %+%
                '\n')
        cat(' Convergence: ' %+% red('Failed') %+% '\n')
        cat('  Iterations: ' %+% paste0(max.iter) %+% '\n\n')
        cat('Unweighted N: ' %+% paste0(uwgt_n) %+% '\n')
        cat(' Effective N: ' %+% '--\n')
        cat('  Weighted N: ' %+% '--\n')
        cat('  Efficiency: ' %+% '--\n')
        cat('        Loss: ' %+% '--\n')
        
    } else {
        
        # clean df to output
        out <-
            # first merge orig and new data...
            
            # get orig dataframe, make it a data.table with rn key/sorted as.numeric
            setkey(
                data.table(universe$data, keep.rownames = TRUE) %>%
                    .[, rn := as.numeric(rn)]
                , "rn"
            ) %>%
            
            # just grab rn and wgt from to_weight, keyed and sorted
            .[setkey(
                to_weight[, c("rn", "wgt")], 
                "rn"
            )] %>%
            
            # combine x.factor
            .[, wgt := wgt * x.factor] %>%
            
            # scrap no-longer-needed rn
            .[, rn := NULL] %>%
            
            as_tibble()
        
        # return output message?
        if (summary) {

            # calculate stats
            wgt    <- out$wgt
            uwgt_n <- nrow(out)
            wgt_n  <- sum(wgt)
            eff_n  <- (sum(wgt) ^ 2) / sum(wgt ^ 2)
            loss   <- round((uwgt_n / eff_n) - 1, 3)
            efficiency <- (eff_n / uwgt_n)
            
            
            # output message
            out_good <- green $ bold
            title1 <- 'iterake summary'
            num_dashes <- nchar(title1) + 4
            rem_dashes <- 80 - num_dashes
            
            cat('\n-- ' %+% 
                    bold(title1) %+% 
                    ' ' %+%
                    paste(rep('-', times = rem_dashes), collapse = "") %+%
                    '\n')
            if (stuck_check > 0) {
                cat(' Convergence: ' %+% green('Success') %+% '\n')
            } else {
                cat(' Convergence: ' %+% green('Success') %+% '\n')    
            }
            cat('  Iterations: ' %+% paste0(count) %+% '\n\n')
            cat('Unweighted N: ' %+% paste0(sprintf("%.2f", uwgt_n)) %+% '\n')
            cat(' Effective N: ' %+% paste0(round(eff_n,  2)) %+% '\n')
            cat('  Weighted N: ' %+% paste0(sprintf("%.2f", wgt_n)) %+% '\n')
            cat('  Efficiency: ' %+% paste0(percent(round(efficiency, 4))) %+% '\n')
            cat('        Loss: ' %+% paste0(loss) %+% '\n')
            
            if (permute) {
                cat('       Order: ' %+% paste(winner, collapse = " ") %+% '\n\n')    
            } else {
                cat('\n')
            }
            
            if (stuck_check > 0) {
                cat(' NOTE: ' %+% 
                        paste0('Threshold met, stopped at difference of ' %+% 
                                   paste0(
                                       formatC(stuck_check, 
                                               format = "e", 
                                               digits = 3))) %+% 
                        ' between weighted sample and universe.\n\n')
            }
        }
        
        # apply new weight name
        names(out)[names(out) == 'wgt'] <- wgt.name

        return(out)
        
    }    
}

utils::globalVariables(c(".", "act_prop", "wgt_temp", "prop_diff", "rn"))