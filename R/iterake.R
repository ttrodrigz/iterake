#' Iterative raking procedure
#' 
#' This function creates row-level weights using an iterative raking algorithm based
#' on targets from a known population (established with \code{universe()}).
#' The weights are appended as a new column in the data. If \code{iterake()} 
#' converges, the weighted marginal proportions of the sample will match those
#' set in \code{universe()}. Summary statistics of the weighting procedure are 
#' presented by default.
#' 
#' @param df Data frame containing data where weights are desired.
#' @param universe Output object created with \code{universe()} function.
#' @param wgt.name Name given to column of weights to be added to data, optional.
#' @param max.wgt Maximum value weights can take on, optional.
#' @param threshold Value specifying minimum summed difference between weighted 
#' marginal proportions of sample and universe before algorithm quits, optional.
#' @param max.iter Value capping number of iterations for the procedure, optional.
#' @param stuck.limit Value capping the number of times summed differences between 
#' sample and universe can oscillate between increasing and decreasing, optional.
#' @param summary Whether or not to display summary output of the procedure, optional.
#' 
#' @importFrom dplyr pull enquo mutate select %>%
#' @importFrom rlang !!
#' @importFrom data.table data.table setkey
#' @importFrom crayon red yellow green bold %+%
#' @importFrom tibble as_tibble
#' @importFrom scales percent
#' 
#' @return Data frame with the resulting weight variable appended to it.
#' 
#' @examples 
#' data(weight_me)
#' 
#' iterake(
#'     df = weight_me,
#'     universe = universe(
#'         df = weight_me,
#' 
#'         category(
#'             name = "seeds",
#'             buckets = c("Tornado", "Bird", "Earthquake"),
#'             targets = c(0.300, 0.360, 0.340)),
#' 
#'         category(
#'             name = "costume",
#'             buckets = c("Bat Man", "Cactus"),
#'             targets = c(0.500, 0.500)),
#' 
#'         category(
#'             name = "transport",
#'             buckets = c("Rocket Cart", "Jet Propelled Skis", "Jet Propelled Unicycle"),
#'             targets = c(0.400, 0.450, 0.150))
#' 
#'     )
#' )
#' 
#' @export
iterake <- function(df, universe, wgt.name = "weight", 
                    max.wgt = 3, threshold = 1e-20, max.iter = 50, 
                    stuck.limit = 5, summary = TRUE) {
    
    # step 1) setup + error checking ----
    if (!("universe" %in% class(universe))) {
        stop("Input to `universe` must be output created by `universe()`.")
    }
    
    # set up useful objects off the bat
    
    # do stuff to to_weight - make data.table and use rn as index
    to_weight <- data.table(df, keep.rownames = TRUE)[, rn := as.numeric(rn)]
    N <- nrow(to_weight)
    wgt_cats <- universe[["category"]]
    
    # make sure data frame is supplied
    if (!is.data.frame(df)) {
        stop("Input to `df` must be a data frame.")
    }
    
    # make sure all wgt_cats are found in data
    wgt_cats_not_in_data <- wgt_cats[!wgt_cats %in% names(df)]
    
    if (length(wgt_cats_not_in_data) > 0) {
        stop(paste("The following weight category names are not found in your data:",
                   paste(wgt_cats_not_in_data, collapse = ", ")),
             sep = "\n")
    }
    
    # wgt.name
    if (any(
        length(wgt.name) > 1,
        !is.character(wgt.name)
    )) {
        
        stop("Input to `wgt.name` must be a single character string.")
    }
    
    if (wgt.name %in% names(df)) {
        stop(paste0("Column name '", wgt.name, "' already exists in the data. Please supply a different name to `wgt.name`."))
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
    
    # summary
    if (!is.logical(summary)) {
        stop("Input to `summary` requires TRUE/FALSE.")
    }
    
    
    
    # trim things and initialize wgt = 1
    to_weight <- to_weight[, 
                           
                           # grab rn and wgt_cats
                           c("rn", wgt_cats), with = FALSE][
                               
                               # start with initial wgt of 1
                               # should there be some sort of duplicate "wgt" name detection here?
                               , wgt := 1]
    
    # data is now ready for weighting !!
    
    # step 2) do the raking ----
    
    # initialize some things
    check <- 1
    count <- 0
    stuck_count <- 0
    stuck_check <- 0
    
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
        
        # loop through each variable in universe$category to generate weight
        for (i in seq_along(wgt_cats)) {
            
            # data.table versions of:
            # - data being weighted, i'th weighting column as key
            # - i'th dataset (targets) of universe
            DT_data   <- data.table(to_weight, key = wgt_cats[i])
            DT_design <- data.table(universe[["data"]][[i]])
            
            # start with original data.table-ized object
            DT_merge <- 
                
                # original everything is getting merged to
                DT_data[
                    
                    # DT object to be merged with the original
                    # ideal becuase it uses the same key for merging
                    DT_data[, 
                            # calcs the "haves" by the i'th weighting category
                            .(act_prop = sum(wgt) / N),
                            by = c(wgt_cats[i])
                            
                            # merge haves with wants
                            ][DT_design
                              
                              ][, 
                                # calculate wants over haves by the i'th weighting category
                                .(wgt_temp = ifelse(act_prop == 0, 0, targ_prop / act_prop)), 
                                by = c(wgt_cats[i])
                                ]
                    ]
            
            # creates the weight factor, max out at the weight limit
            to_weight <- DT_merge[, wgt := ifelse(wgt * wgt_temp > max.wgt, max.wgt, wgt * wgt_temp)][
                # temporary weight no longer needed
                , wgt_temp := NULL]
            
        }
        
        # loop through each to calculate discrepencies
        for (i in seq_along(wgt_cats)) {
            
            DT_design <- data.table(universe[["data"]][[i]])
            DT_data   <- data.table(to_weight, key = wgt_cats[i])
            
            sum_diffs <- DT_data[, 
                                 # calcs the "haves" by the i'th weighting category
                                 .(act_prop = sum(wgt) / N),
                                 by = c(wgt_cats[i])
                                 
                                 # merge haves with wants
                                 ][DT_design
                                   
                                   ][,
                                     # calculate sum of abs(prop diffs)
                                     .(sum = sum(abs(targ_prop - act_prop)))
                                     
                                     ][,
                                       
                                       # just return this sum
                                       sum]
            
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
    
    # step 3) what to return ----
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
        
        # expansion factor calc
        if (!missing(N)) {
            x.factor <- N / nrow(df)
        } else {
            x.factor <- 1
        }
        
        out <-
            # first merge orig and new data...
            
            # get orig dataframe, make it a data.table with rn key/sorted as.numeric
            setkey(data.table(df, keep.rownames = TRUE)[, rn := as.numeric(rn)], "rn")[
                
                # just grab rn and wgt from to_weight, keyed and sorted
                setkey(to_weight[, c("rn", "wgt")], "rn")][
                    
                    # combine x.factor
                    , wgt := wgt * x.factor][
                        
                        # scrap no-longer-needed rn
                        , rn := NULL] %>%
            
            as_tibble()
        
        # calculate stats
        wgt <- out$wgt
        uwgt_n <- nrow(out)
        wgt_n <- sum(wgt)
        eff_n <- (sum(wgt) ^ 2) / sum(wgt ^ 2)
        loss <- round((uwgt_n / eff_n) - 1, 3)
        efficiency <- (eff_n / uwgt_n)
        
        # apply new weight name
        names(out)[names(out) == 'wgt'] <- wgt.name
        
        # output message
        out_good <- green $ bold
        title1 <- 'iterake summary'
        num_dashes <- nchar(title1) + 4
        rem_dashes <- 80 - num_dashes
        
        # return output message?
        if (summary) {
            
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
            cat('        Loss: ' %+% paste0(loss) %+% '\n\n')
            
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
        
        return(out)
        
    }
    
}

utils::globalVariables(c(".", "act_prop", "wgt_temp", "prop_diff", "rn"))