#' Iterative rim weighting procedure
#' 
#' This function creates a weight variable that satisfies the targets described from the output of \code{wgt_design()}.
#' It then appends the weight variable to the data frame used to create the weights.
#' 
#' @param df Data frame containing data you intend to weight.
#' @param design Data frame containing data you intend to weight.
#' @param wgt.name Character name for created weight variable, optional.
#' @param wgt.lim Numeric value created weights cannot exceed, optional.
#' @param threshold Numeric value specifying minimum summed difference between target and weighted proportions, optional.
#' @param max.iter Numeric value capping number of iterations for the procedure, optional.
#' @param stuck.limit Numeric value capping the number of times summed differences between target and weighted
#' proportions can oscillate between increasing and decreasing, optional.
#' @param N Numeric value representing expansion factor to be applied to generated weights, optional.
#' @param summary Boolean value for whether to show summary output of the procedure, optional.
#' 
#' @importFrom dplyr pull enquo mutate select %>%
#' @importFrom rlang !!
#' @importFrom data.table data.table setkey
#' @importFrom crayon red yellow green bold %+%
#' @importFrom tibble as.tibble
#' @importFrom scales percent
#' 
#' @return Data frame with the resulting weight variable appended to it.
#' 
#' @examples 
#' data(weight_me)
#' 
#' iterake(
#'     df = weight_me,
#'     design = universe(
#'         df = weight_me,
#' 
#'         build_margin(
#'             name = "seeds",
#'             buckets = c("Tornado", "Bird", "Earthquake"),
#'             targets = c(0.300, 0.360, 0.340)),
#' 
#'         build_margin(
#'             name = "costume",
#'             buckets = c("Bat Man", "Cactus"),
#'             targets = c(0.500, 0.500)),
#' 
#'         build_margin(
#'             name = "transport",
#'             buckets = c("Rocket Cart", "Jet Propelled Skis", "Jet Propelled Unicycle"),
#'             targets = c(0.400, 0.450, 0.150))
#' 
#'     )
#' )
#' 
#' @export
iterake <- function(df, design, wgt.name = "weight", 
                    wgt.lim = 3, threshold = 1e-20, max.iter = 50, 
                    stuck.limit = 5, N, summary = TRUE) {
    
    # step 1) setup + error checking ----
    if (!("universe" %in% class(design))) {
        stop("Input to `design` must be output created by `universe()`.")
    }
    
    # do stuff to to_weight - make data.table and use rn as index
    to_weight <- data.table(df, keep.rownames = TRUE)[, rn := as.numeric(rn)]
    
    # wgt_cats to get used later
    wgt_cats <- pull(design, wgt_cat)
    
    # make sure dataframe is supplied
    if (!is.data.frame(df)) {
        stop("Input to `df` must be a data frame.")
    }
    
    # make sure all wgt_cats are found in data
    not_in_data <- wgt_cats[!wgt_cats %in% names(df)]
    
    if (length(not_in_data) > 0) {
        stop(paste("The following weight category names are not found in your data:",
                   paste(not_in_data, collapse = ", ")),
             sep = "\n")
    }
    
    # make sure numeric stuff is numeric, character is character, lengths are 1
    
    ## wgt.lim
    if (length(wgt.lim) > 1) {
        
        stop("wgt.lim must be a numeric value of length 1.")
        
    } else if (!is.numeric(wgt.lim)) {
        
        stop("wgt.lim must be numeric.")
        
    } else if (wgt.lim <= 1) {
        
        stop("wgt.lim must be a numeric value greater than 1.")
    
    ## threshold        
    } else if (length(threshold) > 1) {
        
        stop("threshold must be a numeric value of length 1.") 
            
    } else if (!is.numeric(threshold)) {
        
        stop("threshold must be numeric.")
    
    ## max.iter
    } else if (length(max.iter) > 1) {
        
        stop("max.iter must be a numeric value of length 1.") 
        
    } else if (!is.numeric(max.iter)) {
        
        stop("max.iter must be numeric.")
        
    } else if (max.iter <= 0) {
        
        stop("max.iter must be a numeric value greater than 0.")
    
    ## wgt.name    
    } else if (!is.character(wgt.name)) {
        
        warning(paste0("coercing wgt.name '", wgt.name, "' to character."))
        wgt.name <- as.character(wgt.name)
        
    } else if (length(wgt.name) > 1) {
        
        stop("wgt.name must be a character string of length 1.")
        
    }
    
    # N for expansion factor
    if (!missing(N)) {
        if (!is.numeric(N) || length(N) != 1) {
            stop("`N` must be a numeric value of length 1 corresponding to size of population.")
        }
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
    
    N <- nrow(to_weight)
    wgt_names <- design[["wgt_cat"]]
    
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
        
        # loop through each variable in design$wgt_cat to generate weight
        for (i in seq_along(wgt_names)) {
            
            # data.table versions of:
            # - data being weighted, i'th weighting column as key
            # - i'th dataset (targets) of design
            DT_data   <- data.table(to_weight, key = wgt_names[i])
            DT_design <- data.table(design[["data"]][[i]])
            
            # start with original data.table-ized object
            DT_merge <- 
                
                # original everything is getting merged to
                DT_data[
                    
                    # DT object to be merged with the original
                    # ideal becuase it uses the same key for merging
                    DT_data[, 
                            # calcs the "haves" by the i'th weighting category
                            .(act_prop = sum(wgt) / N),
                            by = c(wgt_names[i])
                            
                            # merge haves with wants
                            ][DT_design
                              
                              ][, 
                                # calculate wants over haves by the i'th weighting category
                                .(wgt_temp = ifelse(act_prop == 0, 0, targ_prop / act_prop)), 
                                by = c(wgt_names[i])
                                ]
                    ]
            
            # creates the weight factor, max out at the weight limit
            to_weight <- DT_merge[, wgt := ifelse(wgt * wgt_temp > wgt.lim, wgt.lim, wgt * wgt_temp)][
                # temporary weight no longer needed
                , wgt_temp := NULL]
            
        }

        # loop through each to calculate discrepencies
        for (i in seq_along(wgt_names)) {
            
            DT_design <- data.table(design[["data"]][[i]])
            DT_data   <- data.table(to_weight, key = wgt_names[i])
            
            sum_diffs <- DT_data[, 
                                   # calcs the "haves" by the i'th weighting category
                                   .(act_prop = sum(wgt) / N),
                                   by = c(wgt_names[i])
                                   
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
        
        title1 <- 'iterake summary & effects'
        num_dashes <- nchar(title1) + 4
        rem_dashes <- 80 - num_dashes
        
        cat('\n-- ' %+% 
                bold(title1) %+% 
                ' ' %+%
                paste(rep('-', times = rem_dashes), collapse = "") %+%
                '\n')
        cat(' Convergence: ' %+% red('Failed '%+% '\U2718') %+% '\n')
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

            as.tibble()

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
        title1 <- 'iterake summary & effects'
        num_dashes <- nchar(title1) + 4
        rem_dashes <- 80 - num_dashes
        
        # return output message?
        if (isTRUE(summary)) {
            
            cat('\n-- ' %+% 
                    bold(title1) %+% 
                    ' ' %+%
                    paste(rep('-', times = rem_dashes), collapse = "") %+%
                    '\n')
            if (stuck_check > 0) {
                cat(' Convergence: ' %+% yellow('Success '%+% '\U2714') %+% '\n')
            } else {
                cat(' Convergence: ' %+% green('Success '%+% '\U2714') %+% '\n')    
            }
            cat('  Iterations: ' %+% paste0(count) %+% '\n\n')
            cat('Unweighted N: ' %+% paste0(sprintf("%.2f", uwgt_n)) %+% '\n')
            cat(' Effective N: ' %+% paste0(round(eff_n,  2)) %+% '\n')
            cat('  Weighted N: ' %+% paste0(sprintf("%.2f", wgt_n)) %+% '\n')
            cat('  Efficiency: ' %+% paste0(percent(round(efficiency, 4))) %+% '\n')
            cat('        Loss: ' %+% paste0(loss) %+% '\n\n')
            
            if (stuck_check > 0) {
                cat(' NOTE: ' %+% 
                        yellow('Iterations stopped at a difference of ' %+% 
                                   paste0(
                                       formatC(stuck_check, 
                                               format = "e", 
                                               digits = 3))) %+% 
                        '\n\n')
            }
        }
        
        return(out)
        
    }
    
}

utils::globalVariables(c(".", "act_prop", "wgt_temp", "prop_diff"))