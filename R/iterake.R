#' Iterative rim weighting procedure
#' 
#' This function creates a weight variable that satisfies the targets described from the output of \code{wgt_design()}.
#' It then appends the weight variable to the data frame used to create the weights.
#' 
#' @param df Data frame containing data you intend to weight.
#' @param id Variable name for unique identifier in \code{`df`}.
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
#' @importFrom dplyr pull enquo mutate select group_by_ summarise left_join arrange quo_name %>%
#' @importFrom rlang !!
#' @importFrom data.table data.table
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
#'     id = order, 
#'     design = wgt_design(
#'         df = weight_me,
#' 
#'         wgt_cat(
#'             name = "seeds",
#'             buckets = c("Tornado", "Bird", "Earthquake"),
#'             targets = c(0.300, 0.360, 0.340)),
#' 
#'         wgt_cat(
#'             name = "costume",
#'             buckets = c("Bat Man", "Cactus"),
#'             targets = c(0.500, 0.500)),
#' 
#'         wgt_cat(
#'             name = "transport",
#'             buckets = c("Rocket Cart", "Jet Propelled Skis", "Jet Propelled Unicycle"),
#'             targets = c(0.400, 0.450, 0.150))
#' 
#'     )
#' )
#' 
#' @export
iterake <- function(df, id, design, wgt.name = "weight", 
                    wgt.lim = 3, threshold = 1e-20, max.iter = 50, 
                    stuck.limit = 5, N, summary = TRUE) {
    
    # step 1) setup + error checking ----
    if (!("wgt_design" %in% class(design))) {
        stop("Input to `design` must be output created by `wgt_design()`.")
    }
    
    # do stuff to to_weight
    to_weight <- df
    
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

    # deal with id's, initialize wgt = 1
    if (missing(id)) {
        stop("`id` is missing, must supply a unique identifier.")
        
    } else {
        
        if (!deparse(substitute(id)) %in% names(df)) {
            stop(paste0("id variable '", deparse(substitute(id)), "' not found in data."))
        }
        
        id <- enquo(id)
        
        to_weight <- to_weight %>%
            mutate(wgt = 1) %>%
            select(!! id, one_of(wgt_cats), wgt)
        
    }

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

        # loop through each variable in design$wgt_cat to generate weight
        for (i in seq_along(design$wgt_cat)) {

            # create data.table version of data with target var as key
            table_data <- data.table(to_weight, key = design$wgt_cat[[i]])
            
            # create data.table version of weights by value with target var as key
            table_wgt <- 
                table_data %>%
                group_by_(design$wgt_cat[[i]]) %>%
                summarise(act_prop = sum(wgt) / nrow(.)) %>%
                mutate(wgt_temp = ifelse(act_prop == 0, 0, design$data[[i]]$targ_prop / act_prop)) %>%
                select(design$wgt_cat[[i]], "wgt_temp") %>%
                data.table(., key = design$wgt_cat[[i]])

            # merge the data.table way - works as both have same key
            table_merge <- table_data[table_wgt]
            
            # combine weights, cap as needed, and remove wgt_tmp
            to_weight <- 
                table_merge %>%
                mutate(wgt = wgt * wgt_temp,
                       
                       # and force them to be no larger than wgt.lim, no smaller than 1/wgt.lim
                       wgt = ifelse(wgt >= wgt.lim, wgt.lim, wgt)) %>%

                ## THIS CAPS AT LOWER BOUND, REMOVING FOR NOW ****
                # # and force them to be no larger than wgt.lim, no smaller than 1/wgt.lim
                # wgt = ifelse(wgt >= wgt.lim, wgt.lim,
                #              ifelse(wgt <= 1/wgt.lim,
                #                     1/wgt.lim, wgt))) %>%

                # and remove wgt_temp
                select(-wgt_temp)
                
        }

        # store previous summed difference between targets and actuals
        prev_check <- check
        # reset/initialize check value
        check <- 0
        
        # loop through each to calculate discrepencies
        for (i in seq_along(design$wgt_cat)) {
            
            # compare new actuals to targets, sum abs(differences)
            sum_diffs <- 
                to_weight %>%
                group_by_(design$wgt_cat[[i]]) %>%
                summarise(act_prop = sum(wgt) / nrow(.)) %>%
                mutate(prop_diff = abs(design$data[[i]]$targ_prop - act_prop)) %>%
                summarise(out = sum(prop_diff)) %>%
                pull(out)
            
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
            df %>%
            left_join(to_weight %>% select(!! id, wgt), by = quo_name(id)) %>%
            mutate(wgt = wgt * x.factor) %>%
            arrange(!! id) %>%
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