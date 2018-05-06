iterake <- function(data, id, pop.model, wgt.name = "weight", join.weights = TRUE,
                    wgt.lim = 3, threshold = 1e-20, max.iter = 50) {
    
    # step 1) setup + error checking ----
    
    if (!("pop_model" %in% class(pop.model))) {
        stop("pop.model must be of the proper class. Use pop_model function.")
    }
    
    # do stuff to to_weight
    to_weight <- data
    
    # wgt_cats to get used later
    wgt_cats <- pop.model %>% pull(wgt_cat)
    
    # make sure dataframe is supplied
    if (!is.data.frame(data)) {
        stop("data must be an object of class 'data.frame'")
    }
    
    # make sure all wgt_cats are found in data
    not_in_data <- wgt_cats[!wgt_cats %in% names(data)]
    
    if (length(not_in_data) > 0) {
        stop(paste("The following weight category names are not found in your data:",
                   paste(not_in_data, collapse = ", ")),
             sep = "\n")
    }
    
    # make sure numeric stuff is numeric, character is character, lengths are 1
    
    ## wgt.lim
    if (length(wgt.lim) > 1) {
        
        stop("wgt.lim must be a numeric value of length 1")
        
    } else if (!is.numeric(wgt.lim)) {
        
        stop("wgt.lim must be numeric")
        
    } else if (wgt.lim <= 1) {
        
        stop("wgt.lim must be greater than 1")
    
    ## threshold        
    } else if (length(threshold) > 1) {
        
        stop("threshold must be a numeric value of length 1") 
            
    } else if (!is.numeric(threshold)) {
        
        stop("threshold must be numeric")
    
    ## max.iter
    } else if (length(max.iter) > 1) {
        
        stop("max.iter must be a numeric value of length 1") 
        
    } else if (!is.numeric(max.iter)) {
        
        stop("max.iter must be numeric")
        
    } else if (max.iter <= 0) {
        
        stop("max.iter must be a numeric value greater than 0")
    
    ## wgt.name    
    } else if (!is.character(wgt.name)) {
        
        warning(paste0("coercing wgt.name '", wgt.name, "' to character"))
        wgt.name <- as.character(wgt.name)
        
    } else if (length(wgt.name) > 1) {
        
        stop("wgt.name must be a character string of length 1")
        
    }
    
    # deal with id's, initialize wgt = 1
    if (missing(id)) {
        stop("id is missing, must supply a unique identifier")
        
    } else {
        
        if (!deparse(substitute(id)) %in% names(data)) {
            stop(paste0("id variable '", deparse(substitute(id)), "' not found in data"))
        }
        
        id <- enquo(id)
        
        to_weight <- to_weight %>%
            mutate(wgt = 1) %>%
            select(!! id, one_of(wgt_cats), wgt)
        
    }

    # do some NA checks and adjust targets as needed
    pop.model <- missing_data_adjustment(to_weight, pop.model)
    
    # data is now ready for weighting !!
    
    # step 2) do the raking ----
    
    # initialize things
    check <- 1
    count <- 0
    
    # do the loops until the threshold is reached
    while (check > threshold) {
        
        # iteration limit check
        if (count >= max.iter) {
            n <- nrow(to_weight)
            to_weight <- NULL
            break
        }
        
        # loop through each variable in pop.model$wgt_cat to generate weight
        for (i in 1:length(pop.model$wgt_cat)) {
            
            # merge data
            to_weight <- merge(
                
                to_weight,
                
                # with a merge of target proportion data
                merge(pop.model$data[[i]], 
                      to_weight %>% 
                          group_by(get(pop.model$wgt_cat[[i]])) %>%
                          summarise(act_prop = sum(wgt) / nrow(.)) %>%
                          set_names("value", "act_prop"),
                      
                      by = "value") %>%
                    
                    # calculate weight needed based on targ and actual proportions
                    mutate(wgt_temp = ifelse(act_prop == 0, 0, targ_prop / act_prop)) %>%
                    
                    # only keep variable value and weight
                    select(value, wgt_temp),
                
                by.x = pop.model$wgt_cat[[i]],
                by.y = "value",
                all = TRUE) %>%
                
                # and then multiply the merged wgt_temp by orig weight
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
        
        # reset/initialize check value
        check <- 0
        
        # loop through each to calculate discrepencies
        for (i in 1:length(pop.model$wgt_cat)) {
            
            # check is the sum of whatever check already is
            check <- check +
                
                # plus the sum of a pull from a merge of target proportions
                sum(merge(pop.model$data[[i]], 
                          
                          # and current weighted proportions data
                          to_weight %>% 
                              group_by(get(pop.model$wgt_cat[[i]])) %>%
                              summarise(act_prop = sum(wgt) / nrow(.)) %>%
                              set_names("value", "act_prop"),
                          
                          by = "value") %>%
                        
                        # calculate diff between targ and actual
                        mutate(prop_diff = abs(targ_prop - act_prop)) %>%
                        
                        # pull for sum
                        pull(prop_diff))
        }
        
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
        cat('  Iterations: ' %+% red(max.iter) %+% '\n\n')
        cat('Unweighted N: ' %+% red(n) %+% '\n')
        cat(' Effective N: ' %+% red('--') %+% '\n')
        cat('  Weighted N: ' %+% red('--') %+% '\n')
        cat('  Efficiency: ' %+% red('--') %+% '\n')
        cat('        Loss: ' %+% red('--') %+% '\n')
        
    } else {
        
        # clean df to output
        if (join.weights) {
            out <- to_weight %>%
                select(!! id, wgt, everything()) %>%
                arrange(!! id) %>%
                as_tibble()
        } else {
            out <- to_weight %>%
                select(!! id, wgt) %>%
                arrange(!! id) %>%
                as_tibble()
        }
        
        # calculate stats
        wgt <- out$wgt
        n <- nrow(out)
        wgt_n <- sum(wgt)
        neff <- (sum(wgt) ^ 2) / sum(wgt ^ 2)
        loss <- round((n / neff) - 1, 3)
        efficiency <- (neff / n)
        
        # apply new weight name
        names(out)[names(out) == 'wgt'] <- wgt.name
        
        # output message
        out_good <- green $ bold
        title1 <- 'iterake summary & effects'
        num_dashes <- nchar(title1) + 4
        rem_dashes <- 80 - num_dashes
        
        cat('\n-- ' %+% 
                bold(title1) %+% 
                ' ' %+%
                paste(rep('-', times = rem_dashes), collapse = "") %+%
                '\n')
        cat(' Convergence: ' %+% green('Success '%+% '\U2714') %+% '\n')
        cat('  Iterations: ' %+% green(count) %+% '\n\n')
        cat('Unweighted N: ' %+% green(n) %+% '\n')
        cat(' Effective N: ' %+% green(round(neff, 2)) %+% '\n')
        cat('  Weighted N: ' %+% green(wgt_n) %+% '\n')
        cat('  Efficiency: ' %+% green(scales::percent(round(efficiency, 4))) %+% '\n')
        cat('        Loss: ' %+% green(loss) %+% '\n\n')
        
        return(out)
        
    }
    
}