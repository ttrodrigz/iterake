missing_data_adjustment <- function(df, pop.model) {
    
    # assign this here for workin
    model <- pop.model
    
    # adjust each weighting variable as necessary for missing data
    for (i in 1:length(model$wgt_cat)) {

        # get actual proportions to determine existance of NA
        act_props <- df %>% 
            group_by_(model$wgt_cat[[i]]) %>%
            summarise(n = n()) %>%
            mutate(act_prop = n / sum(n)) %>%
            select(-n)
        
        # figure out what row of above has NA if any
        na_val_a <- which(is.na(act_props[1]))
        # determine if targets already have NA bin
        na_val_t <- which(is.na(model$data[[i]][1]))
        
        # if NA exists in actuals but not in targets - do stuff
        if (length(na_val_a) == 1 & length(na_val_t) == 0) {
            
            # determine proportion of NAs
            na_prop <- test[na_val_a, ]$act_prop
            
            # adjust current targets by a factor of 1 - na_prop
            new_targets <- model$data[[i]] %>%
                mutate(targ_prop = targ_prop * (1 - na_prop))
            # insert a new row with the NA info
            new_targets[na_val_a, ] <- c(NA, na_prop)
            # replace existing target model
            model$data[[i]] <- new_targets
        }

    }
    
    return(model)
}