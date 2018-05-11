missing_data_adjustment <- function(df, pop.model) {
    
    adjusted_model <- pmap(pop.model, function(..., main_data = df) {
        
        ## create list object out of all unspecified arguments passed from pmap
        ## - this is basically the row being evaluated
        inputList <- list(...)
        
        # get actual proportions to determine existance of NA
        act_props <- main_data %>% 
            group_by_(inputList$wgt_cat) %>%
            summarise(n = n()) %>%
            mutate(act_prop = n / sum(n)) %>%
            select(-n)
        
        # figure out what row of above has NA if any
        na_val_a <- which(is.na(act_props[1]))
        # determine if targets already have NA bin
        na_val_t <- which(is.na(inputList$data[1]))
        
        # if NA exists in actuals but not in targets - do stuff
        if (length(na_val_a) == 1 & length(na_val_t) == 0) {
            
            # determine proportion of NAs
            na_prop <- act_props[na_val_a, ]$act_prop
            
            # adjust current targets by a factor of 1 - na_prop
            new_targets <- inputList$data %>%
                mutate(targ_prop = targ_prop * (1 - na_prop))
            # insert a new row with the NA info
            new_targets[na_val_a, ] <- c(NA, na_prop)
            # replace existing target model
            inputList$data <- new_targets
        }
        
        # recreate tibble similar to how it's put together in wgt_Cat
        tibble(
            wgt_cat = inputList$wgt_cat,
            data = list(
                tibble(
                    value = inputList$data$value,
                    targ_prop = inputList$data$targ_prop)))
    
    }) %>%
        
        # bind similar to how it's done in pop_model
        bind_rows()
    
    # reassign class so all is as it should be
    class(adjusted_model) <- c(class(adjusted_model), "pop_model")

    return(adjusted_model)
}