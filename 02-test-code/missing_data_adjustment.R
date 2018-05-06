missing_data_adjustment <- function(data, pop.model) {
    
    # assign this here for workin
    model <- pop.model
    
    # adjust each weighting variable as necessary for missing data
    for (i in 1:length(model$wgt_cat)) {

        # get target proportions from pop.model and evaluate data for NAs
        target_merge <- merge(model$data[[i]], 
                              data %>% 
                                  group_by(get(model$wgt_cat[[i]])) %>%
                                  summarise(n = n()) %>%
                                  mutate(act_prop = n / sum(n)) %>%
                                  select(-n) %>%
                                  set_names("value", "act_prop"),
                              
                              by = "value", all = TRUE) %>% as.tibble()
        
        # get row of NA value if present, check if targ_prop is also NA
        na_val <- which(is.na(target_merge$value))
        na_targ <- ifelse(length(na_val) == 0, FALSE, is.na(target_merge[na_val, ]$targ_prop))
        
        # adjust and replace targets if NA is present in both value and targ_prop
        if (length(na_val) == 1 & na_targ) {
            
            # get act_prop of NA
            na_prop <- target_merge[na_val, ]$act_prop
            
            # mutate non-NA targ_props by factor of 1 - na_prop; assign na_prop to NA targ_prop
            target_merge <- target_merge %>%
                mutate(targ_prop = targ_prop * (1 - na_prop),
                       targ_prop = ifelse(is.na(targ_prop), na_prop, targ_prop)) %>%
                
                # just keep value and targ_prop for replacing
                select(value, targ_prop)
            
            # assign new targ_prop data to pop.model
            model$data[[i]] <- target_merge
        }
        
    }
    
    return(model)
}