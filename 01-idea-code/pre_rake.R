pre_rake <- function(data, pop.model, deviance = .02, min.size = 0.05, print.console = TRUE) {
    
    # this function should...
    
    # calculate unweighted props
    # calc diff btw pop and uwgt
    # determine if weighting is necessary by
        ## % of sample in each group
        ## deviation from pop and uwgt
    
    output <- left_join(
        data %>%
            select(one_of(pop.model$wgt_cat)) %>%
            gather(wgt_cat, value) %>%
            group_by(wgt_cat, value) %>%
            summarise(n = n()) %>%
            group_by(wgt_cat) %>%
            mutate(uwgt.prop = n / sum(n)) %>%
            ungroup(),
        
        pop.model %>%
            unnest(data) %>%
            ungroup(),
        
        by = c("wgt_cat", "value")) %>%
        
        mutate(dev = targ.prop - uwgt.prop,
               dev.flag = ifelse(abs(dev) < deviance, 1, 0),
               size.flag = ifelse(targ.prop < min.size, 1, 0))
    
    if (print.console) {
        print(output)
    } else {
        output    
    }
    
    # some ratio of bins vs sample size?
}

