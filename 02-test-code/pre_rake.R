pre_rake <- function(data, pop.model) {
    
    # this function should...
    
    # calculate unweighted props
    # calc diff btw pop and uwgt
    # determine if weighting is necessary by
        ## % of sample in each group
        ## deviation from pop and uwgt
    
    # do some NA checks and adjust targets as needed
    pop.model <- missing_data_adjustment(data, pop.model)
    
    output <- left_join(
        data %>%
            select(one_of(pop.model$wgt_cat)) %>%
            gather(wgt_cat, value) %>%
            group_by(wgt_cat, value) %>%
            summarise(n = n()) %>%
            group_by(wgt_cat) %>%
            mutate(uwgt_prop = n / sum(n)) %>%
            ungroup(),
        
        pop.model %>%
            unnest(data) %>%
            ungroup(),
        
        by = c("wgt_cat", "value")) %>%
        
        mutate(diff = targ_prop - uwgt_prop)
    print(
        output %>%
            ggplot(aes(x = as.character(value))) +
            geom_errorbar(aes(ymin = targ_prop,
                              ymax = targ_prop),
                          lty = "longdash",
                          color = "#4b4b4b") +
            geom_point(aes(y = uwgt_prop),
                       size = 3,
                       color = "#d10000") +
            scale_y_continuous(breaks = pretty,
                               limits = c(0, max(output$uwgt_prop, 
                                                 output$targ_prop))) +
            facet_wrap(~wgt_cat, scales = "free_y") +
            labs(x = NULL, y = "Proportion",
                 color = NULL) +
            ggtitle("Sample's deviation from population model (unweighted)",
                    "Dashed line = target proportion") +
            coord_flip() +
            theme_bw() +
            theme(strip.background = element_rect(fill = "#fff6b5"),
                  legend.position = "bottom")
    )
    
    # print to screen - invisible output object
    title1 <- 'pre-rake deviation'
    num_dashes <- nchar(title1) + 4
    rem_dashes <- 80 - num_dashes
    cat('\n-- ' %+% 
            bold(title1) %+% 
            ' ' %+%
            paste(rep('-', times = rem_dashes), collapse = "") %+%
            '\n')
    print.data.frame(output, row.names = FALSE)
    invisible(output)
    
    # some ratio of bins vs sample size?
}

