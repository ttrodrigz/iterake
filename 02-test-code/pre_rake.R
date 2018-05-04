pre_rake <- function(data, pop.model, store = FALSE) {
    
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
        
        mutate(diff = targ.prop - uwgt.prop)
    print(
        output %>%
            ggplot(aes(x = value)) +
            geom_errorbar(aes(ymin = targ.prop,
                              ymax = targ.prop),
                          lty = "longdash",
                          color = "#4b4b4b") +
            geom_point(aes(y = uwgt.prop),
                       size = 3,
                       color = "#d10000") +
            scale_y_continuous(breaks = pretty,
                               limits = c(0, max(output$uwgt.prop, 
                                                 output$targ.prop))) +
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
    
    if (store) {
        output
    } else {
        cat('\n-- ' %+% bold('pre-rake deviation') %+% ' -----------------------\n')
        print.data.frame(output, row.names = FALSE)
        invisible(data)
    }
    
    # some ratio of bins vs sample size?
}

