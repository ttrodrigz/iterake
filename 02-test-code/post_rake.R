post_rake <- function(data, weight, pop.model, store = FALSE) {
    
    # setting up data
    weight <- enquo(weight)
    num_cats <- length(pop.model$wgt_cat)
    
    use_data <- data %>%
        select(!! weight, one_of(pop.model$wgt_cat)) %>%
        gather(wgt_cat, value, -1) 
    
    # unweighted
    uwgt <- use_data %>%
        group_by(wgt_cat, value) %>%
        summarise(uwgt.n = n()) %>%
        group_by(wgt_cat) %>%
        mutate(uwgt.prop = uwgt.n / sum(uwgt.n)) %>%
        ungroup()
    
    # weighted
    wgt <- use_data %>%
        group_by(wgt_cat, value) %>%
        summarise(wgt.n = sum(!! weight),
                  wgt.prop = sum(!! weight) / (nrow(.) / num_cats)) %>%
        ungroup()
    
    # join final output
    wgt_table <- left_join(uwgt, wgt,
                           by = c("wgt_cat", "value")) %>%
        left_join(pop.model %>%
                      unnest(data),
                  by = c("wgt_cat", "value")) %>%
        select(wgt_cat, value, uwgt.n, wgt.n, uwgt.prop, wgt.prop, targ.prop) %>%
        mutate(wgt.diff = targ.prop - wgt.prop)
    
    wgt <- data %>% pull(!! weight)
    n <- nrow(data)
    wgt_n <- sum(wgt)
    neff <- (sum(wgt) ^ 2) / sum(wgt ^ 2)
    loss <- (n / neff) - 1
    efficiency <- (neff / n)

    check_table <- tibble(
        uwgt_n = n,
        neff = neff,
        wgt_n = wgt_n,
        efficiency = efficiency,
        loss = loss
    )
    
    # weighted vs unweighted chart
    wgt_uwgt_chart_data <- wgt_table %>%
        group_by(wgt_cat) %>%
        gather(wgt_type, wgt_val, -c(wgt_cat:wgt.n, targ.prop, wgt.diff)) %>%
        mutate(wgt_type = gsub("uwgt.prop", "Unweighted", wgt_type),
               wgt_type = gsub("wgt.prop", "Weighted", wgt_type))
    
    wgt_uwgt_chart <- wgt_uwgt_chart_data %>%
        ggplot(aes(x = value)) +
        geom_errorbar(aes(ymin = targ.prop,
                          ymax = targ.prop),
                      lty = "longdash",
                      color = "#4b4b4b") +
        geom_point(aes(y = wgt_val,
                       color = wgt_type),
                   size = 3) +
        scale_color_manual(values = c("Unweighted" = "#d10000", 
                                      "Weighted" = "#006fd1")) +
        scale_y_continuous(breaks = pretty,
                           limits = c(0, max(wgt_uwgt_chart_data$wgt_val))) +
        facet_wrap(~wgt_cat, scales = "free_y") +
        labs(x = NULL, y = "Proportion",
             color = NULL) +
        ggtitle("Sample's deviation from population model (unweighted/weighted)",
                "Dashed line = target proportion in population model") +
        theme_bw() +
        theme(strip.background = element_rect(fill = "#fff6b5")) +
        coord_flip()
    
    
    # weight distribution
    wgt_dist <- data %>%
        ggplot(aes_string(x = quo_name(weight))) +
        geom_histogram(color = NA,
                     fill = "#006fd1",
                     alpha = 0.2) +
        scale_y_continuous(expand = c(0, 0),
                           breaks = pretty) +
        scale_x_continuous(breaks = pretty) +
        ggtitle("Distribution of weight factors") +
        labs(x = "Weight factor",
             y = "Count") +
        theme_bw()
    
    print(wgt_dist)
    print(wgt_uwgt_chart)

    output <- list(pre.post = wgt_table,
                   diagnostics = check_table)
    
    if (store) {
        output
    } else {
        cat('\n-- ' %+% bold('post-rake deviation') %+% ' ----------------------\n')
        print(wgt_table)
        print(check_table)
        data
    }
    
}