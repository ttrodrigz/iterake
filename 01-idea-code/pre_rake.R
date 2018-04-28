post_rake <- function(data, weight, pop.model) {
    
    # setting up data
    weight <- enquo(weight)
    num_cats <- length(pop.model$wgt_cat)
    
    use_data <- data %>%
        select(!! weight, one_of(pop.model$wgt_cat)) %>%
        gather(wgt_cat, value, -1) 
    
    # unweighted
    uwgt <- use_data %>%
        group_by(wgt_cat, value) %>%
        count %>%
        group_by(wgt_cat) %>%
        mutate(uwgt.prop = n / sum(n)) %>%
        ungroup()
    
    # weighted
    wgt <- use_data %>%
        group_by(wgt_cat, value) %>%
        summarise(wgt.prop = sum(!! weight) / (nrow(.) / num_cats)) %>%
        ungroup()
    
    # join final output
    wgt_table <- left_join(uwgt, wgt,
                           by = c("wgt_cat", "value")) %>%
        left_join(pop.model %>%
                      unnest(data),
                  by = c("wgt_cat", "value"))
    
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
    wgt_uwgt_chart <- wgt_table %>%
        group_by(wgt_cat) %>%
        gather(wgt_type, wgt_val, -c(wgt_cat:n, targ.prop)) %>%
        mutate(wgt_type = gsub("uwgt.prop", "Unweighted", wgt_type),
               wgt_type = gsub("wgt.prop", "Weighted", wgt_type)) %>%
        ggplot(aes(x = value)) +
        geom_errorbar(aes(ymin = targ.prop,
                          ymax = targ.prop),
                      lty = "longdash") +
        geom_point(aes(y = wgt_val,
                       color = wgt_type),
                   size = 3) +
        scale_color_manual(values = c("Unweighted" = "#4b4b4b", 
                                      "Weighted" = "#63A5DA")) +
        scale_y_continuous(breaks = pretty) +
        facet_wrap(~wgt_cat, scales = "free_x") +
        labs(x = NULL, y = "Proportion",
             color = NULL) +
        ggtitle("Sample proportions vs. population (pre/post weighting)") +
        theme_bw()
    
    # weight distribution
    wgt_dist <- data %>%
        ggplot(aes_string(x = quo_name(weight))) +
        geom_density(color = NA,
                     fill = "#63A5DA",
                     alpha = 0.5) +
        scale_y_continuous(expand = c(0, 0),
                           breaks = pretty) +
        scale_x_continuous(breaks = pretty) +
        ggtitle("Distribution of weight factors") +
        labs(x = "Weight factor",
             y = "Density") +
        theme_bw()
    
    print(wgt_dist)
    print(wgt_uwgt_chart)
    
    list(pre.post = wgt_table,
         diagnostics = check_table)
    
}


iterake_check(clean, wgt, model_test)
