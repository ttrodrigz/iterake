post_rake <- function(data, weight, pop.model) {
    
    # do some NA checks and adjust targets as needed
    pop.model <- missing_data_adjustment(data, pop.model)
    
    # setting up data
    weight <- enquo(weight)
    num_cats <- length(pop.model$wgt_cat)
    
    use_data <- data %>%
        select(!! weight, one_of(pop.model$wgt_cat)) %>%
        gather(wgt_cat, value, -1) 
    
    # unweighted
    uwgt <- use_data %>%
        group_by(wgt_cat, value) %>%
        summarise(uwgt_n = n()) %>%
        group_by(wgt_cat) %>%
        mutate(uwgt_prop = uwgt_n / sum(uwgt_n)) %>%
        ungroup()
    
    # weighted
    wgt <- use_data %>%
        group_by(wgt_cat, value) %>%
        summarise(wgt_n = sum(!! weight),
                  wgt_prop = sum(!! weight) / (nrow(.) / num_cats)) %>%
        ungroup()
    
    # join final output
    wgt_table <- left_join(uwgt, wgt,
                           by = c("wgt_cat", "value")) %>%
        left_join(pop.model %>%
                      unnest(data),
                  by = c("wgt_cat", "value")) %>%
        select(wgt_cat, value, uwgt_n, wgt_n, uwgt_prop, wgt_prop, targ_prop) %>%
        mutate(wgt_diff = targ_prop - wgt_prop)
    
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
        gather(wgt_type, wgt_val, -c(wgt_cat:wgt_n, targ_prop, wgt_diff)) %>%
        mutate(wgt_type = gsub("uwgt_prop", "Unweighted", wgt_type),
               wgt_type = gsub("wgt_prop", "Weighted", wgt_type))
    
    wgt_uwgt_chart <- wgt_uwgt_chart_data %>%
        ggplot(aes(x = as.character(value))) +
        geom_errorbar(aes(ymin = targ_prop,
                          ymax = targ_prop),
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
                     alpha = 0.2,
                     bins = prod(count(wgt_table, wgt_cat)$n)) +
        scale_y_continuous(expand = c(0, 0),
                           breaks = pretty) +
        scale_x_continuous(breaks = pretty) +
        ggtitle("Distribution of weight factors") +
        labs(x = "Weight factor",
             y = "Count") +
        theme_bw()
    
    print(wgt_dist)
    print(wgt_uwgt_chart)

    output <- list(deviance = wgt_table,
                   effects = check_table)

    # print summary to screen - invisible output object
    title1 <- 'post-rake deviance'
    num_dashes1 <- nchar(title1) + 4
    rem_dashes1 <- 80 - num_dashes1
    
    title2 <- 'post-rake effects'
    num_dashes2 <- nchar(title2) + 4
    rem_dashes2 <- 80 - num_dashes2
    
    cat('\n-- ' %+% 
            bold(title1) %+% 
            ' ' %+%
            paste(rep('-', times = rem_dashes1), collapse = "") %+%
            '\n')
    print.data.frame(wgt_table, row.names = FALSE)
    cat('\n-- ' %+% 
            bold(title2) %+% 
            ' ' %+%
            paste(rep('-', times = rem_dashes2), collapse = "") %+%
            '\n')
    cat('Unweighted N: ' %+% paste0(n, '\n'))
    cat(' Effective N: ' %+% paste0(round(neff, 2), '\n'))
    cat('  Weighted N: ' %+% paste0(wgt_n, '\n'))
    cat('  Efficiency: ' %+% paste0(scales::percent(round(efficiency, 4)), '\n'))
    cat('        Loss: ' %+% paste0(round(loss, 3), '\n\n') )
    invisible(output)

}