pre_rake <- function(df, pop.model) {
    
    # make sure each wgt_cat in pop.model has a matching column in df
    df.names  <- names(df)
    mod.names <- pull(pop.model, wgt_cat)
    bad.names <- mod.names[!mod.names %in% df.names]
    
    if (length(bad.names) > 0) {
        stop(
            paste(
                "Each weighting category in `pop.model` must have a matching column name in `df`. The following weighting cagegories have no match:",
                paste(bad.names, collapse = ", "),
                sep = "\n"
            )
        )
    }
    
    # do a tally of NA's and tell 'em what's going to happen
    df %>%
        select(one_of(pop.model$wgt_cat)) %>%
        map_int(sum(is.na(.)))
    

    # do some NA checks and adjust targets as needed
    # pop.model <- missing_data_adjustment(df, pop.model)
    
    # ----
    # output <- dplyr::left_join(
    #     df %>%
    #         dplyr::select(one_of(pop.model$wgt_cat)) %>%
    #         tidyr::gather(wgt_cat, value) %>%
    #         dplyr::group_by(wgt_cat, value) %>%
    #         dplyr::summarise(n = n()) %>%
    #         dplyr::group_by(wgt_cat) %>%
    #         dplyr::mutate(uwgt_prop = n / sum(n)) %>%
    #         dplyr::ungroup(),
    #     
    #     pop.model %>%
    #         tidyr::unnest(data) %>%
    #         dplyr::ungroup(),
    #     
    #     by = c("wgt_cat", "value")) %>%
    #     
    #     dplyr::mutate(uwgt_diff = targ_prop - uwgt_prop)
    # 
    # print(
    #     output %>%
    #         ggplot(aes(x = as.character(value))) +
    #         geom_errorbar(aes(ymin = targ_prop,
    #                           ymax = targ_prop),
    #                       lty = "longdash",
    #                       color = "#4b4b4b") +
    #         geom_point(aes(y = uwgt_prop),
    #                    size = 3,
    #                    color = "#d10000") +
    #         scale_y_continuous(breaks = pretty,
    #                            limits = c(0, max(output$uwgt_prop, 
    #                                              output$targ_prop))) +
    #         facet_wrap(~wgt_cat, scales = "free_y") +
    #         labs(x = NULL, y = "Proportion",
    #              color = NULL) +
    #         ggtitle("Sample's deviation from population model (unweighted)",
    #                 "Dashed line = target proportion") +
    #         coord_flip() +
    #         theme_bw() +
    #         theme(strip.background = element_rect(fill = "#fff6b5"),
    #               legend.position = "bottom")
    # )
    # 
    # # print to screen - invisible output object
    # title1 <- 'pre-rake deviance'
    # num_dashes <- nchar(title1) + 4
    # rem_dashes <- 80 - num_dashes
    # cat('\n-- ' %+% 
    #         bold(title1) %+% 
    #         ' ' %+%
    #         paste(rep('-', times = rem_dashes), collapse = "") %+%
    #         '\n')
    # print.data.frame(output, row.names = FALSE)
    # invisible(output)
    
    # some ratio of bins vs sample size?
}

pre_rake(df_1, mod)


df_1 %>%
    select(one_of(mod$wgt_cat)) %>%
    map_int(~ sum(is.na(.)))
