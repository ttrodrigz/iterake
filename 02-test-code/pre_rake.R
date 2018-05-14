pre_rake <- function(df, pop.model) {
    
    # step 0: error checking tbd ----
    

    # step 1: calculated unweighted proportions in df ----
    
    # USE THIS TO CALCULATE N'S UWGT PROPS OVER EACH WGT CAT IN DF
    calcs <- function(x) {
        
        calc <- x %>%
            group_by(value) %>%
            summarise(uwgt_n = n()) %>%
            ungroup() %>%
            mutate(uwgt_prop = uwgt_n / sum(uwgt_n)) %>%
            ungroup()
        
    }
    
    # USE THIS TO MAKE LIST NAME INTO WGT_CAT VARIABLE
    add_wgt_cat <- function(df_list) {
        
        for (i in seq_along(df_list)) {
            
            df_list[[i]]$wgt_cat <- names(df_list[i])
            
        }
        
        df_list
    }
    
    uwgt <- 
        df %>%
        
        # need only wgt_cat vars
        select(one_of(mod$wgt_cat)) %>%
        
        # to maintain metadata
        map(as_tibble) %>%
        
        # do the calcs
        map(calcs) %>%
        
        # to bind all together
        add_wgt_cat() %>%
        map(group_by, wgt_cat) %>%
        map(nest, .key = "uwgt") %>%
        bind_rows()
    
    # step 2: join unweighted proportions to model
    # diff.calc <- function(df) {
    #     
    #     df %>%
    #         mutate(uwgt_diff = uwgt_prop - targ_prop)
    # }
    
    # comb <- 
    #     pmap(left_join(
    #         x = uwgt, 
    #         y = data, 
    #         by = "wgt_cat"), 
    #         
    #         function(...) {
    #             
    #             use <- list(...)
    #             
    #             
    #         }
    #     )
    
    comb.data <-
        left_join(x = uwgt,
                  y = mod,
                  by = "wgt_cat") %>%
        mutate(comb = map2(uwgt, data, left_join, by = "value")) %>%
        select(wgt_cat, comb)
    
    pmap(comb.data,
         function(...) {
             
             use.list <- list(...)
             
             use.list$comb %>%
                 mutate(value = as.character(value),
                        uwgt_diff = uwgt_prop - targ_prop)
         })
    
    # later ----
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
