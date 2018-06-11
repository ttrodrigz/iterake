post_rake <- function(df, weight, pop.model, plot = FALSE) {
    
    # step 1: error checking tbd ----
    if (!is.data.frame(df)) {
        stop("`df` must be a dataframe.")
    }
    
    if (missing(weight)) {
        stop("'weight' is missing, must supply weight variable.")
        
    } else {
        
        if (!deparse(substitute(weight)) %in% names(df)) {
            stop(paste0("weight variable '", deparse(substitute(weight)), "' not found in data."))
        }
        
        weight <- dplyr::enquo(weight)
    }
    
    df.names <- names(df)
    mod.names <- pop.model$wgt_cat
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
    
    # used for wgt prop calculations    
    num_cats <- length(pop.model$wgt_cat)
    
    # step 2: calculated unweighted proportions in df ----
    
    # calculates unweighted props over each weight category
    calcs_uwgt <- function(x) {
        
        calc <- x %>%
            dplyr::group_by(buckets) %>%
            dplyr::summarise(uwgt_n = n()) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(uwgt_prop = uwgt_n / sum(uwgt_n)) %>%
            dplyr::ungroup()
        
    }
    
    # calculates unweighted props over each weight category
    calcs_wgt <- function(x, weight) {
        
        calc <- x %>%
            dplyr::mutate(wgt = weight) %>%
            dplyr::group_by(buckets) %>%
            dplyr::summarise(wgt_n = sum(wgt)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(wgt_prop = wgt_n / sum(wgt_n)) %>%
            dplyr::ungroup()
        
    }
    
    # turns list name into wgt_cat variable
    add_wgt_cat <- function(df_list) {
        
        for (i in seq_along(df_list)) {
            
            df_list[[i]]$wgt_cat <- names(df_list[i])
            
        }
        
        df_list
    }
    
    uwgt <- 
        
        df %>%
        
        # need only wgt_cat vars
        dplyr::select(one_of(pop.model$wgt_cat)) %>%
        
        # to maintain metadata
        purrr::map(as_tibble) %>%
        purrr::map(set_names, "buckets") %>%
        
        # do the calcs
        purrr::map(calcs_uwgt) %>%
        
        # to bind all together
        add_wgt_cat() %>%
        purrr::map(group_by, wgt_cat) %>%
        purrr::map(nest, .key = "uwgt") %>%
        dplyr::bind_rows()
    
    wgt <- 
        
        df %>%
        
        # need only wgt_cat vars
        dplyr::select(one_of(pop.model$wgt_cat)) %>%
        
        # to maintain metadata
        purrr::map(as_tibble) %>%
        purrr::map(set_names, "buckets") %>%
        
        # do the calcs
        purrr::map(calcs_wgt, df %>% pull(!! weight)) %>%
        
        # to bind all together
        add_wgt_cat() %>%
        purrr::map(group_by, wgt_cat) %>%
        purrr::map(nest, .key = "wgt") %>%
        dplyr::bind_rows()
    
    # step 3: create final output ----
    out <-
        
        # join population model
        uwgt %>%
        dplyr::left_join(pop.model, by = "wgt_cat") %>%
        dplyr::left_join(wgt, by = "wgt_cat") %>%
        
        # join data from population model to unweighted props
        # calculate difference from unweighted to target
        dplyr::mutate(comb = map2(uwgt, data, dplyr::left_join, by = "buckets")) %>%
        dplyr::mutate(comb = map2(comb, wgt, dplyr::left_join, by = "buckets")) %>%
        dplyr::select(wgt_cat, comb) %>%
        dplyr::mutate(comb = map(comb, function(x)
            x %>%
                dplyr::mutate(uwgt_diff = uwgt_prop - targ_prop,
                              wgt_diff = targ_prop - wgt_prop))
        ) %>%
        
        # unnest results
        tidyr::unnest(comb) %>%
        dplyr::rename(bucket = buckets) %>%
        dplyr::select(wgt_cat, bucket, uwgt_n, wgt_n, uwgt_prop, wgt_prop, targ_prop, uwgt_diff, wgt_diff)
    
    out
    
    # step 4: plot ----
    if (isTRUE(plot)) {
        chart_data <-
            out %>%
            dplyr::group_by(wgt_cat) %>%
            tidyr::gather(wgt_type, wgt_val, -c(wgt_cat:wgt_n, targ_prop, uwgt_diff, wgt_diff)) %>%
            dplyr::mutate(wgt_type = gsub("uwgt_prop", "Unweighted", wgt_type),
                          wgt_type = gsub("wgt_prop", "Weighted", wgt_type))
        print(
            chart_data %>%
                
                # begin plot
                ggplot2::ggplot(aes(x = as.character(bucket))) +
                
                # errorbars
                ggplot2::geom_errorbar(aes(ymin = targ_prop,
                                           ymax = targ_prop),
                                       lty = "longdash",
                                       color = "#4b4b4b") +
                
                # points
                ggplot2::geom_point(aes(y = wgt_val,
                                        color = wgt_type),
                                    size = 3) +
                
                # set point colors
                ggplot2::scale_color_manual(values = c("Unweighted" = "#d10000", 
                                                       "Weighted" = "#006fd1")) +
                
                # adjust scales, use 0 to max of uwgt/targ props
                ggplot2::scale_y_continuous(breaks = pretty,
                                            limits = c(0, max(chart_data$wgt_val))) +
                
                # facet plots, independent y (eventually x) axes
                ggplot2::facet_wrap(~wgt_cat, scales = "free_y") +
                
                # tweak labels
                ggplot2::labs(x = NULL, 
                              y = "Proportion",
                              color = NULL) +
                
                # add title
                ggplot2::ggtitle("Unweighted and Weighted vs. Target Proportions",
                                 "Dashed line = target") +
                
                # final theming
                ggplot2::theme_bw() +
                ggplot2::theme(strip.background = element_rect(fill = "#fff6b5")) +
                ggplot2::coord_flip()
        )
        
    }
    
    return(out)
    
}