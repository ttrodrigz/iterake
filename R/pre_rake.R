#' Compare unweighted data to targets
#' 
#' Inspect by what degree the unweighted sample proportions deviate from the
#' target proportions in the population model.
#' 
#' @param df Data frame of the data you intend on weighting.
#' @param design Full weighting design created by \code{wgt_design()}.
#' @param plot Display plot, default = FALSE.
#' 
#' @importFrom dplyr select group_by summarise mutate ungroup n bind_rows left_join rename one_of
#' @importFrom purrr map map2 set_names
#' @importFrom tibble as_tibble
#' @importFrom tidyr nest unnest
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point scale_y_continuous facet_wrap labs element_rect ggtitle coord_flip theme_bw theme
#' 
#' @return A tibble of unweighted counts and proportions, difference between 
#' unweighted and target proportions. Optionally, a plot of this information.
#' 
#' @examples 
#' data("weight_me")
#' 
#' mod <- wgt_design(
#' 
#'     df = weight_me,
#' 
#'     wgt_cat(
#'         name = "gender",
#'         buckets = c(1, 2),
#'         targets = c(0.5, 0.5)),
#' 
#'     wgt_cat(
#'         name = "vehicle",
#'         buckets = c(1, 2, 3),
#'         targets = c(0.333, 0.333, 0.333))
#' )
#' 
#' pre_rake(
#'     df = weight_me,
#'     design = mod,
#'     plot = TRUE
#' )
#' 
#' @export
utils::globalVariables(c("bucket", "buckets", "uwgt_n", "data", "comb", "uwgt_prop", "targ_prop"))

pre_rake <- function(df, design, plot = FALSE) {
    
    # step 1: error checking tbd ----
    if (!is.data.frame(df)) {
        stop("`df` must be a dataframe.")
    }
    
    df.names <- names(df)
    mod.names <- design$wgt_cat
    bad.names <- mod.names[!mod.names %in% df.names]
    
    if (length(bad.names) > 0) {
        stop(
            paste(
                "Each weighting category in `design` must have a matching column name in `df`. The following weighting cagegories have no match:",
                paste(bad.names, collapse = ", "),
                sep = "\n"
            )
        )
    }
    
    # step 2: calculated unweighted proportions in df ----
    
    # calculates unweighted props over each weight category
    calcs <- function(x) {
        
        calc <- x %>%
            group_by(buckets) %>%
            summarise(uwgt_n = n()) %>%
            mutate(uwgt_prop = uwgt_n / sum(uwgt_n)) %>%
            ungroup()
        
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
        select(one_of(design$wgt_cat)) %>%
        
        # to maintain metadata
        map(as_tibble) %>%
        map(set_names, "buckets") %>%
        
        # do the calcs
        map(calcs) %>%
        
        # to bind all together
        add_wgt_cat() %>%
        map(group_by, wgt_cat) %>%
        map(nest, .key = "uwgt") %>%
        bind_rows()
    
    # step 3: create final output ----
    out <-
        
        # join population model
        uwgt %>%
        left_join(design, by = "wgt_cat") %>%
        
        # join data from population model to unweighted props
        # calculate difference from unweighted to target
        mutate(comb = map2(uwgt, data, left_join, by = "buckets")) %>%
        select(wgt_cat, comb) %>%
        mutate(comb = map(comb, function(x)
            x %>% mutate(uwgt_diff = uwgt_prop - targ_prop))
        ) %>%
        
        # unnest results
        unnest(comb) %>%
        rename(bucket = buckets)
    
    out
    
    # step 4: plot ----
    if (isTRUE(plot)) {
        print(
            out %>%
                
                # begin plot
                ggplot(aes(x = as.character(bucket))) +
                
                # errorbars
                geom_errorbar(
                    aes(ymin = targ_prop,
                        ymax = targ_prop),
                    lty = "longdash",
                    color = "#4b4b4b"
                ) +
                
                # points
                geom_point(
                    aes(y = uwgt_prop),
                    size = 3,
                    color = "#d10000"
                ) +
                
                # adjust scales, use 0 to max of uwgt/targ props
                scale_y_continuous(
                    breaks = pretty,
                    limits = c(0, max(out$uwgt_prop, out$targ_prop))
                ) +
                
                # facet plots, independent y (eventually x) axes
                facet_wrap(~wgt_cat, scales = "free_y") +
                
                # tweak labels
                labs(
                    x = NULL,
                    y = "Proportion",
                    color = NULL
                ) +
                
                # add title
                ggtitle(
                    "Unweighted vs. Target Proportions",
                    "Dashed line = target"
                ) +
                
                # final theming
                coord_flip() +
                theme_bw() +
                theme(strip.background = element_rect(fill = "#fff6b5"))
        )
    }
    
    return(out)
    
}