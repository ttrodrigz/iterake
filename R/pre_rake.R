#' Compare unweighted data to targets
#' 
#' Inspect by what degree the unweighted sample proportions deviate from the
#' target proportions in the population model.
#' 
#' @param df Data frame of the data you intend on weighting.
#' @param pop.model Full weighting design created by \code{wgt_design()}.
#' @param plot Display plot, default = FALSE.
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
#'     pop.model = mod,
#'     plot = TRUE
#' )
#' 
#' @export
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
            dplyr::group_by(buckets) %>%
            dplyr::summarise(uwgt_n = dplyr::n()) %>%
            dplyr::mutate(uwgt_prop = uwgt_n / sum(uwgt_n)) %>%
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
        dplyr::select(one_of(design$wgt_cat)) %>%
        
        # to maintain metadata
        purrr::map(tibble::as_tibble) %>%
        purrr::map(purrr::set_names, "buckets") %>%
        
        # do the calcs
        purrr::map(calcs) %>%
        
        # to bind all together
        add_wgt_cat() %>%
        purrr::map(dplyr::group_by, wgt_cat) %>%
        purrr::map(tidyr::nest, .key = "uwgt") %>%
        dplyr::bind_rows()
    
    # step 3: create final output ----
    out <-

        # join population model
        uwgt %>%
        dplyr::left_join(design, by = "wgt_cat") %>%

        # join data from population model to unweighted props
        # calculate difference from unweighted to target
        dplyr::mutate(comb = purrr::map2(uwgt, data, dplyr::left_join, by = "buckets")) %>%
        dplyr::select(wgt_cat, comb) %>%
        dplyr::mutate(comb = purrr::map(comb, function(x)
            x %>%
                dplyr::mutate(uwgt_diff = uwgt_prop - targ_prop))
        ) %>%

        # unnest results
        tidyr::unnest(comb) %>%
        dplyr::rename(bucket = buckets)

    out
    
    # step 4: plot ----
    if (isTRUE(plot)) {
        print(
            out %>%

                # begin plot
                ggplot2::ggplot(aes(x = as.character(bucket))) +

                # errorbars
                ggplot2::geom_errorbar(
                    aes(ymin = targ_prop,
                        ymax = targ_prop),
                    lty = "longdash",
                    color = "#4b4b4b"
                ) +

                # points
                ggplot2::geom_point(
                    aes(y = uwgt_prop),
                    size = 3,
                    color = "#d10000"
                ) +

                # adjust scales, use 0 to max of uwgt/targ props
                ggplot2::scale_y_continuous(
                    breaks = pretty,
                    limits = c(0, max(out$uwgt_prop, out$targ_prop))
                ) +

                # facet plots, independent y (eventually x) axes
                ggplot2::facet_wrap(~wgt_cat, scales = "free_y") +

                # tweak labels
                ggplot2::labs(
                    x = NULL,
                    y = "Proportion",
                    color = NULL
                ) +

                # add title
                ggplot2::ggtitle(
                    "Unweighted vs. Target Proportions",
                    "Dashed line = target"
                ) +

                # final theming
                ggplot2::coord_flip() +
                ggplot2::theme_bw() +
                ggplot2::theme(strip.background = element_rect(fill = "#fff6b5"))
        )
    }
    
    return(out)
    
}