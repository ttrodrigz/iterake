#' Compare unweighted or weighted data to targets
#' 
#' Inspect by what degree the unweighted or weighted sample proportions deviate from the
#' target proportions.
#' 
#' @param universe Full weighting design created by \code{universe()}.
#' @param data Data frame that includes weight from \code{iterake()}, optional.
#' @param weight Name of weight variable in optional \code{`data`} object, optional.
#' @param plot Display plot, default = FALSE.
#' 
#' @importFrom dplyr select group_by mutate mutate_at vars funs bind_rows rename one_of everything
#' @importFrom data.table data.table setnames
#' @importFrom rlang !! :=
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point scale_y_continuous facet_wrap labs element_rect ggtitle coord_flip theme_bw theme scale_color_manual
#' @importFrom tidyr pivot_longer
#' @importFrom tibble as_tibble
#' 
#' @return A tibble of unweighted counts and proportions, difference between 
#' unweighted and target proportions. If \code{`data`} and \code{`weight`} is given, it also includes
#' weighted counts and proportions, difference between weighted and target proportions.
#' Optionally, a plot of this information.
#' 
#' @examples 
#' data(demo_data)
#' 
#' mod <- universe(
#'         data = demo_data,
#'         
#'         category(
#'             name = "Sex",
#'             buckets = factor(
#'                 x = levels(demo_data[["Sex"]]),
#'                 levels = levels(demo_data[["Sex"]])
#'             ),
#'             targets = c(0.4, 0.5),
#'             sum.1 = TRUE
#'         ),
#' 
#'         category(
#'             name = "BirthYear",
#'             buckets = c(1986:1990),
#'             targets = rep(0.2, times = 5)
#'         ),
#'     
#'         category(
#'             name = "EyeColor",
#'             buckets = c("brown", "green", "blue"),
#'             targets = c(0.8, 0.1, 0.1)
#'         ),
#'     
#'         category(
#'             name = "HomeOwner",
#'             buckets = c(TRUE, FALSE),
#'             targets = c(3/4, 1/4)
#'         )
#'     )
#' 
#' compare_margins(
#'     universe = mod,
#'     plot = TRUE
#' )
#' 
#' wgts <- iterake(
#'     universe = mod
#' )
#' 
#' compare_margins(
#'     universe = mod,
#'     data = wgts,
#'     weight = weight,
#'     plot = FALSE
#' )
#'
#' @export
compare_margins <- function(universe, data, weight, plot = FALSE) {
    
    if (!"universe" %in% class(universe)) {
        stop("'universe' must be of class 'universe', rerun universe()")
    }
    
    # grab relevant data frame
    if (!missing(weight) & !missing(data)) {
        if (!is.data.frame(data)) {
            stop("`data` must be a dataframe.")
        }
        
        df <- data
    } else {
        
        df <- universe[["data"]]
    }

    df.names <- names(df)
    
    mod.names <- names(universe[["universe"]])
    
    # this can all likely be eliminated once compare_margins only
    # looks to universe for data, as this check will already have happened
    bad.names <- mod.names[!mod.names %in% df.names]
    
    if (length(bad.names) > 0) {
        
        stop.message <- glue(
            "
            Each name given to a weighting category in `universe()` must have a matching column name in `df`. The following weighting categories have no match:
            ",
            glue_collapse(bad.names, sep = ", "),
            .sep = "\n"
        )
        
        stop(stop.message)
        
    }

    # deal with weights possibly being/not being provided
    if (missing(weight)) {
        wgt <- 1
        tmp <-
            df %>%
            mutate(weight_var = 1) %>%
            select(weight_var,
                   one_of(mod.names))
    } else {
        if (!deparse(substitute(weight)) %in% names(df)) {
            stop(glue("Weight variable '{deparse(substitute(weight))}' not found in data."))
        }
        
        wgt <- enquo(weight)
        tmp <-
            df %>%
            select(!! wgt,
                   one_of(mod.names)) %>%
            rename(weight_var := !! wgt)
        
    }
    
    calcd <- NA

    for (i in seq_along(mod.names)) {

        DT_data <- 
            data.table(
                tmp %>% 
                    mutate_at(vars(one_of(mod.names[i])), funs(as.character)) %>% 
                    select(weight_var, one_of(mod.names[i])), 
                key = mod.names[i])
        
        # exclude the first column, not needed
        DT_design <- data.table(universe[["universe"]][[i]])[, c(-1, -4)] %>% mutate(bucket = as.character(bucket))
        
        DT_out <- 
            DT_data[, .(uwgt_n = .N, wgt_n = sum(weight_var)), by = c(mod.names[i])] %>%
                .[, uwgt_prop := uwgt_n / sum(uwgt_n)] %>%
                .[, wgt_prop := wgt_n / sum(wgt_n)] %>%
                .[DT_design] %>%
                .[, uwgt_diff := uwgt_prop - targ_prop] %>%
                .[, wgt_diff := wgt_prop - targ_prop] %>%
                .[, category := mod.names[i]]
        
        setnames(DT_out, mod.names[i], "bucket")

        if (all(is.na(calcd))) {
            calcd <- DT_out
        } else {
            calcd <- bind_rows(calcd, DT_out)
        }

    }
    
    # modify output based on whether or not weights are provided
    if (missing(weight)) {
        
        calcd <-
            calcd %>%
            select(-wgt_n, -wgt_prop, -wgt_diff)
        
    }

    if (isTRUE(plot)) {

        # prepare chart_data based on weight being given
        if (missing(weight)) {
            chart_data <- calcd
        } else {
            chart_data <-
                calcd %>%
                group_by(category) %>%
                pivot_longer(
                    -c(bucket:wgt_n, targ_prop, uwgt_diff, wgt_diff, category), 
                    names_to = "wgt_type", 
                    values_to = "wgt_val"
                ) %>%
                mutate(wgt_type = gsub("uwgt_prop", "Unweighted", wgt_type),
                       wgt_type = gsub("wgt_prop", "Weighted", wgt_type))
        }

        # create chart object
        chart <- 
            chart_data %>%
            
            # begin plot
            ggplot(aes(x = as.character(bucket))) +
            
            # errorbars
            geom_errorbar(
                aes(ymin = targ_prop,
                    ymax = targ_prop),
                lty = "longdash",
                color = "#4b4b4b"
            )
        
        ifelse(missing(weight),
               
               chart <- 
                   chart + 
                   
                   # points
                   geom_point(
                       aes(y = uwgt_prop),
                       size = 3,
                       color = "#d10000"
                   ) + 
                   
                   # adjust scales, use 0 to max of uwgt/targ props
                   scale_y_continuous(
                       breaks = pretty,
                       limits = c(0, max(chart_data$uwgt_prop, chart_data$targ_prop))
                   )
               ,
               
               chart <- 
                   chart +
                   
                   # points
                   geom_point(
                       aes(y = wgt_val,
                           color = wgt_type),
                       size = 3
                   ) +
                   
                   # set point colors
                   scale_color_manual(
                       values = c("Unweighted" = "#d10000", 
                                  "Weighted" = "#006fd1")
                   ) +
                   
                   # adjust scales, use 0 to max of wgt_val props
                   scale_y_continuous(
                       breaks = pretty,
                       limits = c(0, max(chart_data$targ_prop, chart_data$wgt_val))
                   )
        )
        
        chart <- 
            chart +
            
            # facet plots, independent y (eventually x) axes
            facet_wrap(~category, scales = "free_y") +
            
            # tweak labels
            labs(
                x = NULL,
                y = "Proportion",
                color = NULL
            )
        
        ifelse(missing(weight),
               
               chart <- 
                   chart +
                   
                   # add title
                   ggtitle(
                       "Unweighted Sample vs. Universe Proportions",
                       "Dashed line = Universe"
                   )
               ,
               
               chart <- 
                   chart +
                   
                   # add title
                   ggtitle(
                       "Unweighted and Weighted Sample vs. Universe Proportions",
                       "Dashed line = Universe")
        )
        
        # final theming
        chart <- 
            chart +
            theme_bw(base_size = 10) +
            theme(strip.background = element_rect(fill = "#FCEBE4")) +
            coord_flip()
        
        print(chart)
    }
    
    calcd <- 
        calcd %>%
        select(category, everything()) %>%
        as_tibble()
    
    return(calcd)
}

utils::globalVariables(c("buckets", "data", "uwgt_n", "uwgt_prop", "targ_prop", "weight_var", "props", "wgt_prop", "wgt_diff", "wgt_type", "wgt_val", "uwgt_diff", "category", "bucket", ".N"))    
