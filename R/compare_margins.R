#' Compare unweighted or weighted data to targets
#' 
#' Inspect by what degree the unweighted or weighted sample proportions deviate from the
#' target proportions.
#' 
#' @param df Data frame of the data you intend on weighting.
#' @param universe Full weighting design created by \code{universe()}.
#' @param weight Name of weight variable, optional.
#' @param plot Display plot, default = FALSE.
#' 
#' @importFrom dplyr select group_by summarise mutate mutate_at vars funs ungroup n bind_rows left_join rename one_of
#' @importFrom purrr map map2 set_names
#' @importFrom rlang !! :=
#' @importFrom tibble as_tibble
#' @importFrom tidyr nest unnest
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_point scale_y_continuous facet_wrap labs element_rect ggtitle coord_flip theme_bw theme scale_color_manual
#' 
#' @return A tibble of unweighted counts and proportions, difference between 
#' unweighted and target proportions. If \code{`weight`} is given, it also includes
#' weighted counts and proportions, difference between weighted and target proportions.
#' Optionally, a plot of this information.
#' 
#' @examples 
#' data(weight_me)
#' 
#' mod <- universe(
#' 
#'     df = weight_me,
#' 
#'     category(
#'         name = "costume",
#'         buckets = c("Bat Man", "Cactus"),
#'         targets = c(0.5, 0.5)),
#' 
#'     category(
#'         name = "seeds",
#'         buckets = c("Tornado", "Bird", "Earthquake"),
#'         targets = c(0.3, 0.3, 0.4))
#' )
#' 
#' compare_margins(
#'     df = weight_me,
#'     universe = mod,
#'     plot = TRUE
#' )
#' 
#' wgts <- iterake(
#'     df = weight_me,
#'     universe = mod
#' )
#' 
#' compare_margins(
#'     df = wgts,
#'     universe = mod,
#'     weight = weight,
#'     plot = FALSE)
#'
#' @export
compare_margins <- function(df, weight, universe, plot = FALSE) {
    
    # make sure you've got a df
    if (!is.data.frame(df)) {
        stop("`df` must be a dataframe.")
    }
    
    if (!"universe" %in% class(universe)) {
        stop("'universe' must be of class 'universe', rerun universe()")
    }
    
    # check to see if an `N` was supplied to universe
    if ("targ_n" %in% names(universe$data[[1]])) {
        uni_n <- TRUE
    } else {
        uni_n <- FALSE
    }
    
    df.names <- names(df)
    
    # category here refers to the variable created in category() that identifies a target weighting variable
    mod.names <- universe$category
    bad.names <- mod.names[!mod.names %in% df.names]
    
    if (length(bad.names) > 0) {
        stop(
            paste(
                "Each weighting category in `universe` must have a matching column name in `df`. The following weighting cagegories have no match:",
                paste(bad.names, collapse = ", "),
                sep = "\n"
            )
        )
    }
    
    # deal with weights possibly being/not being provided
    if (missing(weight)) {
        wgt <- 1
        tmp <-
            df %>%
            mutate(weight_var = 1) %>%
            select(weight_var,
                   one_of(universe$category))
    } else {
        if (!deparse(substitute(weight)) %in% names(df)) {
            stop(paste0("Weight variable '", deparse(substitute(weight)), "' not found in data."))
        }
        
        wgt <- enquo(weight)
        tmp <-
            df %>%
            select(!! wgt,
                   one_of(universe$category)) %>%
            rename(weight_var := !! wgt)
        
    }
    
    # prepare data by nesting
    nested <- 
        tmp %>%
        mutate_at(vars(-weight_var), funs(as.character)) %>%
        gather(category, buckets, -weight_var) %>%
        group_by(category) %>%
        nest()
    
    prop.calcs <- function(x) {
        
        x %>%
            # DID THIS SO ANY VARIABLE TYPE WILL UNNEST (AKA JOIN) TOGETHER
            mutate(buckets = as.character(buckets)) %>%
            group_by(buckets) %>%
            summarise(uwgt_n = n(),
                      wgt_n = sum(weight_var)) %>%
            ungroup() %>%
            mutate(uwgt_prop = uwgt_n / sum(uwgt_n),
                   wgt_prop  = wgt_n  / sum(wgt_n))
        
    }
    
    # calculate all data
    calcd <-
        
        nested %>%
        
        # calculate props and unnest
        mutate(props = map(data, prop.calcs)) %>%
        unnest(props) %>%
        
        # join in targets from universe
        left_join(
            y = unnest(universe %>% 
                           mutate(data = map(data, function(x) {
                               x$buckets <- as.character(x$buckets)
                               x}))
            ), 
            by = c("category", "buckets")
        ) %>%
        
        # calculate differences
        mutate(uwgt_diff = uwgt_prop - targ_prop,
               wgt_diff  = wgt_prop - targ_prop) %>%
        
        # better names
        rename(
            bucket   = buckets
        )
    
    # modify output based on whether or not weights are provided
    if (missing(weight)) {
        
        calcd <-
            calcd %>%
            select(-wgt_n, -wgt_prop, -wgt_diff)
        
    }
    
    # drop `targ_n` if it exists
    if (uni_n) {
        
        calcd <-
            calcd %>%
            select(-targ_n)
    }
    
    if (isTRUE(plot)) {
        
        # prepare chart_data based on weight being given
        if (missing(weight)) {
            chart_data <- calcd
        } else {
            chart_data <-
                calcd %>%
                group_by(category) %>%
                gather(wgt_type, wgt_val, -c(category:wgt_n, targ_prop, uwgt_diff, wgt_diff)) %>%
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
                       limits = c(0, max(chart_data$wgt_val))
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
    
    return(calcd)
}

utils::globalVariables(c("buckets", "data", "uwgt_n", "uwgt_prop", "targ_prop", "weight_var", "props", "wgt_prop", "wgt_diff", "wgt_type", "wgt_val", "uwgt_diff", "category", "bucket", "targ_n"))    
