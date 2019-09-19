# set up ----
library(iterake)
library(tidyverse)

fake <- readr::read_rds("./data-for-testing/test_data.rds")
mod <- wgt_design(df = fake,
                  
                  # age category
                  wgt_cat(name = "age",
                          buckets = c("18-34", "35-54", "55+"),
                          targets = c(0.300, 0.360, 0.340)),
                  
                  # gender category
                  wgt_cat(name = "gender",
                          buckets = c("Female", "Male"),
                          targets = c(0.500, 0.500)),
                  
                  # vehicle category
                  wgt_cat(name = "vehicle",
                          buckets = c("Car", "SUV", "Truck"),
                          targets = c(0.400, 0.450, 0.150))
                  
)

# new function ----
wgt_review <- function(df, design, weight, plot = FALSE) {
    
    # make sure you've got a df
    if (!is.data.frame(df)) {
        stop("`df` must be a dataframe.")
    }
    
    # deal with weights possibly being/not being provided
    if (missing(weight)) {
        wgt <- 1
        tmp <-
            df %>%
            mutate(weight_var = 1) %>%
            select(weight_var,
                   one_of(design$wgt_cat))
    } else {
        if (!deparse(substitute(weight)) %in% names(df)) {
            stop(paste0("Weight variable '", deparse(substitute(weight)), "' not found in data."))
        }
        
        wgt <- enquo(weight)
        tmp <-
            df %>%
            select(!! wgt,
                   one_of(design$wgt_cat)) %>%
            rename(weight_var := !! wgt)

    }
    
    # prepare data by nesting
    nested <- 
        tmp %>%
        gather(wgt_cat, buckets, -weight_var) %>%
        group_by(wgt_cat) %>%
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
        
        # join in targets from design
        left_join(
            y = unnest(design) %>% 
                mutate(buckets = as.character(buckets)), 
            by = c("wgt_cat", "buckets")
        ) %>%
        
        # calculate differences
        # mutate(uwgt_diff = targ_prop - uwgt_prop,
        #        wgt_diff  = targ_prop - wgt_prop)
        mutate(uwgt_diff = uwgt_prop - targ_prop,
               wgt_diff  = wgt_prop - targ_prop) %>%
        tibble(tibble-options())
    
    
    # modify output based on whether or not weights are provided
    if (missing(weight)) {
        
        calcd <-
            calcd %>%
            select(-wgt_n, -wgt_prop, -wgt_diff)
        
    }
    
    if (isTRUE(plot)) {
        
        if (missing(weight)) {
            chart_data <- calcd
        } else {
            chart_data <-
                calcd %>%
                dplyr::group_by(wgt_cat) %>%
                tidyr::gather(wgt_type, wgt_val, -c(wgt_cat:wgt_n, targ_prop, uwgt_diff, wgt_diff)) %>%
                dplyr::mutate(wgt_type = gsub("uwgt_prop", "Unweighted", wgt_type),
                              wgt_type = gsub("wgt_prop", "Weighted", wgt_type))
        }

        chart <- 
            chart_data %>%
            
            # begin plot
            ggplot2::ggplot(aes(x = as.character(buckets))) +
            
            # errorbars
            ggplot2::geom_errorbar(
                aes(ymin = targ_prop,
                    ymax = targ_prop),
                lty = "longdash",
                color = "#4b4b4b"
            )
        
        ifelse(missing(weight),
               
               chart <- 
                   chart + 
                   
                   # points
                   ggplot2::geom_point(
                       aes(y = uwgt_prop),
                       size = 3,
                       color = "#d10000"
                   ) + 
                   
                   # adjust scales, use 0 to max of uwgt/targ props
                   ggplot2::scale_y_continuous(
                       breaks = pretty,
                       limits = c(0, max(chart_data$uwgt_prop, chart_data$targ_prop))
                   )
               ,
               
               chart <- 
                   chart +
                   
                   # points
                   ggplot2::geom_point(
                       aes(y = wgt_val,
                           color = wgt_type),
                       size = 3
                   ) +
                   
                   # set point colors
                   ggplot2::scale_color_manual(
                       values = c("Unweighted" = "#d10000", 
                                  "Weighted" = "#006fd1")
                   ) +
                   
                   # adjust scales, use 0 to max of wgt_val props
                   ggplot2::scale_y_continuous(
                       breaks = pretty,
                       limits = c(0, max(chart_data$wgt_val))
                   )
        )
        
        chart <- 
            chart +
            
            # facet plots, independent y (eventually x) axes
            ggplot2::facet_wrap(~wgt_cat, scales = "free_y") +
            
            # tweak labels
            ggplot2::labs(
                x = NULL,
                y = "Proportion",
                color = NULL
            )
        
        ifelse(missing(weight),
               
               chart <- 
                   chart +
                   
                   # add title
                   ggplot2::ggtitle(
                       "Unweighted vs. Target Proportions",
                       "Dashed line = target"
                   )
               ,
               
               chart <- 
                   chart +
                   
                   # add title
                   ggplot2::ggtitle(
                       "Unweighted and Weighted vs. Target Proportions",
                       "Dashed line = target")
        )

        # final theming
        chart <- 
            chart +
            ggplot2::theme_bw() +
            ggplot2::theme(strip.background = element_rect(fill = "#fff6b5")) +
            ggplot2::coord_flip()

        print(chart)
    }
    
    return(calcd)
}
pre_rake(fake, mod)
wgt_review(fake, mod, plot = F)
wgt_review(fake, mod, plot = T)
out <- iterake(fake, id, mod)
wgt_review(out, mod, weight, plot = T)

