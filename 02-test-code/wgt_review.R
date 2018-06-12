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
            mutate(buckets == as.character(buckets)) %>%
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
        mutate(uwgt_diff = targ_prop - uwgt_prop,
               wgt_diff  = targ_prop - wgt_prop)
    
    # modify output based on whether or not weights are provided
    if (missing(weight)) {
        
        calcd <-
            calcd %>%
            select(-wgt_n, -wgt_prop, -wgt_diff)
        
    }
    
    return(calcd)
}

wgt_review(fake, mod)
out <- iterake(fake, id, mod)
wgt_review(out, mod, weight)

