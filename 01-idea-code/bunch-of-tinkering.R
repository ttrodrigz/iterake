# test that iterake can properly handle factors

# set up ----
library(tidyverse)
library(crayon)

source("./03-approved-code/pop_model.r")
source("./03-approved-code/wgt_cat.r")
source("./02-test-code/missing_data_adjustment.r")
source("./02-test-code/iterake.r")

# this will coerce char to fact (visa versa) ----

# data/pop.model prep ----
df_1 <- 
    read_rds("./data-test/df_1.rds") %>%
    mutate(
        v2 = case_when(
            v2 == 1 ~ "male",
            v2 == 2 ~ "female"),
        v2 = factor(
            x = v2, 
            levels = c("female", "male"), 
            ordered = TRUE)
    )

mod <- pop_model(
    wgt_cat(name = "v1", 
            value = c(1, 2, 3), 
            targ.prop = c(0.6, 0.25, 0.15)),
    wgt_cat(name = "v2", 
            value = c("male", "female"),
            targ.prop = c(0.5, 0.5)),
    wgt_cat(name = "v3",
            value = c(1, 2, 3, 4, 5),
            targ.prop = c(0.25, 0.15, 0.28, 0.18, 0.14)),
    wgt_cat(name = "v4",
            value = c(1, 2, 3, 4),
            targ.prop = c(0.2, 0.5, 0.15, 0.15))
)

# USE THIS TO MAP OVER LIST OF DF'S
calcs <- function(x) {
    
    calc <- x %>%
        group_by(value) %>%
        summarise(n = n()) %>%
        ungroup() %>%
        mutate(uwgt_prop = n / sum(n)) %>%
        ungroup()
    
}

# USE THIS TO MAKE LIST NAME INTO WGT_CAT VARIABLE
add_wgt_cat <- function(df_list) {
    
    for (i in seq_along(df_list)) {
        
        df_list[[i]]$wgt_cat <- names(df_list[i])
        
    }
    
    df_list
}

# THIS IS GOOD RIGHT HERE
uwgt_props <- df_1 %>%
    select(one_of(mod$wgt_cat)) %>%
    map(as_tibble) %>%
    map(calcs) %>%
    add_wgt_cat() %>%
    map(group_by, wgt_cat) %>%
    map(nest, .key = "uwgt") %>%
    bind_rows()

# THIS IS ALSO GOOD!
(left_join(uwgt_props, mod, by = "wgt_cat") %>%
        mutate(full = map2(uwgt, data, left_join, by = "value")))$full[[2]]
