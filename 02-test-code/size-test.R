# setup ----
library(tidyverse)
library(mpace)
library(iterake)

# load data ----
df_1 <- readr::read_rds("./data-for-testing/df_1.rds")
df_2 <- readr::read_rds("./data-for-testing/df_2.rds")
df_3 <- readr::read_rds("./data-for-testing/df_3.rds")

purrr::map_int(list(df_1, df_2, df_3), nrow)

# existing proportions for the big dataset, use these actual props
# to build pop model to be used for all three datasets
purrr::map(df_1[, -1], table) %>% 
    map(prop.table)

# set up iterake population model ----
mod1 <- universe(
    df = df_1,
    
    category(name = "v1",
                 buckets = c(1, 2, 3),
                 targets = c(0.6, 0.25, 0.15)),
    
    category(name = "v2", 
                 buckets = c(1, 2),
                 targets = c(0.5, 0.5)),
    
    category(name = "v3",
                 buckets = c(1, 2, 3, 4, 5),
                 targets = c(0.25, 0.15, 0.28, 0.18, 0.14)),
    
    category(name = "v4",
                 buckets = c(1, 2, 3, 4),
                 targets = c(0.2, 0.5, 0.15, 0.15))
)

mod2 <- universe(
    df = df_2,
    
    category(name = "v1",
                 buckets = c(1, 2, 3),
                 targets = c(0.6, 0.25, 0.15)),
    
    category(name = "v2", 
                 buckets = c(1, 2),
                 targets = c(0.5, 0.5)),
    
    category(name = "v3",
                 buckets = c(1, 2, 3, 4, 5),
                 targets = c(0.25, 0.15, 0.28, 0.18, 0.14)),
    
    category(name = "v4",
                 buckets = c(1, 2, 3, 4),
                 targets = c(0.2, 0.5, 0.15, 0.15))
)

mod3 <- universe(
    df = df_3,
    
    category(name = "v1",
                 buckets = c(1, 2, 3),
                 targets = c(0.6, 0.25, 0.15)),
    
    category(name = "v2", 
                 buckets = c(1, 2),
                 targets = c(0.5, 0.5)),
    
    category(name = "v3",
                 buckets = c(1, 2, 3, 4, 5),
                 targets = c(0.25, 0.15, 0.28, 0.18, 0.14)),
    
    category(name = "v4",
                 buckets = c(1, 2, 3, 4),
                 targets = c(0.2, 0.5, 0.15, 0.15))
)

# setup mpace design ----
design <- wgt_design(
    id = "id", 
    wgt_lim = 3,
    wgt_name = "weight",
    targets = list(v1 = c(0.6, 0.25, 0.15),
                   v2 = c(0.5, 0.5),
                   v3 = c(0.25, 0.15, 0.28, 0.18, 0.14),
                   v4 = c(0.2, 0.5, 0.15, 0.15)))

# run iterake on three datasets ----

### DEVON!! LOOK HERE!!
compare_margins(df_1, mod1)
wgt <- iterake(df_1, mod1)
compare_margins(wgt, mod1, weight)

system.time(
    iterake(df = df_1,
            design = mod1, 
            wgt.lim = 3, 
            max.iter = 2000)
)

system.time(
    wgt_rake(df_1, design)
)

wgt_check(wgt_rake(df_1, design))

system.time(
    iterake(df = df_2, 
            design = mod2, 
            wgt.lim = 3, 
            max.iter = 2000)
)

system.time(
    wgt_rake(df_2, design)
)
wgt_check(wgt_rake(df_2, design))

system.time(
iterake(df = df_3, 
        design = mod3, 
        wgt.lim = 3, 
        max.iter = 2000))

system.time(
    wgt_rake(df_3, design)
)

wgt_check(wgt_rake(df_3, design))
