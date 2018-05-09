# setup ----
library(data.table)
library(magrittr)
library(tidyverse)
library(crayon)
library(mpace)

source("./03-approved-code/pop_model.r")
source("./03-approved-code/wgt_cat.r")
source("./02-test-code/missing_data_adjustment.r")
source("./02-test-code/pre_rake.r")
source("./02-test-code/iterake.r")
source("./02-test-code/post_rake.r")

# load data ----
df_1 <- read_rds("./data-test/df_1.rds")
df_2 <- read_rds("./data-test/df_2.rds")
df_3 <- read_rds("./data-test/df_3.rds")

map_int(list(df_1, df_2, df_3), nrow)

# existing proportions for the big dataset, use these actual props
# to build pop model to be used for all three datasets
map(df_1[, -1], table) %>% 
    map(prop.table)

# set up iterake population model ----
mod <- pop_model(
    wgt_cat(name = "v1", 
            value = c(1, 2, 3), 
            targ.prop = c(0.6, 0.25, 0.15)),
    wgt_cat(name = "v2", 
            value = c(1, 2),
            targ.prop = c(0.5, 0.5)),
    wgt_cat(name = "v3",
            value = c(1, 2, 3, 4, 5),
            targ.prop = c(0.25, 0.15, 0.28, 0.18, 0.14)),
    wgt_cat(name = "v4",
            value = c(1, 2, 3, 4),
            targ.prop = c(0.2, 0.5, 0.15, 0.15))
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
pre_rake(df_1, mod)
wgt <- iterake(df_1, id, mod)
post_rake(wgt, weight, mod)

iterake(df = df_1, 
        id = id,
        pop.model = mod, 
        wgt.lim = 3, 
        max.iter = 2000)

wgt_check(wgt_rake(df_1, design))


iterake(df = df_2, 
        id = id,
        pop.model = mod, 
        wgt.lim = 3, 
        max.iter = 2000)

wgt_check(wgt_rake(df_2, design))


iterake(df = df_3, 
        id = id,
        pop.model = mod, 
        wgt.lim = 3, 
        max.iter = 2000)

wgt_check(wgt_rake(df_3, design))
