# test that iterake can properly handle factors

# set up ----
library(tidyverse)
library(crayon)

source("./03-approved-code/pop_model.r")
source("./03-approved-code/wgt_cat.r")
source("./02-test-code/missing_data_adjustment.r")
source("./02-test-code/pre_rake.r")
source("./02-test-code/iterake.r")
source("./02-test-code/post_rake.r")

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

# above is good, now pre_rake is breaking for same bind_rows() issues ----
pre_rake(df_1, mod)

iterake(df_1, id, mod)
