library(tidyverse)

fake1 <- readr::read_rds("./data-for-testing/test_data.rds")

fake2 <-
    fake1 %>%
    mutate(gender = ifelse(gender == "Female", 1, 0),
           vehicle = as.factor(vehicle))

mod1 <- wgt_design(
    
    df = fake1,
    
    # wgt_cat(name = "age",
    #         buckets = c("18-34", "35-54", "55+"),
    #         targets = c(0.333, 0.333, 0.334)),
    
    wgt_cat(name = "gender",
            buckets = c("Female", "Male"),
            targets = c(0.50, 0.5)),
    
    wgt_cat(name = "vehicle",
            buckets = c("Car", "SUV", "Truck"),
            targets = c(0.3, 0.55, 0.15))
)

# recoded ----
mod2 <- wgt_design(
    
    df = fake2,
    
    wgt_cat(name = "age",
            buckets = c("18-34", "35-54", "55+"),
            targets = c(0.333, 0.333, 0.334)),
    
    wgt_cat(name = "gender",
            buckets = c(1, 2),
            targets = c(0.5, 0.5)),
    
    wgt_cat(name = "vehicle",
            buckets = c("Car", "SUV", "Truck"),
            targets = c(0.3, 0.55, 0.15))
)

# testing ----
wgt_review(df = fake1, design = mod1)
wgt_review(df = fake2, design = mod2)

out1 <- iterake(df = fake1, id = id, design = mod1, max.iter = 1000)
