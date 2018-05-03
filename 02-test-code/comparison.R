library(magrittr)
library(tidyverse)
library(crayon)
library(mpace)

source("./02-test-code/pop_model.r")
source("./02-test-code/wgt_cat.r")
source("./01-idea-code/pre_rake.r")
source("./02-test-code/iterake.r")
source("./01-idea-code/post_rake.r")

# Testing iterake vs. mpace/anesrake

# load data
fake <- read_rds("./data/test_data.rds")

# set up things using tidywgt/iterake
mod <- pop_model(
    
    # age category
    wgt_cat(name = "age",
            value = c("18-34", "35-54", "55+"),
            targ.prop = c(0.300, 0.360, 0.340)),
    
    # gender category
    wgt_cat(name = "gender",
            value = c("Female", "Male"),
            targ.prop = c(0.500, 0.500)),
    
    # vehicle category
    wgt_cat(name = "vehicle",
            value = c("Car", "SUV", "Truck"),
            targ.prop = c(0.400, 0.450, 0.150))
    
)

pre_rake(data = fake, pop.model = mod, deviance = 0.02)
weights <- iterake(fake, id, mod, threshold = 1e-15)

# set up things using mpace/anesrake
# recode to be all numeric
fakempace <- fake %>%
    mutate(age = ifelse(age == "18-34", 1, ifelse(age == "35-54", 2, 3)),
           gender = ifelse(gender == "Female", 1, 2),
           vehicle = ifelse(vehicle == "Car", 1, ifelse(vehicle == "SUV", 2, 3)))

design <- wgt_design(id = "id",
                     wgt_name = "weight",
                     targets = list(age = c(0.300, 0.360, 0.340),
                                    gender = c(0.500, 0.500),
                                    vehicle = c(0.400, 0.450, 0.150)))

wgt_tab(fakempace, design)
weightsmpace <- wgt_rake(fakempace, design)

# vector of differences
diff <- weights$weight - weightsmpace$weight
sum(diff)
sum(abs(diff))
max(abs(diff))
min(abs(diff))

# some piping tests
test <- fake %T>%
    pre_rake(mod) %>%
    iterake(id, mod) %T>%
    post_rake(weight, mod)
