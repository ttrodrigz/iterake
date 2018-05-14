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

pre_rake(df = fake, pop.model = mod)
weights <- iterake(fake, id, mod, threshold = 1e-15)
post_rake(weights, weight, mod)

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
wgt_check(weightsmpace)

# vector of differences
diff <- weights$weight - weightsmpace$weight
summary(diff)
sum(abs(diff))

# test iterake w/ numeric data from mpace test
mod2 <- pop_model(df = testpace,
    
    # age category
    wgt_cat(name = "age",
            value = c(1, 2, 3),
            targ.prop = c(0.300, 0.360, 0.340)),
    
    # gender category
    wgt_cat(name = "gender",
            value = c(1, 2),
            targ.prop = c(0.500, 0.500)),
    
    # vehicle category
    wgt_cat(name = "vehicle",
            value = c(1, 2, 3),
            targ.prop = c(0.400, 0.450, 0.150))
    
)

pre_rake(df = fakempace, pop.model = mod2)
weights2 <- iterake(fakempace, id, mod2, threshold = 1e-15)
post_rake(weights2, weight, mod2)

# vector of differences
diff2 <- weights2$weight - weightsmpace$weight
summary(diff2)
sum(abs(diff2))


# test with missing data - compare to mpace/anesrake

testpace <- fakempace
testpace[1:5, 2] <- NA

# mpace/anesrake
wgt_tab(testpace, design) # error stemming from NAs
weightsmpace_missing <- wgt_rake(testpace, design) # but oddly enough this works...
wgt_check(weightsmpace_missing)

# new design with bin category for NAs
design2 <- wgt_design(id = "id",
                      wgt_name = "weight",
                      targets = list(age = c(0.2925, 0.3510, 0.3315, 0.0250),
                                     gender = c(0.500, 0.500),
                                     vehicle = c(0.400, 0.450, 0.150)))
wgt_tab(testpace, design2) # works now
weightsmpace_missing2 <- wgt_rake(testpace, design) # but now this crashes...
#wgt_check(weightsmpace_missing)

# maybe recode NAs to their own group (4)?
newtest <- testpace %>%
    mutate(age = ifelse(is.na(age), 4, age))
wgt_tab(newtest, design2) # works now
weightsmpace_missing2 <- wgt_rake(newtest, design2) # works again
wgt_check(weightsmpace_missing2)


# tidywgt/iterake
pre_rake(df = testpace, pop.model = mod2)
weights_missing <- iterake(testpace, id, mod2, threshold = 1e-15)
post_rake(weights_missing, weight, mod2)

# vector of differences
diff_missing <- weights_missing$weight - weightsmpace_missing$weight
summary(diff_missing)
sum(abs(diff_missing)) # quite a bit off, more than usual anyway

# vector of differences
diff_missing2 <- weights_missing$weight - weightsmpace_missing2$weight
summary(diff_missing2)
sum(abs(diff_missing2)) # this looks more like it

# some oddities on how anesrake handles missing data... but pretty close all in all
