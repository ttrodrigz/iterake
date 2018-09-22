library(data.table)
library(tidyverse)
library(mpace)
library(readr)
library(iterake)

# Testing iterake vs. mpace/anesrake

# load data
fake <- read_rds("./data-for-testing/test_data.rds")

# set up things using tidywgt/iterake
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

wgt_review(df = fake, design = mod)
weights <- iterake(fake, id, mod, threshold = 1e-15)
wgt_review(weights, mod, weight)

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
mod2 <- wgt_design(df = fakempace,
    
    # age category
    wgt_cat(name = "age",
            buckets = c(1, 2, 3),
            targets = c(0.300, 0.360, 0.340)),
    
    # gender category
    wgt_cat(name = "gender",
            buckets = c(1, 2),
            targets = c(0.500, 0.500)),
    
    # vehicle category
    wgt_cat(name = "vehicle",
            buckets = c(1, 2, 3),
            targets = c(0.400, 0.450, 0.150))
    
)

wgt_review(df = fakempace, design = mod2)
weights2 <- iterake(fakempace, id, mod2, threshold = 1e-15)
wgt_review(weights2, mod2, weight)

# vector of differences
diff2 <- weights2$weight - weightsmpace$weight
summary(diff2)
sum(abs(diff2))


# test with missing data - compare to mpace/anesrake

testpace <- fakempace
testpace[1:5, 2] <- NA

mod3 <- wgt_design(df = testpace,
                  
                  # age category
                  wgt_cat(name = "age",
                          buckets = c(1, 2, 3),
                          targets = c(0.300, 0.360, 0.340)),
                  
                  # gender category
                  wgt_cat(name = "gender",
                          buckets = c(1, 2),
                          targets = c(0.500, 0.500)),
                  
                  # vehicle category
                  wgt_cat(name = "vehicle",
                          buckets = c(1, 2, 3),
                          targets = c(0.400, 0.450, 0.150))
                  
)

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
wgt_review(df = testpace, design = mod3)
weights_missing <- iterake(testpace, id, mod3, threshold = 1e-15)
wgt_review(weights_missing, mod3, weight)

# vector of differences
diff_missing <- weights_missing$weight - weightsmpace_missing$weight
summary(diff_missing)
sum(abs(diff_missing)) # quite a bit off, more than usual anyway

# vector of differences
diff_missing2 <- weights_missing$weight - weightsmpace_missing2$weight
summary(diff_missing2)
sum(abs(diff_missing2)) # this looks more like it

# some oddities on how anesrake handles missing data... but pretty close all in all
## Here is a test for filling targets from data

# load data ----
data("weight_me")

# do it the mpace way
# build weight design ----
designMpace <- wgt_design(id = "id",
                          wgt_name = "weight",
                          data = weight_me %>% filter(group == 2),
                          prev_wgt = "origWeight",
                          targets = list(gender = c(),
                                         vehicle = c()))


# check target/actual props ----
wgt_tab(weight_me %>% filter(group == 1), designMpace)

# weight the data ----
weightsMpace <- wgt_rake(weight_me %>% filter(group == 1), designMpace)

# check target/actual/weighted props ----
wgt_tab(weight_me %>% filter(group == 1), designMpace, weightsMpace)

# how'd it do? ----
wgt_check(weightsMpace)

# do it the iterake way
designIterake <- wgt_design(df = weight_me %>% filter(group == 1),
                           
                           # gender category
                           wgt_cat_inherit(name = "gender",
                                           df = weight_me %>% filter(group == 2),
                                           prev.wgt = origWeight),
                           
                           # vehicle category
                           wgt_cat_inherit(name = "vehicle",
                                           df = weight_me %>% filter(group == 2),
                                           prev.wgt = origWeight)
)

wgt_review(df = weight_me %>% filter(group == 1), design = designIterake)
weightsIterake <- iterake(weight_me %>% filter(group == 1), id, designIterake, threshold = 1e-15)
wgt_review(weightsIterake, designIterake, weight)

# using included data tests...
library(iterake)

data(weight_me)
mod <- wgt_design(
    df = weight_me,
    
    wgt_cat(
        name = "costume",
        buckets = c("Bat Man", "Cactus"),
        targets = c(0.5, 0.5)),
    
    wgt_cat(
        name = "seeds",
        buckets = c("Tornado", "Bird", "Earthquake"),
        targets = c(0.4, 0.3, 0.3))
)

wgt_review(weight_me, mod, plot = T)
wgt <- iterake(weight_me, order, mod)
wgt_review(wgt, mod, weight, plot = T)


test <- weight_me
test$logical <- ifelse(test$costume == "Bat Man", TRUE, FALSE)
test$number <- as.numeric(as.factor(test$seeds))

mod2 <- wgt_design(
    df = test,
    
    wgt_cat(
        name = "logical",
        buckets = c(TRUE, FALSE),
        targets = c(0.5, 0.5)),
    
    wgt_cat(
        name = "number",
        buckets = c(1, 2, 3),
        targets = c(0.3, 0.3, 0.4))
)

wgt_review(test, mod2)
wgt2 <- iterake(test, order, mod)
wgt_review(wgt2, mod2, weight)
