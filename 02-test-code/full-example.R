library(magrittr)
library(tidyverse)
library(crayon)

source("./02-test-code/pop_model.r")
source("./02-test-code/wgt_cat.r")
source("./01-idea-code/pre_rake.r")
source("./02-test-code/iterake.r")
source("./01-idea-code/post_rake.r")

fake <- read_rds("./data/test_data.rds")
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
raked <- iterake(fake, id, mod, wgt.lim = 4)
no_console <- post_rake(raked, weight, mod, print = FALSE)

fake %T>%
    pre_rake(mod) %>%
    iterake(id, mod) %T>%
    post_rake(weight, mod)

# checking iterake

## wrong data type
iterake(data = list(), pop.model = mod)

## no id
iterake(data = fake, pop.model = mod)

## wrong id
iterake(data = fake, pop.model = mod, id = shit)

## bad wgt.names
iterake(data = fake, pop.model = mod, id = id,
        wgt.name = c("wgt1", "wgt2"))

iterake(data = fake, pop.model = mod, id = id,
        wgt.name = 1)

## bad numeric stuff
iterake(data = fake, pop.model = mod, id = id,
        wgt.lim = "0.5")

iterake(data = fake, pop.model = mod, id = id,
        wgt.lim = 0.5)

iterake(data = fake, pop.model = mod, id = id,
        threshold = "0.5")

iterake(data = fake, pop.model = mod, id = id,
        max.iter = c(1, 1))

iterake(data = fake, pop.model = mod, id = id,
        max.iter = 0)

# will fail
iterake(data = fake, pop.model = mod, id = id,
        wgt.lim = 1.01)

iterake(data = fake, pop.model = mod, id = id,
        max.iter = 1)

# will succeed
wgts <- iterake(fake, id, mod, wgt.lim = 3)

wgts_only <- iterake(fake, id, mod, wgt.lim = 3, join.weights = FALSE)

comb <- fake %>% left_join(wgts, by = "id")

post_rake(comb, weight, mod)
