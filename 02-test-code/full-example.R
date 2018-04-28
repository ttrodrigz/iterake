library(tidyverse)
library(crayon)

source("./02-test-code/pop_model.r")
source("./02-test-code/wgt_cat.r")
source("./02-test-code/iterake.r")

fake <- read_rds("./data/test_data.rds")
mod <- pop_model(
    
    # age category
    wgt_cat(name = "age",
            value = c("18-34", "35-54", "55+"),
            targ.prop = c(0.320, 0.350, 0.330)),
    
    # gender category
    wgt_cat(name = "gender",
            value = c("Female", "Male"),
            targ.prop = c(0.54, 0.46)),
    
    # vehicle category
    wgt_cat(name = "vehicle",
            value = c("Car", "SUV", "Truck"),
            targ.prop = c(0.380, 0.470, 0.150))
    
)

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

comb <- fake %>% left_join(wgts, by = "id")

post_rake(comb, weight, mod)
