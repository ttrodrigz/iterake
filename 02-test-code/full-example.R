library(iterake)

source("./03-approved-code/post_rake.r")

fake <- readr::read_rds("./data-for-testing/test_data.rds")
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

pre_rake(fake, mod, plot = T)
wgt <- iterake(fake, id, mod)
post_rake(wgt, weight, mod, plot = T)

raked <- iterake(fake, id, mod, wgt.lim = 4)
post_rake_details <- post_rake(raked, weight, mod)

# checking iterake

## wrong data type
iterake(df = list(), design = mod)

## no id
iterake(df = fake, design = mod)

## wrong id
iterake(df = fake, design = mod, id = shit)

## bad wgt.names
iterake(df = fake, design = mod, id = id,
        wgt.name = c("wgt1", "wgt2"))

iterake(df = fake, design = mod, id = id,
        wgt.name = 1)

## bad numeric stuff
iterake(df = fake, design = mod, id = id,
        wgt.lim = "0.5")

iterake(df = fake, design = mod, id = id,
        wgt.lim = 0.5)

iterake(df = fake, design = mod, id = id,
        threshold = "0.5")

iterake(df = fake, design = mod, id = id,
        max.iter = c(1, 1))

iterake(df = fake, design = mod, id = id,
        max.iter = 0)

# will fail
iterake(df = fake, design = mod, id = id,
        wgt.lim = 1.01)

iterake(df = fake, design = mod, id = id,
        max.iter = 1)

# will succeed
wgts <- iterake(fake, id, mod, wgt.lim = 3)

post_rake(comb, weight, mod)
