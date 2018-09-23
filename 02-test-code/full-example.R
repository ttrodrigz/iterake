library(iterake)

fake <- readr::read_rds("./data-for-testing/test_data.rds")
mod <- universe(df = fake,
    
    # age category
    build_margin(name = "age",
            buckets = c("18-34", "35-54", "55+"),
            targets = c(0.300, 0.360, 0.340)),
    
    # gender category
    build_margin(name = "gender",
            buckets = c("Female", "Male"),
            targets = c(0.500, 0.500)),
    
    # vehicle category
    build_margin(name = "vehicle",
            buckets = c("Car", "SUV", "Truck"),
            targets = c(0.400, 0.450, 0.150))
    
)

compare_margins(fake, mod, plot = T)
wgt <- iterake(fake, id, mod)
compare_margins(wgt, mod, weight, plot = T)

# numeric vector
weight_stats(wgt$weight)

# non-numeric vector
weight_stats(wgt$age)

# multi-col df
weight_stats(wgt)

# numeric one-col df
weight_stats(dplyr::select(wgt, weight))

# non-numeric one col df
weight_stats(dplyr::select(wgt, age))


raked <- iterake(fake, id, mod, wgt.lim = 4)
post_rake_details <- compare_margins(raked, mod, weight)

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

compare_margins(comb, mod, weight)
