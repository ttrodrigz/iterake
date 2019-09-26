library(tidyverse)
library(iterake)
library(toolbox)
library(glue)
library(data.table)
library(scales)
library(crayon)

dd2 <- mutate(dealer_data, Age2 = inject_value(Age, p = 0.1))

# One thing I haven't accounted for yet is factors...

dd2 <- mutate(dd2, Type2 = factor(Type, c("Car", "SUV", "Truck")))


univ <- universe(
    
    data = demo_data,
    # category2(
    #     name = "Sex",
    #     buckets = c("Male", "Female"),
    #     targets = c(0.4, 0.5),
    #     sum.1 = TRUE
    # ),
    
    category(
        name = "Sex",
        buckets = factor(
            x = levels(demo_data[["Sex"]]), 
            levels = levels(demo_data[["Sex"]])
        ),
        targets = c(0.4, 0.5),
        sum.1 = TRUE
    ),
    
    category(
        name = "BirthYear",
        buckets = c(1986:1990),
        targets = rep(0.2, times = 5)
    ),
    
    category(
        name = "EyeColor",
        buckets = c("brown", "green", "blue"),
        targets = c(0.8, 0.1, 0.1)
    ),
    
    category(
        name = "HomeOwner",
        buckets = c(T, F),
        targets = c(3/4, 1/4)
    )
)

compare_margins(
    df = demo_data,
    universe = univ,
    plot = TRUE
)

wgts <- demo_data
wgts$weight <- 1

stuff <- compare_margins(
    df = wgts,
    weight = weight,
    universe = univ,
    plot = TRUE
)

a <- iterak2(univ, wgt.name = "turd", max.wgt = 10, max.iter = 10, threshold = 1e-10)
a

plot(a$turd)

