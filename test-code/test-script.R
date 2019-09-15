library(tidyverse)
library(iterake)
library(toolbox)
library(glue)

dd2 <- mutate(dealer_data, Age2 = inject_value(Age, p = 0.1))

# One thing I haven't accounted for yet is factors...

dd2 <- mutate(dd2, Type2 = factor(Type, c("Car", "SUV", "Truck")))

# Also in universe(), should there be a type check? Make sure that type
# which gets defined by the buckts in category() matches the data?

# Also, what happens if `NA` is used as a bucket, is this acceptable? My thought
# is no, not sure though...?

universe2(
    
    data = demo_data,
    
    category2(
        name = "Sex",
        buckets = factor(
            x = c("Male", "Female"), 
            levels = c("Male", "Female")
        ),
        targets = c(0.4, 0.5),
        sum.1 = TRUE
    ),
    
    category2(
        name = "BirthYear",
        buckets = c(1986:1990),
        targets = rep(0.2, times = 5)
    ),
    
    category2(
        name = "EyeColor",
        buckets = c("brown", "green", "blue"),
        targets = c(0.8, 0.1, 0.1)
    ),
    
    category2(
        name = "HomeOwner",
        buckets = c(T, F),
        targets = c(1/3, 2/3)
    )
)
