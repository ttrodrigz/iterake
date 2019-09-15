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
    
    data = dd2,
    
    category2(
        name = "Age2",
        buckets = c("18-34", "35-54", "55+"),
        targets = c(0.2, 0.6, 0.2)
    ),
    
    category2(
        name = "Year",
        buckets = c(2015:2018),
        targets = c(0.15, 0.25, 0.4, 0.3)
    )
)
