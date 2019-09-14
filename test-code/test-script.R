library(tidyverse)
library(iterake)
library(toolbox)
library(glue)

dd2 <- mutate(dealer_data, Age2 = inject_value(Age, p = 0.1))

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
