library(tidyverse)

ages  <- c("18-34", "35-54", "55+")
years <- c(2015, 2016, 2017, 2018)
cars  <- c("Car", "SUV", "Truck")

N <- 400

set.seed(101)

dealer_data <-
    tibble(
        Sales_ID = sample(1000:9999, size = N, replace = F),
        Age = sample(ages, N, replace = T, prob = c(.17, .55, .28)),
        Year = sample(years, N, replace = T, prob = c(.20, .23, .34, .23)),
        Type = sample(cars, N, replace = T, prob = c(.36, .52, .12))
    )

save(dealer_data, file = "data/dealer_data.RData")
