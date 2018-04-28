library(tidyverse)

set.seed(999)
n <- 200

df <- tibble(
    id = 1:n,
    AGE = runif(n),
    GENDER = runif(n),
    VEHICLE = runif(n),
    age = ifelse(AGE < 1/3, "18-34",
                 ifelse(AGE > 1/3 & AGE < 2/3, "35-54", "55+")),
    gender = ifelse(GENDER < 0.55, "Female", "Male"),
    vehicle = ifelse(VEHICLE < 0.35, "Car",
                    ifelse(VEHICLE > 0.35 & VEHICLE < 0.85, "SUV", "Truck"))
    ) %>%
    select(id, age, gender, vehicle)

write_csv(df, path = "./data/test_data.csv")
haven::write_sas(df, path = "./data/test_data.sas7bdat")
saveRDS(df, "./data/test_data.RDS")
saveRDS(df, "./data/test_data.Rdata")
