library(tidyverse)
set.seed(999)

# first dataset
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

sim_size <- function(n) {
    
    tibble::tibble(
        id = 1:n,
        v1 = sample(x = c(1, 2, 3), 
                    size = n,
                    replace = TRUE, 
                    prob = c(0.5, 0.3, 0.2)),
        
        v2 = sample(x = c(1, 2), 
                    size = n,
                    replace = TRUE, 
                    prob = c(0.45, 0.55)),
        
        v3 = sample(x = c(1, 2, 3, 4, 5), 
                    size = n,
                    replace = TRUE, 
                    prob = c(0.2, 0.2, 0.2, 0.2, 0.2)),
        
        v4 = sample(x = c(1, 2, 3, 4), 
                    size = n,
                    replace = TRUE, 
                    prob = c(0.15, 0.45, 0.2, 0.2))
    )
}

df_1 <- sim_size(1000)
df_2 <- sim_size(1e5)
df_3 <- sim_size(1e7)

saveRDS(df_1, "./data-test/df_1.rds")
saveRDS(df_2, "./data-test/df_2.rds")
saveRDS(df_3, "./data-test/df_3.rds")
