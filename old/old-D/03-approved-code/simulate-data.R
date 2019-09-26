library(tidyverse)
set.seed(999)

# first dataset
n <- 200

weight_me <- tibble(
    order = 1:n,
    SEEDS = runif(n),
    COSTUME = runif(n),
    TRANSPORT = runif(n),
    SAT = runif(n),
    seeds = ifelse(SEEDS < 1/3, "Tornado",
                 ifelse(SEEDS > 1/3 & SEEDS < 2/3, "Bird", "Earthquake")),
    costume = ifelse(COSTUME < 0.55, "Bat Man", "Cactus"),
    transport = ifelse(TRANSPORT < 0.35, "Rocket Cart",
                    ifelse(TRANSPORT > 0.35 & TRANSPORT < 0.85, "Jet Propelled Skis", "Jet Propelled Unicycle")),
    satisfied = ifelse(SAT < 0.5, "Yes", "No"),
    prev_weight = 1 + 0.15*(rnorm(n))
    ) %>%
    select(order, seeds, costume, transport, satisfied, prev_weight)

save(weight_me, file = "./data/weight_me.RData")

write_csv(weight_me, path = "./data-for-testing/test_data.csv")
haven::write_sas(weight_me, path = "./data-for-testing/test_data.sas7bdat")
saveRDS(weight_me, "./data-for-testing/test_data.RDS")
saveRDS(weight_me, "./data-for-testing/test_data.Rdata")

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

saveRDS(df_1, "./data-for-testing/df_1.rds")
saveRDS(df_2, "./data-for-testing/df_2.rds")
saveRDS(df_3, "./data-for-testing/df_3.rds")
