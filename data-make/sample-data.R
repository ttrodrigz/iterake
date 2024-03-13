
# Set up ------------------------------------------------------------------

library(tidyverse)


# Make data ---------------------------------------------------------------

# This is just a tibbled-up version of the datasets::HairEyeColor data,
# then randomly sampled w/out replacment and added IDs.

HairEyeColor2 <-
    datasets::HairEyeColor |> 
    as.data.frame() |> 
    as_tibble() |> 
    uncount(weights = Freq) |> 
    slice_sample(prop = 1, replace = FALSE) |> 
    rename_with(tolower) |> 
    rowid_to_column("id")

usethis::use_data(HairEyeColor2, overwrite = TRUE)


# Universe ----------------------------------------------------------------


u <- universe(
    data = HairEyeColor2,
    category(
        name = "hair",
        groups = fct_levels(HairEyeColor2[["hair"]]),
        targets = c(0.20, 0.52, 0.07, 0.21)
    ),
    category(
        name = "eye",
        groups = fct_levels(HairEyeColor2[["eye"]]),
        targets = c(0.40, 0.35, 0.17, 0.08)
    ),
    category(
        name = "sex",
        groups = fct_levels(HairEyeColor2[["sex"]]),
        targets = c(0.5, 0.5)
    )
)

fct_levels <- function(f) {
    fct_inorder(levels(f))
}

i <- iterake(u, permute = FALSE)

