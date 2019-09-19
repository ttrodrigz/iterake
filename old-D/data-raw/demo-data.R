## code to prepare `demo-data` dataset goes here

bigN <- 200

set.seed(101)

demo_data <- tibble::tibble(
    ID = 1:bigN,
    Sex = factor(
        x = sample(
            x = c("Male", "Female", NA), 
            size = bigN, 
            replace = T, 
            prob = c(0.40, 0.55, 0.05)
        ),
        levels = c("Male", "Female")
    ),
    BirthYear = sample(1986:1990, bigN, T),
    EyeColor = sample(
        c("brown", "green", "blue"),
        size = bigN,
        replace = TRUE,
        prob = c(0.75, 0.10, 0.05)
    ),
    HomeOwner = sample(
        x = c(TRUE, FALSE, NA),
        size = bigN,
        replace = TRUE,
        prob = c(0.85, 0.10, 0.05)
    )
)

usethis::use_data(demo_data, overwrite = TRUE)
