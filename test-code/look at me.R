
u <-
    mtcars |> 
    universe() |> 
    new_category(
        name = "cyl",
        groups = c(4, 6, 8),
        targets = c(1, 1, 1)
    ) |> 
    new_category(
        name = "gear",
        groups = c(3, 4, 5),
        targets = c(1, 1, 1)
    )

got_stuck <- iterake(
    universe = u,
    control = control_iterake(threshold = 0, max_iter = 100000)
)

met_iter <- iterake(
    universe = u,
    control = control_iterake(threshold = 0, max_iter = 10)
)


good <- iterake(
    universe = u,
    control = control_iterake()
)
