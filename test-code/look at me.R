
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

good <- iterake(
    universe = u,
    control = control_iterake()
)

good |> 
    append_weights() |> 
    count(cyl, gear, weight)


aa <- c("cyl", "gear")
aa <- syms(aa)

good |> 
    append_weights() |> 
    count(!!!aa, weight) |> 
    summarise(sum(weight * n))
