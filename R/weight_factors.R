#' Generate information on weight factors
#' 
#' @description
#' This function creates a `tibble` containing every combination of weighting categories
#' used by `iterake()`, along with the size of each combination and its weight.
#' 
#' @param x Output of `iterake()`.
#' 
#' @importFrom dplyr across count group_by select ungroup
#' @importFrom rlang abort
#' @importFrom tidyselect all_of
#' 
#' @return A `tibble` containing every weighting category combination used by `iterake()` along with 
#' the weight assigned to it and the number in each combination.
#' 
#' @export
weight_factors <- function(x) {
    
    if (!inherits(iterake, "iterake")) {
        abort("Input to `x` must be the output of `iterake()`.")
    }
    
    x$universe$data$data |>
        select(names(x$universe$categories)) |>
        mutate(weight = x$results) |>
        group_by(across(all_of(names(x$universe$categories)))) |>
        count(weight) |>
        ungroup()
    
}

utils::globalVariables(c("weight"))