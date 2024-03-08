#' Get weighting factors for categories used in weighting scheme
#' 
#' @description
#' This creates a `tibble` of every combination of the groups of the weighting
#' categories passed to `universe()`, their weights, and the number of occurrences.
#' 
#' @param x Output of `iterake()`.
#' 
#' @importFrom dplyr as_tibble mutate count
#' @importFrom rlang abort syms
#' 
#' @return A `tibble`.
#' 
#' @export
weight_factors <- function(x) {
    
    if (!inherits(x, "iterake")) {
        abort("Input to `x` must be the output of `iterake()`.")
    }
    
    cat.vars <- c(names(x$universe$categories), ".weight")
    
    x$universe$data$data |> 
        as_tibble() |> 
        mutate(.weight = x$results) |> 
        count(!!!syms(cat.vars))
    
}

utils::globalVariables(c("weight"))