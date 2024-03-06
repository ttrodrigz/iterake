#' Generate information on the weight factors used by iterake.
#' 
#' This function creates a tibble containing every combination of weighting categories
#' used by iterake, along with the size of each combination and its weight.
#' 
#' @param iterake Output object created with \code{iterake()} function.
#' 
#' @importFrom dplyr across count group_by select ungroup
#' @importFrom rlang abort
#' @importFrom tidyselect all_of
#' 
#' @return A tibble containing every weighting category combination used by iterake along with 
#' the weight assigned to it and the number in each combination.
#' 
#' @export
weight_factors <- function(iterake) {
    
    its.a.rake <- inherits(iterake, "iterake")
    
    if (!its.a.rake) {
        abort("Input to `iterake` must be the output of `iterake()`.")
    }
    
    iterake$universe$data$data |>
        select(names(iterake$universe$categories)) |>
        mutate(weight = iterake$results) |>
        group_by(across(all_of(names(iterake$universe$categories)))) |>
        count(weight) |>
        ungroup()
    
}

utils::globalVariables(c("weight"))