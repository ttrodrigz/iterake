#' Extract weights
#' 
#' Extracts the weights from the output of `iterake()`. It either returns
#' data passed to `universe()` with the weights appended, or just a one-column
#' `tibble` of the weights.
#' 
#' @param x Output of `iterake()`.
#' @param name Desired column name of the weights, default is `"weight"`.
#' @param append Whether or not to either append the weights to the original 
#' data (`TRUE`, default), or to only return a `tibble` of the weights.
#' 
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom rlang := abort
#' @importFrom tibble tibble as_tibble
#' 
#' @return A `tibble` of the weights, optionally with the original data.
#' 
#' @export
extract_weights <- function(x, name = "weight", append = TRUE) {
    
    if (!inherits(x, iterake)) {
        abort("Input to `x` must be the output of `iterake()`.")
    }
    
    if (any(
        length(name) > 1,
        !is.character(name)
    )) {
        abort("Input to `name` must be a single character vector.")
    }
    
    
    if (append) {
        
        if (name %in% names(x$universe$data$data)) {
            abort(glue("Column '{name}' already exists in the data, please supply a different name."))
        }
        
        out <-
            x$universe$data$data |>
            as_tibble() |>
            mutate({{name}} := x$results)
        
    } else {
        out <- tibble({{name}} := x$results)
    }
    
    out
    
}
