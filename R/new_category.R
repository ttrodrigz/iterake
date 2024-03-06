#' Append universe with new category
#' 
#' @description
#' This appends an existing `universe` object with a new weighting category.
#' 
#' @param universe Output object from `universe()`.
#' @param name The name of the weighting category matching the name of the
#' corresponding variable in the data to be weighted.
#' @param groups A vector of the groups in the weighting category.
#' @param targets A vector of the desired proportions of the `groups`.
#' These targets will be automatically re-proportioned to sum to 1 if
#' necessary.
#' 
#' @return A `list` with special class `universe`.
#' 
#' @examples 
#' uni <- universe(
#'     data = mtcars,
#' 
#'     category(
#'         name = "vs",
#'         groups = c(0, 1),
#'         targets = c(1/2, 1/2)
#'     )
#' )
#' 
#' new_category(
#'     universe = uni, 
#'     name = "am",
#'     groups = c(0, 1),
#'     targets = c(1/3, 2/3)
#' )
#' 
#' @export
new_category <- function(universe, name, groups, targets) {
    
    # each separate function does error checks -
    # should there be some in here as well?
    cat <- category(
        name = name,
        groups = groups,
        targets = targets
    )
    
    out <- add_category(universe, cat)
    
    out
    
}
