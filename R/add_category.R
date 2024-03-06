#' Append universe with existing category
#' 
#' This appends an existing `universe()` object with an existing `category()` object.
#' This does the same checks done in `universe()` but just on what is passed to `category()`.
#' 
#' @param universe Output object from `universe()`.
#' @param category Output object from `category()`.
#' 
#' @importFrom dplyr setdiff
#' @importFrom glue glue
#' @importFrom purrr list_c map map_lgl
#' @importFrom rlang abort dots_list
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
#' cat <- category(
#'     name = "am",
#'     groups = c(0, 1),
#'     targets = c(1/3, 2/3)
#' )
#' 
#' add_category(uni, cat)
#' 
#' @export
add_category <- function(universe, category) {
    
    if (!inherits(universe, "universe")) {
        abort("Input to `universe` must be the output of `universe()`.")
    }
    
    if (!inherits(category, "category")) {
        abort("Input to `category` must be the output of `category()`.")
    }
    
    # Check to make sure names of the categories and their groups
    # match those provided in the data, and that its not already
    # in the universe
    cats <-
        dots_list(category) |> 
        map(unclass) |> 
        list_c()
    
    cat.name <- names(cats)
    
    mismatch <- setdiff(cat.name, names(universe$data$data))
    
    if (length(mismatch) > 0) {
        abort(glue("`{cat.name}` does not have a corresponding column in `data`."))
    }
    
    if (cat.name %in% names(universe$categories)) {
        abort(glue("`{cat.name}` already exists in `universe`, no need to add it."))
    }
    
    # Check that there is no mismatch between the unique values in the
    # data and the groups provided in the category.
    cat.now <- cat.name
    vec.now <- universe$data$data[[cat.name]]
    groups.now <- cats[[1]][["groups"]]
    
    # Check for type compatibility
    if (!compatible_types(vec.now, groups.now)) {
        abort(glue(
            "Weighting category `{cat.now}` is a {class(groups.now)} vector in the category specification but is a {class(vec.now)} vector in the data; they need to be the same class."
        ))
    }
    
    vec.unique <- unique(vec.now)
    
    mismatch <- setdiff(groups.now, vec.unique)
    
    if (length(mismatch) > 0) {
        abort(glue("The groups specified for the weighting category `{cat.now}` need to be identical to the unique values of that variable in the data."))
    }
    
    universe$categories <- c(universe$categories, cats)
    
    universe
    
}