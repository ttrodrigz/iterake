#' Create universe
#' 
#' Along with the data to be weighted, this combines one or more objects of 
#' class \code{category} to build the universe with known marginal proportions. 
#' It also checks and adjusts the proportions to account for missing values in the data.
#' 
#' @param data Data frame containing data where weights are desired.
#' @param ... One or more output objects from \code{category()}.
#' 
#' @importFrom dplyr setdiff
#' @importFrom glue glue
#' @importFrom purrr list_c map map_lgl
#' @importFrom rlang abort dots_list
#' 
#' @return A \code{list} with special class \code{universe}.
#' 
#' @examples 
#' universe(
#'     data = mtcars,
#'     category(
#'         name = "vs",
#'         groups = c(0, 1),
#'         targets = c(1/2, 1/2)
#'     )
#' )
#' 
#' @export
universe <- function(data, ...) {
    
    # Get stuff related to the data
    data.name <- deparse(substitute(data))
    ss <- nrow(data)
    
    # Get the categories passed to the dots
    cats <- dots_list(...)
    
    # check to make sure they provided categories
    gave.cats <- length(cats) > 0
    
    # check to make sure all the dots are categories
    if (gave.cats) {
        
        dots.are.cats <- all(map_lgl(cats, inherits, "category"))
        
        if (!dots.are.cats) {
            abort("All inputs to `...` must be the output of `category()`.")
        }
        
        # Check to make sure names of the categories and their groups
        # match those provided in the data.
        cats <-
            cats |> 
            map(unclass) |> 
            list_c()
        
        cat.names <- names(cats)
        
        mismatch <- setdiff(cat.names, names(data))
        
        if (length(mismatch) > 0) {
            abort(glue(
                "The names of the following weight categories provided do not have a corresponding column in `data`:",
                "\n",
                paste(mismatch, collapse = ", ")
            ))
        }
        
        # Check that there is no mismatch between the unique values in the
        # data and the groups provided in the category.
        for (i in seq_along(cats)) {
            
            cat.now <- cat.names[i]
            vec.now <- data[[cat.names[i]]]
            groups.now <- cats[[i]][["groups"]]
            
            # Check for type compatibility
            is.compatible <- compatible_types(vec.now, groups.now)
            
            if (!is.compatible) {
                abort(glue(
                    "Weighting category `{cat.now}` is a {class(groups.now)} vector in the category specification but is a {class(vec.now)} vector in the data; they need to be the same class."
                ))
            }
            
            vec.unique <- unique(vec.now)
            
            mismatch <- setdiff(groups.now, vec.unique)
            
            if (length(mismatch) > 0) {
                abort(glue("The groups specified for the weighting category `{cat.now}` need to be identical to the unique values of that variable in the data."))
            }
            
        }
        
    }

    
    out <- list(
        data = list(
            data = data,
            data_name = data.name,
            ss = ss
        ),
        categories = cats
    )
    
    class(out) <- c(class(out), "universe")
    
    out
    
}

#' Print method for universe objects.
#' 
#' @method print universe
#' 
#' @param x A `universe` object.
#' @param digits Number of digits for printing the target proportions, default is 3.
#' @param ... Not currently used.
#' 
#' @importFrom cli cat_bullet cat_line rule
#' @importFrom glue glue
#' @importFrom scales comma
#'
#' @export
print.universe <- function(x, digits = 3, ...) {
    
    cats <- x$categories
    ncat <- length(cats)
    cat.names <- names(cats)
    data.name <- x$data$data_name
    ss <- comma(x$data$ss)
    
    # Header line
    cat_line(rule(
        left = "Weighting Universe",
        right = glue("N={ss}"),
        line = 2
    ))
    
    if (ncat == 1) {
        cat_line(glue("A weighting universe with {ncat} weighting category."))
    } else {
        cat_line(glue("A weighting universe with {ncat} weighting categories."))
    }
    
    if (ncat > 0) {
        cat_bullet(cat.names, bullet = "bullet", bullet_col = "green")
    }
}
