library(cli)
library(dplyr)
library(glue)
library(labelled)
library(purrr)
library(rlang)
library(scales)
library(stringr)

category <- function(name, groups, targets) {
    
    # Check to make sure groups and targets are the same length
    if (length(groups) != length(targets)) {
        abort("The number of groups must equal the number of targets.")
    }
    
    # Store as a list
    out <- list(list(
        groups = groups,
        targets = targets
    ))
    
    names(out) <- name
    
    # Check to make sure that the targets sum to 1
    sum.targets <- sum(targets, na.rm = TRUE)
    sum1 <- sum.targets == 1
    
    if (!sum1) {
        warn(glue(
            "Weighting targets for '{name}' will be re-proportioned to sum to 1."
        ))
        targets.sum1 <- targets / sum.targets
        out[[1]]$targets_sum1 <- targets.sum1
    }
    
    # Are the groups labelled?
    is.labelled <- is.labelled(groups)
    
    if (is.labelled) {
        out[[1]]$labels <- names(val_labels(groups))
    }
    
    class(out) <- c(class(out), "category")
    
    out
    
}

universe <- function(data, ..., pop_size = NULL) {
    
    compatible_types <- function(x, y) {
        
        if (is.numeric(x) & !is.numeric(y)) {
            return(FALSE)
        }
        
        if (is.character(x) & !is.character(y)) {
            return(FALSE)
        }
        
        if (is.factor(x) & !is.factor(y)) {
            return(FALSE)
        }
        
        if (is.logical(x) & !is.logical(y)) {
            return(FALSE)
        }
        
        TRUE
        
    }
    
    
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
        categories = cats,
        pop_size = pop_size
    )
    
    class(out) <- c(class(out), "universe")
    
    out
    
}

uni <- universe(
    data = mtcars,
    
    category(
        name = "vs",
        groups = c(0, 1),
        targets = c(1/2, 1/2)
    )
)

cat <- category(
    name = "am",
    groups = c(0, 1),
    targets = c(1/3, 2/3)
)

# for add_category, which makes more sense:
# - take new category and pass it and existing categories back
#   through something that looks a lot like universe
# - do checks only based on the new category being added, but
#   will need to make sure the new category doesn't already exist?

#' Add a category object to a universe.
add_category <- function(universe, category) {

    compatible_types <- function(x, y) {
        
        if (is.numeric(x) & !is.numeric(y)) {
            return(FALSE)
        }
        
        if (is.character(x) & !is.character(y)) {
            return(FALSE)
        }
        
        if (is.factor(x) & !is.factor(y)) {
            return(FALSE)
        }
        
        if (is.logical(x) & !is.logical(y)) {
            return(FALSE)
        }
        
        TRUE
        
    }
    
    its.a.uni <- inherits(universe, "universe")
    
    if (!its.a.uni) {
        abort("Input to `universe` must be the output of `universe()`.")
    }
    
    its.a.cat <- inherits(category, "category")
    
    if (!its.a.cat) {
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
        
    universe$categories <- c(universe$categories, cats)
    
    universe
    
}

test <- uni %>% add_category(cat)

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

test2 <- new_category(
    uni,
    name = "am",
    groups = c(0, 1),
    targets = c(1/3, 2/3)
)

test3 <-
    uni %>% new_category(name = "am",
                         groups = c(0, 1),
                         targets = c(1/3, 2/3))
