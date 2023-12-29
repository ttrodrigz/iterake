library(rlang)
library(tidyverse)
universe <- function(data, ..., pop_size = NULL) {
    
    # Collect info about the data
    data.name <- deparse(substitute(data))
    data.nrow <- nrow(data)
    
    
}

rule(
    left = "iterake", line = 2
)


my_function <- function(x) {
    object_name <- deparse(substitute(x))
    cat("The name of the object is:", object_name, "\n")
}

# Example usage
my_vector <- c(1, 2, 3)
my_function(my_vector)

data |> 
    universe() |> 
    add_category() |> 
    add_category()

universe <- function(data, ..., pop_size = NULL) {
    
    # Get stuff related to the data
    data.name <- deparse(substitute(data))
    ss <- nrow(data)
    
    cats <- dots_list(...)
    
    # check to make sure they provided categories
    gave.cats <- length(cats) > 0
    
    # check to make sure all the dots are categories
    if (gave.cats) {
        
        dots.are.cats <- all(map_lgl(cats, inherits, "category"))
        
        if (!dots.are.cats) {
            abort("All inputs to `...` must be the output of `category()`.")
        }
        
        # Check to make sure names of the categories and their buckets
        # match those provided in the data.
        
    }
    
    out <- list(
        data = data,
        data_name = data.name,
        ss = ss,
        categories = cats
    )
    
    class(out) <- c(class(out), "universe")
    
    out
    
}


mtcars |> 
    universe(
        category(
            name = "cyl", 
            buckets = c(4, 6, 8), 
            targets = c(1/3, 1/3, 1/3)
        )
    ) |> 
    add_category(
        name = "am",
        buckets = c(0, 1),
        targets = c(1/2, 1/2)
    )

add_category <- function(x, name, buckets, targets) {
    
    new.cat <- category(name = name, buckets = buckets, targets = targets)
    

    x[["categories"]] <- append(x[["categories"]], list(new.cat))
    
    x
    
}
