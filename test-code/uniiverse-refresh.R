
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

