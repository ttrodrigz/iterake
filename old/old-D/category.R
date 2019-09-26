#' Create weighting category
#' 
#' This function creates an individual weighting category with known marginal 
#' probabilities. (E.g., age group, eye color.) One or more of these are built and
#' fed into \code{universe()}.
#' 
#' @param name Name given to weighting category, character. 
#' Must have exact match in the column names of data to be weighted.
#' @param buckets Vector corresponding to the "buckets" in which your category can be classified.
#' @param targets Numeric vector of the proportions each element of \code{`buckets`} represents in the population.
#' @param sum.1 Whether or not to force inputs to \code{`targets`} to sum to one.
#' 
#' @importFrom tibble tibble
#' 
#' @return A nested \code{tibble} with special class \code{category}.
#' 
#' @examples 
#' category(
#'   name = "costume",
#'   buckets = c("Bat Man", "Cactus"),
#'   targets = c(0.645, 0.355)
#' )
#' 
#' category(
#'   name = "seeds",
#'   buckets = c("Tornado", "Bird", "Earthquake"),
#'   targets = c(0.3333, 0.3333, 0.3333),
#'   sum.1 = TRUE
#' )
#' 
#' @export
category <- function(name, buckets, targets, sum.1 = FALSE) {
    
    # verify parameters
    if (!is.character(name) || length(name) != 1) {
        stop("`name` must be a character vector of length one.")
    }
    
    if (length(buckets) != length(targets)) {
        stop("Length of `buckets` must match length of `targets`.")
    }
    
    if (nchar(name) == 0) {
        stop("String length of `name` must be greater than zero.")
    }
    
    # adjust if specified
    if (sum.1) {
        targets <- targets / sum(targets)
    }
    
    # the rounding is here in case of odd rounding issues...
    # there were instances where sum(targets) seemed to equal 1, but was coming up as TRUE when 
    # used in sum(targets) != 1 testing - rounding to 15 digits for some reason made it happy again...
    if (round(sum(targets), 15) != 1) {
        stop("`targets` must sum to 1. Review target proportions or force them to sum to 1 by setting `sum.1` = TRUE.")
    }
    
    # create nested tibble structure
    out <- 
        tibble(
            category = name,
            data = list(
                tibble(buckets = buckets,
                       targ_prop = targets)
            )
        )
    
    # assign class
    class(out) <- c(class(out), "category")
    
    return(out)
    
}