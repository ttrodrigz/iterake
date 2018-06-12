#' Create weighting category
#' 
#' To be fed into \code{pop_model()}, this function creates an individual weighting category. (E.g., age group.)
#' 
#' @param name Name given to weighting category, character. Must have exact match in the column names of data you intend to weight.
#' @param buckets Vector corresponding to the "buckets" in which your sample is divided.
#' @param targets Numeric vector of the proportions each element of \code{`buckets`} represents in the population.
#' @param sum.1 Whether or not to force inputs to \code{`targets`} to sum to one.
#' 
#' @return A nested \code{tibble} with special class \code{wgt_cat}.
#' 
#' @examples 
#' wgt_cat(
#'   name = "age",
#'   buckets = c("18-54", "55+"),
#'   targets = c(0.645, 0.355)
#' )
#' 
#' wgt_cat(
#'   name = "vehicle",
#'   buckets = c("car", "suv", "truck"),
#'   targets = c(0.3333, 0.3333, 0.3333),
#'   sum.1 = TRUE
#' )
#' 
#' @export
wgt_cat <- function(name, buckets, targets, sum.1 = FALSE) {
    
    # verify parameters
    if (!is.character(name) || length(name) != 1) {
        stop("`name` must be a character vector of length one.")
    }
    
    if (length(buckets) != length(targets)) {
        stop("Length of `buckets` must match `targets`.")
    }
    
    if (stringr::str_length(name) == 0) {
        stop("String length of `name` must be greater than zero.")
    }
    
    # adjust if specified
    if (isTRUE(sum.1)) {
        targets <- targets / sum(targets)
    }
    
    if (sum(targets) != 1) {
        stop("`targets` must sum to 1. Review target proportions or force them to sum to 1 by setting `sum.1` = TRUE.")
    }
    
    # create nested tibble structure
    out <- 
        tibble::tibble(
            wgt_cat = name,
            data = list(
                tibble::tibble(buckets = buckets,
                               targ_prop = targets)
            )
        )
    
    # assign class
    class(out) <- c(class(out), "wgt_cat")

    return(out)
    
}