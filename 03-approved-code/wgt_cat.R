wgt_cat <- function(name, buckets, targets, sum.1 = FALSE) {
    
    if (!is.character(name) || length(name) != 1) {
        stop("`name` must be a character vector of length one.")
    }
    
    if (length(buckets) != length(targets)) {
        stop("Length of `buckets` must match `targets`.")
    }
    
    if (str_length(name) == 0) {
        stop("String length of `name` must be greater than zero.")
    }
    
    if (isTRUE(sum.1)) {
        targets <- targets / sum(targets)
    }
    
    if (sum(targets) != 1) {
        stop("`targets` must sum to 1. Review target proportions or force them to sum to 1 by setting `sum.1` = TRUE.")
    }
    
    out <- 
        tibble(
            wgt_cat = name,
            data = list(
                tibble(buckets = buckets,
                       targ_prop = targets)
            )
        )
    
    class(out) <- c(class(out), "wgt_cat")
    return(out)
    
}