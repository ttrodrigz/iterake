wgt_cat <- function(name, value, targ.prop, sum.1 = FALSE) {
    
    if (!is.character(name) || length(name) != 1) {
        stop("`name` must be a character vector of length one.")
    }
    
    if (length(value) != length(targ.prop)) {
        stop("Length of `value` must match `targ.prop`.")
    }
    
    if (str_length(name) == 0) {
        stop("String length of `name` must be greater than zero.")
    }
    
    if (isTRUE(sum.1)) {
        targ.prop <- targ.prop / sum(targ.prop)
    }
    
    if (sum(targ.prop) != 1) {
        stop("`targ.prop` must sum to 1. Review target proportions or force them to sum to 1 by setting `sum.1` = TRUE.")
    }
    
    out <- 
        tibble(
            wgt_cat = name,
            data = list(
                tibble(value = value,
                       targ_prop = targ.prop)
            )
        )
    
    class(out) <- c(class(out), "wgt_cat")
    return(out)
    
}