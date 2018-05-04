wgt_cat <- function(name, value, targ.prop, sum.1 = FALSE) {
    
    if (!is.character(name) || length(name) != 1) {
        stop("name must be a character vector with one element.")
    }
    
    if (length(value) != length(targ.prop)) {
        stop("Must provide equal number of values and target proportions.")
    }
    
    if (name == "") {
        stop("name must be an non-empty character string.")
    }
    
    if (isTRUE(sum.1)) {
        targ.prop <- targ.prop / sum(targ.prop)
    }
    
    if (sum(targ.prop) != 1) {
        stop("Target proportions must sum to 1, if an issue of rounding set sum.1 = TRUE.")
    }
    
    out <- list(tibble(wgt_cat = name,
                       value = value,
                       targ_prop = targ.prop))
    
    names(out) <- name
    class(out) <- c(class(out), "wgt_cat")
    return(out)
    
}

