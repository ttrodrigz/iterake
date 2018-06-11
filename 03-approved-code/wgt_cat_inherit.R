wgt_cat_inherit <- function(name, df, prev.wgt) {
    
    if (!is.character(name) || length(name) != 1) {
        stop("`name` must be a character vector of length one.")
    }
    
    if (str_length(name) == 0) {
        stop("String length of `name` must be greater than zero.")
    }

    if (!is.data.frame(df)) {
        stop("data must be an object of class 'data.frame'")
    }
    
    if (missing(prev.wgt)) {
        
        df <- 
            df %>%
            mutate(prevWeight = 1) %>%
            select(prevWeight, name)
        
    } else {
        
        prev.wgt <- dplyr::enquo(prev.wgt)
        df <- 
            df %>%
            mutate(prevWeight = !! prev.wgt) %>%
            select(prevWeight, name)
    }
    
    targs <- df %>%
        tidyr::gather(name, value, -1) %>%
        dplyr::group_by(name, value) %>%
        dplyr::summarise(targ_prop = sum(prevWeight) / (nrow(.) / nlevels(as.factor(value))))
    
    out <- 
        tibble(
            wgt_cat = name,
            data = list(
                tibble(value = targs$value,
                       targ_prop = targs$targ_prop)
            )
        )
    
    class(out) <- c(class(out), "wgt_cat")
    
    return(out)
    
}
