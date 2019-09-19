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
            dplyr::mutate(prevWeight = 1) %>%
            dplyr::select(prevWeight, name)
        
    } else {
        
        prev.wgt <- dplyr::enquo(prev.wgt)
        df <- 
            df %>%
            dplyr::mutate(prevWeight = !! prev.wgt) %>%
            dplyr::select(prevWeight, name)
    }
    
    targs <- 
        df %>%
        tidyr::gather(name, value, -1) %>%
        dplyr::group_by(value) %>%
        dplyr::summarise(wgt_n = sum(prevWeight)) %>%
        dplyr::mutate(wgt_prop = wgt_n / sum(wgt_n)) %>%
        dplyr::ungroup()
    
    out <- 
        tibble::tibble(
            wgt_cat = name,
            data = list(
                tibble::tibble(buckets = targs$value,
                               targ_prop = targs$wgt_prop)
            )
        )
    
    class(out) <- c(class(out), "wgt_cat")
    
    return(out)
    
}