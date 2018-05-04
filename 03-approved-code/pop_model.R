pop_model <- function(...) {
    
    wgt_cats <- list(...)

    if (!(all(map_lgl(wgt_cats, function(x) "wgt_cat" %in% class(x))))) {
        stop("All members must be of the proper class. Use wgt_cat function.")
    } 
    
    wgt_cats <- flatten(wgt_cats)
    
    wgt_cats <- wgt_cats %>%
        bind_rows() %>%
        group_by(wgt_cat) %>%
        nest() %>%
        arrange(wgt_cat)
    
    class(wgt_cats) <- c(class(wgt_cats), "pop_model")
    
    wgt_cats
}
