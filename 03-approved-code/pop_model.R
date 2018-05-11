pop_model <- function(...) {
    
    wgt_cats <- list(...)
    
    if (!(all(map_lgl(wgt_cats, function(x) "wgt_cat" %in% class(x))))) {
        stop("Each input to pop_model must be of the class `wgt_cat`. Use `wgt_cat() to construct this input.")
    } 
    
    wgt_cats <- bind_rows(wgt_cats)

    class(wgt_cats) <- c(class(wgt_cats), "pop_model")
    
    wgt_cats
}





