pop_model <- function(...) {
    
    wgt_cats <- list(...)
    
    # are all inputs to this function wgt_cats?
    if (!(all(map_lgl(wgt_cats, function(x) "wgt_cat" %in% class(x))))) {
        stop("Each input to pop_model must be of the class `wgt_cat`. Use `wgt_cat()` to construct this input.")
    } 
    
    # smush 'em together into final form
    out <- bind_rows(wgt_cats)

    class(out) <- c(class(out), "pop_model")
    
    out
    
}





