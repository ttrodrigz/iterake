pop_model <- function(...) {
    
    wgt_cats <- list(...)
    wgt_cats <- flatten(wgt_cats)
    
    wgt_cats %>%
        bind_rows() %>%
        group_by(wgt_cat) %>%
        nest() %>%
        arrange(wgt_cat)
    
}
