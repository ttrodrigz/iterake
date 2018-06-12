#' Create full weighting design
#' 
#' This combines any number of objects with special class \code{wgt_cat}. It also checks and adjusts
#' for \code{NA} data in the data frame to be weighted.
#' 
#' @param df Data frame containing data you intend to weight.
#' @param ... Object or objects of special class \code{wgt_cat}.
#' 
#' @return A nested \code{tibble} with special class \code{pop_model}.
#' 
#' @examples 
#' data("weight_me")
#' 
#' pop_model(
#'     df = weight_me,
#' 
#'     wgt_cat(
#'         name = "gender",
#'         buckets = c(1, 2),
#'         targets = c(0.5, 0.5)),
#' 
#'     wgt_cat(
#'         name = "vehicle",
#'         buckets = c(1, 2, 3),
#'         targets = c(0.333, 0.333, 0.333))
#' )
#' 
#' @export
pop_model <- function(df, ...) {
    
    # make sure dataframe is supplied
    if (!is.data.frame(df)) {
        stop("Input to `df` must be a data frame.")
    }
    
    # list object of all unspecified arguments passed to function
    wgt_cats <- list(...)
    
    # are all inputs to this function wgt_cats?
    if (!(all(purrr::map_lgl(wgt_cats, function(x) "wgt_cat" %in% class(x))))) {
        stop("Each input to pop_model must be of the class `wgt_cat`. Use `wgt_cat()` to construct this input.")
    } 
    
    # smush 'em together into final form
    out <- dplyr::bind_rows(wgt_cats)
    
    # make sure each wgt_cat in pop.model has a matching column in df
    df.names  <- names(df)
    mod.names <- dplyr::pull(out, wgt_cat)
    bad.names <- mod.names[!mod.names %in% df.names]
    
    if (length(bad.names) > 0) {
        stop(
            paste(
                "Each weighting category in `pop.model` must have a matching column name in `df`. The following weighting cagegories have no match:",
                paste(bad.names, collapse = ", "),
                sep = "\n"
            )
        )
    }
    
    # create function for matching wgt_cat attributes to data source
    inherit_chr_fct <- function(to, from) {
        
        # to = character, from = factor
        if ((is.character(to) | is.factor(to)) & is.factor(from)) {
            
            to <- factor(x = to, 
                         levels = levels(from))
            
            if (is.ordered(from)) {
                
                to <- factor(to, ordered = TRUE)
                
            }
        }
        
        # to = factor, from = character
        if (is.factor(to) & is.character(from)) {
            
            to <- as.character(to)
        }
        
        to
        
    }
    
    # adjust targets for any NA in wgt_cats and adjust attributes as needed
    adjusted_model <- purrr::pmap(out, function(..., main_data = df) {
        
        ## create list object out of all unspecified arguments passed from pmap
        ## - this is basically the row being evaluated
        inputList <- list(...)
        
        # get actual proportions to determine existance of NA
        act_props <- 
            main_data %>% 
            dplyr::group_by_(inputList$wgt_cat) %>%
            dplyr::summarise(n = n()) %>%
            dplyr::mutate(act_prop = n / sum(n)) %>%
            dplyr::select(-n)
        
        # figure out what row of above has NA if any
        na_val_a <- which(is.na(act_props[1]))
        # determine if targets already have NA bin
        na_val_t <- which(is.na(inputList$data[1]))
        
        # if NA exists in actuals but not in targets - do stuff
        if (length(na_val_a) == 1 & length(na_val_t) == 0) {
            
            # an attempt at notifying when a change happens due to NAs...
            cat('NAs found in ' %+% paste0(inputList$wgt_cat) %+% '; adjusting targets...\n')
            
            # determine proportion of NAs
            na_prop <- act_props[na_val_a, ]$act_prop
            
            # adjust current targets by a factor of 1 - na_prop
            new_targets <- 
                inputList$data %>%
                dplyr::mutate(targ_prop = targ_prop * (1 - na_prop))

            # insert a new row with the NA info
            new_targets[na_val_a, ] <- c(NA, na_prop)
            # replace existing target model
            inputList$data <- new_targets
        }
        
        # reassign attribute types to match supplied data
        inputList$data$buckets <- inherit_chr_fct(inputList$data$buckets, act_props[[inputList$wgt_cat]])
        
        # recreate tibble similar to how it's put together in wgt_Cat
        tibble::tibble(
            wgt_cat = inputList$wgt_cat,
            data = list(
                tibble::tibble(
                    buckets = inputList$data$buckets,
                    targ_prop = inputList$data$targ_prop)))
        
    }) %>%
        
        # bind it all together
        dplyr::bind_rows()
    
    # assign class
    class(adjusted_model) <- c(class(adjusted_model), "pop_model")
    
    return(adjusted_model)

}
