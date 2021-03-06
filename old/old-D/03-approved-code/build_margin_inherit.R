#' Create weighting category from data
#' 
#' To be fed into \code{universe()}, this function creates an individual weighting category. (E.g., age group.)
#' Unlike \code{build_margin()}, this function creates bucket targets from a dataframe instead of being directly assigned.
#' 
#' @param name Name given to weighting category, character. Must have exact match in the column names of data you intend to weight.
#' @param df Data frame containing data you intend to weight.
#' @param prev.wgt Variable name of weight used to calculate bucket proportions in \code{`df`}.
#' 
#' @importFrom dplyr mutate select enquo group_by summarise ungroup %>%
#' @importFrom rlang !!
#' @importFrom tidyr gather
#' @importFrom tibble tibble
#' 
#' @return A nested \code{tibble} with special class \code{build_margin}.
#' 
#' @examples 
#' data(weight_me)
#' 
#' build_margin_inherit(
#'   name = "costume",
#'   df = dplyr::filter(weight_me, satisfied == "No")
#' )
#' 
#' build_margin_inherit(
#'   name = "transport",
#'   df = dplyr::filter(weight_me, satisfied == "No"),
#'   prev.wgt = prev_weight
#' )
#' 
#' @export
build_margin_inherit <- function(name, df, prev.wgt) {
    
    # verify parameters
    if (!is.character(name) || length(name) != 1) {
        stop("`name` must be a character vector of length one.")
    }
    
    if (nchar(name) == 0) {
        stop("String length of `name` must be greater than zero.")
    }
    
    if (!is.data.frame(df)) {
        stop("'df' must be an object of class 'data.frame'")
    }

    # assign prevWeight value of 1 or prev.wgt if specified 
    if (missing(prev.wgt) || !deparse(substitute(prev.wgt)) %in% names(df)) {
        
        df <- 
            df %>%
            mutate(prevWeight = 1) %>%
            select(prevWeight, name)
        
    } else {
        
        prev.wgt <- enquo(prev.wgt)
        df <- 
            df %>%
            mutate(prevWeight = !! prev.wgt) %>%
            select(prevWeight, name)
    }
    
    # create weighted proportions from data
    targs <- 
        df %>%
        gather(name, value, -1) %>%
        group_by(value) %>%
        summarise(wgt_n = sum(prevWeight)) %>%
        mutate(wgt_prop = wgt_n / sum(wgt_n)) %>%
        ungroup()
    
    # create nested tibble structure
    out <- 
        tibble(
            wgt_cat = name,
            data = list(
                tibble(buckets = targs$value,
                       targ_prop = targs$wgt_prop)
            )
        )

    # assign class    
    class(out) <- c(class(out), "build_margin")
    
    return(out)
    
}

utils::globalVariables(c("prevWeight", "value", "wgt_n"))