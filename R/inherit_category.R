#' Create weighting category from data
#' 
#' This function creates an individual weighting category with known marginal 
#' probabilities (E.g., age group, eye color.), but unlike \code{category()} it 
#' creates them from existing data instead of being directly assigned. One or more 
#' of these are built and fed into \code{universe()}.
#' 
#' @param name Name given to weighting category, character. 
#' Must have exact match in the column names of data you intend to weight as well as the
#' existing data from where targets are being pulled.
#' @param from Existing data frame from where targets will be pulled.
#' @param weight Optionally provide an existing weight variable to calculate 
#' weighted target proportions. Omit if unweighted target proportions are desired.
#' 
#' @importFrom dplyr mutate select enquo group_by summarise ungroup %>%
#' @importFrom rlang !!
#' @importFrom tidyr gather
#' @importFrom tibble tibble
#' 
#' @return A nested \code{tibble} with special class \code{category}.
#' 
#' @examples 
#' data(demo_data)
#' 
#' inherit_category(
#'   name = "EyeColor",
#'   from = demo_data
#' )
#' 
#' inherit_category(
#'   name = "BirthYear",
#'   from = demo_data,
#'   weight = prev_weight
#' )
#' 
#' @export
inherit_category <- function(name, from, weight) {
    
    # verify parameters
    if (!is.character(name) || length(name) != 1) {
        stop("`name` must be a character vector of length one.")
    }
    
    if (nchar(name) == 0) {
        stop("String length of `name` must be greater than zero.")
    }
    
    if (!is.data.frame(from)) {
        stop("'from' must be an object of class 'data.frame'")
    }
    
    # assign prevWeight value of 1 or weight if specified 
    if (missing(weight) || !deparse(substitute(weight)) %in% names(from)) {
        
        from <- 
            from %>%
            mutate(prevWeight = 1) %>%
            select(prevWeight, name)
        
    } else {
        
        weight <- enquo(weight)
        from <- 
            from %>%
            mutate(prevWeight = !! weight) %>%
            select(prevWeight, name)
    }
    
    # create weighted proportions from data
    targs <- 
        from %>%
        gather(name, value, -1) %>%
        group_by(value) %>%
        summarise(wgt_n = sum(prevWeight)) %>%
        mutate(wgt_prop = wgt_n / sum(wgt_n)) %>%
        ungroup()
    
    # create nested tibble structure
    out <- 
        tibble(
            category = name,
            data = list(
                tibble(buckets = targs$value,
                       targ_prop = targs$wgt_prop)
            )
        )
    
    # assign class    
    class(out) <- c(class(out), "category")
    
    return(out)
    
}

utils::globalVariables(c("prevWeight", "value", "wgt_n"))