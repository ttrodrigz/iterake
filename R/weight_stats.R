#' Generate summary statistics for a set of weights
#' 
#' This function creates a tibble containing information pertaining to a set of weights. It includes
#' sample size, weighted sample size, effective sample size, as well as loss and efficiency values.
#' 
#' @param weight Single column of a data frame or vector of weights.
#' 
#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' 
#' @return A tibble containing unweighted and weighted sample size, effective sample size, and
#' efficiency and loss values for the supplied weight data.
#' 
#' @examples 
#' data(demo_data)
#' 
#' wgts <- 
#'   iterake(
#'     universe = universe(
#'         data = demo_data,
#'         
#'         category(
#'             name = "Sex",
#'             buckets = factor(
#'                 x = levels(demo_data[["Sex"]]),
#'                 levels = levels(demo_data[["Sex"]])
#'             ),
#'             targets = c(0.4, 0.5),
#'             sum.1 = TRUE
#'         ),
#' 
#'         category(
#'             name = "BirthYear",
#'             buckets = c(1986:1990),
#'             targets = rep(0.2, times = 5)
#'         ),
#'     
#'         category(
#'             name = "EyeColor",
#'             buckets = c("brown", "green", "blue"),
#'             targets = c(0.8, 0.1, 0.1)
#'         ),
#'     
#'         category(
#'             name = "HomeOwner",
#'             buckets = c(TRUE, FALSE),
#'             targets = c(3/4, 1/4)
#'         )
#'     )
#'   )
#' 
#' weight_stats(wgts$weight)
#' 
#' @export
weight_stats <- function(weight) {
    
    # one col df's and numeric vectors are permissible
    if (!is.numeric(weight) & !is.data.frame(weight)) {
        
        stop("Input to `weight` must be a data frame with one column or a numeric vector.")
        
    }
    
    # # if one col df, turn into vector
    if (is.data.frame(weight)) {

        if (ncol(weight) == 1) {
            
            weight <- pull(weight)
            
            # after vector-ing, make sure numeric
            if (!is.numeric(weight)) {
                
                stop("Column in provided data frame must be numeric.")
                
            }

        } else {

            stop("Input to `weight` must be a data frame with one column or a numeric vector.")

        }

    }
    
    # calculate stats
    uwgt_n <- length(weight)
    wgt_n  <- sum(weight)
    eff_n  <- (sum(weight) ^ 2) / sum(weight ^ 2)
    loss   <- (uwgt_n / eff_n) - 1
    efficiency <- (eff_n / uwgt_n)
    min_wgt <- min(weight)
    max_wgt <- max(weight)
    
    # output
    tibble(
        uwgt_n = uwgt_n,
        wgt_n  = wgt_n,
        eff_n  = eff_n,
        loss   = loss,
        efficiency = efficiency,
        min_wgt = min(weight),
        max_wgt = max(weight)
    )
    
}
