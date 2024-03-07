#' Weight summary statistics
#' 
#' @description
#' Returns a `tibble` of the unweighted, weighted, and effective sample sizes,
#' the loss and weighting efficiency, and the minimum and maximum weight values.
#' 
#' @param x An `iterake` object or a numeric vector.
#' 
#' @return A `tibble`.
#' 
#' @export
weight_stats <- function(x) {
    UseMethod("weight_stats", x)
}


#' Weight summary statistics for an `iterake` object
#' 
#' @param x Output of `iterake()`.
#' 
#' @export
weight_stats.iterake <- function(x) {
    
    x$summary
    
}

#' Weight summary statistics for a numeric vector
#' 
#' @param x A numeric vector.
#' 
#' @importFrom collapse fmin fmax
#' @importFrom tibble tibble
#' @importFrom rlang warn
#' 
#' @export
weight_stats.numeric <- function(x) {
    
    if (any(x < 0)) {
        warn("Input to `x` contains negative values, use with caution.")
    }
    
    tibble(
        uwgt_n = sample_size(x, x, type = "u"),
        wgt_n = sample_size(x, x, type = "w"),
        eff_n = sample_size(x, x, type = "e"),
        loss = uwgt_n / eff_n - 1,
        efficiency = weighting_efficiency(x),
        min = fmin(x),
        max = fmax(x)
    )
    
}
