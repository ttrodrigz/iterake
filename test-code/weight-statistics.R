#' Unweighted sample size.
#' 
#' @param x A vector.
#' 
#' @importFrom collapse fsum
#' 
#' @examples
#' unweighted_ss(
#'     x = runif(3),
#' )
#' 
#' @export
unweighted_ss <- function(x) {
    
    fsum(!is.na(x))
    
}

#' Weighted sample size.
#' 
#' @param x A vector.
#' @param w A vector of weights.
#' 
#' @importFrom collapse fsum
#' 
#' @examples
#' weighted_ss(
#'     x = runif(3),
#'     w = c(0.75, 1.00, 1.25)
#' )
#' 
#' @export
weighted_ss <- function(x, w) {

    valid <- complete.cases(cbind(x, w))
    
    fsum(w[valid])
    
}

#' Effective sample size.
#' 
#' @param x A vector.
#' @param w A vector of weights.
#' 
#' @importFrom collapse fsum
#' 
#' @examples
#' effective_ss(
#'     x = runif(3),
#'     w = c(0.75, 1.00, 1.25)
#' )
#' 
#' @export
effective_ss <- function(x, w) {
    
    valid <- complete.cases(cbind(x, w))
    
    x <- x[valid]
    w <- w[valid]
    
    (sum(w) ^ 2) / sum(w ^ 2)
    
}


#' Weighting efficiency.
#' 
#' @param w A vector of weights.
#' 
#' @importFrom collapse fsum
#' 
#' @examples
#' weighting_efficiency(c(0.75, 1.00, 1.25))
#' 
#' @export
weighting_efficiency <- function(w) {
    effective_ss(w, w) / unweighted_ss(w)
}
