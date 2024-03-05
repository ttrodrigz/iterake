#' Type compatibility check
#' 
#' Internal function to verify match between category group vector type and 
#' data vector type.
#' 
#' @param x First vector of data to be typed and compared.
#' @param y Second vector of data to be typed and compared.
#' 
#' @return A \code{boolean} indicating whether the types match.
#' 
#' @examples 
#' compatible_types(
#'     x = mtcars$am,
#'     y = mtcars$vs
#' )
#' @noRd
compatible_types <- function(x, y) {
    
    if (is.numeric(x) & !is.numeric(y)) {
        return(FALSE)
    }
    
    if (is.character(x) & !is.character(y)) {
        return(FALSE)
    }
    
    if (is.factor(x) & !is.factor(y)) {
        return(FALSE)
    }
    
    if (is.logical(x) & !is.logical(y)) {
        return(FALSE)
    }
    
    TRUE
    
}

#' Sample size calculations.
#' 
#' @details
#' The unweighed sample size is the number of non-missing elements in `x`. The
#' weighted sample size is the sum of the weights for the complete cases in `x` 
#' and `w`. The effective sample size is \eqn{(sum(w)^2) / sum(w^2)}.
#' 
#' @importFrom collapse fsum 
#' @importFrom rlang abort
#' 
#' @examples
#' x <- 1:3
#' w <- c(0.75, 1.00, 1.25)
#' 
#' sample_size(x, w, type = "u")
#' sample_size(x, w, type = "w")
#' sample_size(x, w, type = "e")
#' sample_size(x, w, type = "all")
#' 
#' @param x A vector.
#' @param w A vector of weights.
#' @param type One of `"u"` for unweighted, `"w"` for weighted, `"e"` effective, 
#' or `"all"` for all three.
#' 
#' @return Either a single numeric value, or a named vector if `type = "all"`.
#' 
#' @export
sample_size <- function(x, w, type = "u") {
    
    if (type != "u" & missing(w)) {
        abort("Must provide an input to `w` if weighted or effective sample size is desired.")
    }
    
    
    if (type != "u") {
        valid <- complete.cases(cbind(x, w))
        w.valid <- w[valid]
        wss <- fsum(w.valid)
        ess <- (fsum(w.valid) ^ 2) / fsum(w.valid ^ 2)
        
    }
    
    uss <- fsum(!is.na(x))    
    
    if (type == "u") {
        return(uss)
    } else if (type == "w") {
        return(wss)
    } else if (type == "e") {
        return(ess)
    } else if (type == "all") {
        return(
            c("unweighted" = uss, "weighted" = wss, "effective" = ess)
        )
    }
    
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
    sample_size(w, w, type = "e") / sample_size(w, type = "u")
}


#' Parameter setting function for \code{iterake()}.
#' 
#' @param threshold Value specifying minimum summed difference between weighted marginal 
#' proportions of sample and universe before algorithm quits, default is 1e-10.
#' @param max_weight Maximum value weights can take on, default is 3. The capping 
#' takes place prior to applying expansion factor (if \code{pop_size} is set in \code{universe()}.
#' @param max_iter Value capping number of iterations for the procedure, default is 50.
#' @param max_stuck Value capping the number of times summed differences between sample 
#' and universe can oscillate between increasing and decreasing, default is 5.
#' 
#' @importFrom rlang abort
#' 
#' @return A \code{list} with special class \code{control}.
#' 
#' @export
control_iterake <- function(
        threshold = 1e-10,
        max_weight = 3,
        max_iter = 50,
        max_stuck = 5
) {
    
    # threshold
    if (any(
        length(threshold) > 1,
        !is.numeric(threshold),
        threshold < 0
    )) {
        
        abort("Input to `threshold` must be a single positive numeric value.")
    }
    
    # max_weight
    if (any(
        length(max_weight) > 1,
        !is.numeric(max_weight),
        max_weight <= 1
    )) {
        
        abort("Input to `max_weight` must be a single numeric value greater than 1.")
        
    }
    
    # max_iter
    if (any(
        length(max_iter) > 1,
        !is.numeric(max_iter),
        max_iter <= 0
    )) {
        
        abort("Input to `max_iter` must be a single numeric value greater than 0.")
        
    }
    
    # max_stuck
    if (any(
        length(max_stuck) > 1,
        !is.numeric(max_stuck),
        max_stuck < 1
    )) {
        
        abort("Input to `max_stuck` must be a single numeric value greater than or equal to 1.")
        
    }
    
    out <- list(
        threshold = threshold,
        max_weight = max_weight,
        max_iter = max_iter,
        max_stuck = max_stuck
    )
    
    class(out) <- c(class(out), "control")
    
    out
}
