#' Type compatability check
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
