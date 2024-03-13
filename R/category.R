#' Create a weighting category.
#' 
#' @description
#' This function creates a new weighting category.
#' 
#' @param name The name of the weighting category matching the name of the
#' corresponding variable in the data to be weighted.
#' @param groups A vector of the groups in the weighting category.
#' @param targets A vector of the desired proportions of the `groups`.
#' These target will be automatically re-proportioned to sum to 1 if
#' necessary.
#' 
#' @importFrom labelled is.labelled val_labels
#' @importFrom rlang warn abort
#' 
#' @return A `list` with the class of `"category"`.
#' 
#' @examples
#' category(
#'     name = "vs",
#'     groups = c(0, 1),
#'     targets = c(1/3, 2/3)
#' )
#' 
#' @export
category <- function(name, groups, targets) {
    
    # Check to make sure groups and targets are the same length
    if (length(groups) != length(targets)) {
        abort("The number of groups must equal the number of targets.")
    }
    
    # Store as a list
    out <- list(list(
        groups = groups,
        targets = targets
    ))
    
    names(out) <- name
    
    # Check to make sure that the targets sum to 1
    sum.targets <- sum(targets, na.rm = TRUE)
    sum1 <- sum.targets == 1
    
    if (!sum1) {
        warn(glue(
            "Weighting targets for '{name}' will be re-proportioned to sum to 1."
        ))
        targets.sum1 <- targets / sum.targets
        out[[1]]$targets_sum1 <- targets.sum1
    }
    
    # Are the groups labelled?
    is.labelled <- is.labelled(groups)
    
    if (is.labelled) {
        out[[1]]$labels <- names(val_labels(groups))
    }
    
    class(out) <- c(class(out), "category")
    
    out
    
}

#' Print method for category objects.
#' 
#' @method print category
#' 
#' @param x A `category` object.
#' @param digits Number of digits for printing the target proportions, default is 3.
#' @param ... Not currently used.
#' 
#' @importFrom glue glue
#' @importFrom cli cat_rule cat_line style_italic
#' @importFrom labelled is.labelled
#' @importFrom stringr str_pad
#' @importFrom scales number
#' 
#' @export
print.category <- function(x, digits = 3, ...) {
    
    # Was this weight category re-proportioned?
    was.reprop <- !is.null(x[[1]]$targets_sum1)
    
    # What's the weight category name?
    wgt.cat <- names(x)
    
    # What are the groups and the targets?
    groups       <- x[[1]][["groups"]]
    targets      <- x[[1]][["targets"]]
    targets_sum1 <- x[[1]][["targets_sum1"]]
    
    # Header line
    cat_rule(glue("Weighting Category: {wgt.cat}"))
    
    # Build the bullet point strings
    is.labelled <- is.labelled(groups)
    if (is.labelled) {
        labels <- x[[1]]$labels
        groups <- glue("{groups} [{labels}]")
    }
    groups    <- paste0(groups, ":")
    group.chr <- nchar(groups)
    max.chr   <- max(group.chr)
    
    groups <- str_pad(groups, width = max.chr, side = "right", pad = ' ')
    
    if (was.reprop) {
        cat_line(style_italic("Targets were re-proportioned to sum to 1."))
        values <- number(targets_sum1, accuracy = 1/10^digits, trim = FALSE)
    } else {
        values <- number(targets, accuracy = 1/10^digits, trim = FALSE)
    }
    
    cat_line(glue("{groups} {values}"))
    
}
