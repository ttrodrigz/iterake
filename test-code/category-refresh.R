library(labelled)
library(tidyverse)
library(rlang)
library(collapse)
library(glue)
library(cli)

category <- function(name, buckets, targets) {
    
    # Check to make sure buckets and targets are the same length
    if (length(buckets) != length(targets)) {
        abort("The number of buckets must equal the number of targets.")
    }
    
    # Store as a list
    out <- list(list(
        buckets = buckets,
        targets = targets
    ))
    
    names(out) <- name
    
    # Check to make sure that the targets sum to 1
    sum.targets <- fsum(targets)
    sum1 <- sum.targets == 1
    
    if (!sum1) {
        warn(glue(
            "Weighting targets for '{name}' will be re-proportioned to sum to 1."
        ))
        targets.sum1 <- targets / sum.targets
        out[[1]]$targets_sum1 <- targets.sum1
    }
    
    # Are the buckets labelled?
    is.labelled <- labelled::is.labelled(buckets)
    
    if (is.labelled) {
        out[[1]]$labels <- names(labelled::val_labels(buckets))
    }
    
    class(out) <- c(class(out), "category")
    
    out
    
}

aa <- category(
    name = "color",
    buckets = c("blue", "red", "green"),
    targets = c(1, 2, 3)
)

bb <- category(
    name = "this_that",
    buckets = labelled_spss(
        x = 1:3,
        labels = c("One" = 1, "Two" = 2, "Three" = 3)
    ),
    targets = c(2.3, 1.1, 2.9)
)

#' @method print category
#' @export
print.category <- function(x, digits = 3) {
    
    # Was this weight category re-proportioned?
    was.reprop <- !is.null(x[[1]]$targets_sum1)
    
    # What's the weight category name?
    wgt.cat <- names(x)
    
    # What are the buckets and the targets?
    buckets <- x[[1]][["buckets"]]
    targets <- x[[1]][["targets"]]
    targets_sum1 <- x[[1]][["targets_sum1"]]
    
    # Header line
    cli::cat_rule(glue("Weighting Category: {wgt.cat}"))
    
    # Build the bullet point strings
    is.labelled <- labelled::is.labelled(buckets)
    if (is.labelled) {
        labels <- x[[1]]$labels
        buckets <- glue("{buckets} [{labels}]")
    }
    buckets <- paste0(buckets, ":")
    bucket.chr <- nchar(buckets)
    max.chr <- max(bucket.chr)
    
    buckets <- str_pad(buckets, width = max.chr, side = "right", pad = ' ')
    
    if (was.reprop) {
        cli::cat_line(cli::style_italic("Targets were re-proportioned to sum to 1."))
        values <- scales::number(targets_sum1, accuracy = 1/10^digits, trim = FALSE)
    } else {
        values <- scales::number(targets, accuracy = 1/10^digits, trim = FALSE)
    }
    
    cli::cat_line(glue("{buckets} {values}"))
    
}

print(aa, digits = 4)
