
# Ideas -------------------------------------------------------------------

#' The universe() function will accept a data frame, dots for the weighting
#' categories, and a population size parameter to rescale the weights to
#' sum to the population size.

w <- c(0.75, 1, 1.25)

neff <- function(x) {
    (sum(x) ^ 2) / sum(x^2)
}

neff(w)

5 / sum(w) * w

rs <- function(x, sum_to) {
    sum_to / sum(x) * x
}

neff(rs(w, -1))
rs(w, -3)


