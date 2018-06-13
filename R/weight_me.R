#' Simulated data for weighting
#'
#' A dataset of 200 orders for specific ACME products - including seeds, transport, and costumes.
#' Also includes a satisfaction variable for grouping and a previous weight variable.
#'
#' @docType data
#'
#' @usage data(weight_me)
#'
#' @format A data frame with 200 rows and six columns:
#' \describe{
#'    \item{order}{unique identifier}
#'    \item{seeds}{type of seed ordered, character, three levels}
#'    \item{costume}{type of costume ordered, character, two levels}
#'    \item{transport}{type of transport ordered, character, three levels}
#'    \item{satisfied}{satisfied with order, character, two levels}
#'    \item{prev_weight}{previous weight variable, numeric}
#' }
#'
#' @examples
#' data(weight_me)
"weight_me"
