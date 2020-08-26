  
#' Calculate Decimal Growth Rate.
#' 
#' \code{growth} return the decimal growth rate of a numeric vector. It employs data.table:::shift() to achieve the best performance.
#'
#' @export
#' @param x A numeric vector.
#' @return The decimal growth rate of vector \code{x}. The first element will be NA.
#' @examples
#' growth(c(1, 2, 1))

growth <- function(x) {
    x / shift(x) -1
}