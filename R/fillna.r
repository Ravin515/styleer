#' Fill NAs with user-specified value
#'
#' @param x A numeric vector
#' @param na.value Value for NAs
#' @param inf.value Value for Inf or -Inf
#' @export

fillna <- function(x, na.value = 0, fill.inf = F, inf.value = 0){
    x[which(is.na(x))] <- na.value
    if (fill.inf == T) {
        x[which(is.infinite(x))] <- inf.value
    }
    x
}